rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, sf
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)
setwd("C:/Users/iddo2/Documents/thesis data")

fires <- read_sf("fires with admin units test/Firms_Thailand_Points_LandUse.shp") 
boundaries <- read_sf("thai forest control units/408_Polygon_join.shp")
admin_units <- read_sf("thai admin units/admin3_simplified_en.shp")

lampang <- "ลำปาง"
lamphun <- "ลำพูน"
tak <- "ตาก"
chiangrai <- "เชียงราย"
chiangmai <- "เชียงใหม่"
maehongson <- "แม่ฮ่องสอน"
nan <- "น่าน"
phrae <- "แพร่"
phayao <- "พะเยา"
northern_regions <- c(
  lampang, lamphun, tak, chiangrai, chiangmai, 
  nan, maehongson, phrae, phayao
  )
northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

boundaries_filtered <- boundaries %>% 
  mutate(Province = sub("^จ\\.", "", PROV_NAM_T)) %>% 
  filter(Province %in% northern_regions) %>% 
  select(Province, Unit_Name)

admin_units_north <- admin_units %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  st_transform(st_crs(boundaries_filtered))

fires_filtered <- fires %>% 
  mutate(
    id = row_number()
  ) %>% 
  st_transform(st_crs(admin_units_north)) %>% 
  st_intersection(admin_units_north) %>% # makes me lose about 35 fires 
  st_intersection(boundaries_filtered) # makes me lose about 300 fires?

fires_filtered <- fires_filtered %>% 
  mutate(
    land_use_cat = case_when(
      land_use <= 5 ~ "forests",
      land_use >= 6 & land_use <=7 ~ "shrubs",
      land_use >= 8 & land_use <=9 ~ "savannah",
      land_use == 10 ~ "grasslands",
      land_use == 12 | land_use == 14 ~ "crops",
      land_use == 13 ~ "urban",
      TRUE ~ NA
    ),
    year = year(date),
    month = month(date),
    month_cat = ifelse(month >= 5, "may to december", month)
  )

# there are tiny differences between Province and ADM_1_EN (around borders?)
# they're quite marginal so ignoring them for now 
# (for exmaple - out of 6365 with ADM1_EN == "Chiang Mai", 99.4% have the 'correct' Province)
ggplot(fires_filtered) +
  geom_sf(data = boundaries_filtered, color = "black") +
  geom_sf(data = admin_units_north, color = "darkred") +
  geom_sf(aes(color = month_cat), size = 0.5) +
  scale_color_viridis_d() +
  theme_minimal() 
  #theme(legend.position="none")

### control units ----
unit47 <- read_sf("thai forest control units/Unit_Zone47_Point_Edit.shp") 
unit48 <- read_sf("thai forest control units/Unit_Zone48_Point_Edit.shp")
# unit48 are units that are located in the east of thailand, so use UTM zone 48N
unit47 <- unit47 %>%
  rename(
    No_ = No,
    Province = Provine,
    E = X,
    N = Y
  )
unit48 <- unit48 %>% st_transform(st_crs(unit47)) 
# converting all to EPSG:32647 (differences are marginal probably - maybe test)
units <- bind_rows(unit47, unit48) %>% 
  rename(
    District = Amphoe,
    Subdistrict = Tambon
  ) %>% 
  select(Province, District, Subdistrict, Unit_Name)
rm(unit47, unit48)

matched_fires <- fires_filtered %>% 
  # match between fire and its responsible unit and calculate distance
  rowwise() %>%
  mutate(
    matching_units = list(units[units$Unit_Name == .data$Unit_Name,]),
    Matching_Unit_Name = if(nrow(matching_units) > 0) {
      matching_units$Unit_Name[1]
    } else {
      NA_character_
    },
    Distance = if(nrow(matching_units) > 0) {
      st_distance(geometry, matching_units$geometry, by_element = TRUE)
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

units_nogeom <- units %>% st_set_geometry(NULL) %>% select(Unit_Name)
matched_fires_df <- matched_fires %>% # 
  mutate(distance_km = as.numeric(Distance)/1000) %>% # convert to numeric km
  left_join(units_nogeom, by = "Unit_Name") %>% 
  select(-Distance, -Matching_Unit_Name, -matching_units, -Province)

rm(units_nogeom, matched_fires, admin_units_north, boundaries_filtered, 
   boundaries, admin_units, fires, units)

### bans -----
bans <- fread("0211 data/Northern Thailand Crop Burning Ban.csv")
bans <- bans %>% 
  select(Region, `Ban start`, `Ban end`) %>% 
  rename(ADM1_EN = Region,
         Ban_start = `Ban start`, 
         Ban_end = `Ban end`)
bans[, Ban_start := as.Date(Ban_start, format = "%Y-%m-%d")]
bans[, Ban_end := as.Date(Ban_end, format = "%Y-%m-%d")]

matched_fires_dt <- matched_fires_df %>% 
  as.data.table() 
matched_fires_dt[, is_ban := 0]  # Initialize is_ban
matched_fires_dt <- matched_fires_dt %>% mutate(date = as.Date(date))
matched_fires_dt[bans, on = .(ADM1_EN = ADM1_EN, date >= Ban_start, date <= Ban_end), 
      is_ban := 1]

matched_fires_dt_relevant <- matched_fires_dt %>% 
  select(-geometry) %>% # disappeared in the conversion to dt anyway
  mutate(year = year(date)) %>% 
  filter(
    year >= 2016 & year <= 2020 & (date < '2020-06-30')
    )

ggplot(matched_fires_dt_relevant) +
  geom_line(aes(x = date, y = is_ban, color = ADM1_EN)) +
  theme_minimal()

matched_fires_dt_relevant_exp <- matched_fires_dt_relevant[bans,                                                            on = .(ADM1_EN =ADM1_EN), 
                                                           allow.cartesian = TRUE]
matched_fires_dt_relevant_exp[, `:=`(
  days_to_ban_start = as.integer(date - Ban_start),
  days_to_ban_end = as.integer(date - Ban_end)
)]

# Step 3: Find the closest Ban start and Ban end for each firm date
# We take the minimum absolute days difference for each date and region combination
fires_closest_bans <- matched_fires_dt_relevant_exp[
  , .(
    days_to_closest_ban_start = days_to_ban_start[which.min(abs(days_to_ban_start))],
    days_to_closest_ban_end = days_to_ban_end[which.min(abs(days_to_ban_end))]
  ),
  by = .(ADM1_EN, date)
]

# Step 4: Merge the closest ban information back to the original firms table
fires <- merge(matched_fires_dt_relevant_exp, fires_closest_bans, by = c("ADM1_EN", "date"), all.x = TRUE)

fires_filtered <- fires %>% 
  mutate(year_ban = year(Ban_start),
         same_year = ifelse(year_ban == year, 1, 0)) %>% # won't work well if bans begin before 1 of february
  filter(same_year == 1) %>% 
  select(-same_year, -Ban_start, -Ban_end, -year_ban, -month_cat, -ADM0_EN) %>% 
  mutate(
    distance_group = case_when(
      distance_km < 5 ~ "<5",
      distance_km >= 10 & distance_km < 20 ~ "10-20",
      distance_km >= 20 ~ ">20",
      TRUE ~ NA
    )
  )

grouped_adm3 <- fires_filtered %>% 
  group_by(
    date, ADM1_EN, ADM2_EN, ADM3_EN, land_use_cat  # Group by all relevant variables
  ) %>% 
  summarise(
    fire_count = n(),  # Count fires for each land_use_cat within the groups
    .groups = 'drop'  # Optional: Drop the grouping structure after summarising
  ) %>% 
  pivot_wider(
    names_from = land_use_cat,  # Create columns from land_use_cat
    values_from = fire_count,   # Fill columns with fire counts
    values_fill = list(fire_count = 0)  # Replace NA with 0 for land_use_cat with no fires
  ) %>% 
  select(-'NA', -shrubs, -urban)

admin_info <- grouped_adm3 %>%
  ungroup() %>%
  select(ADM1_EN, ADM2_EN, ADM3_EN) %>%
  distinct()

dates <- c(seq(as.Date("2016-01-01"), as.Date("2016-06-30"), by="day"),
           seq(as.Date("2017-01-01"), as.Date("2017-06-30"), by="day"),
           seq(as.Date("2018-01-01"), as.Date("2018-06-30"), by="day"),
           seq(as.Date("2019-01-01"), as.Date("2019-06-30"), by="day"),
           seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by="day"))

ban_status <- expand.grid(
  date = dates,
  ADM1_EN = unique(bans$ADM1_EN),
  stringsAsFactors = FALSE
  ) %>%
  as_tibble() %>%
  mutate(year = lubridate::year(date)) %>%
  left_join(bans, 
            by = "ADM1_EN",
            relationship = "many-to-many") %>%
  mutate(
    is_ban = !is.na(Ban_start) & !is.na(Ban_end) &
      year == lubridate::year(Ban_start) &
      date >= Ban_start & date <= Ban_end
  ) %>%
  group_by(date, ADM1_EN) %>%
  summarize(is_ban = any(is_ban), .groups = "drop") %>% 
  mutate(
    is_ban = ifelse(is_ban == TRUE, 1, 0)
  )

ban_status_18 <- ban_status %>%
  mutate(
    date = as.Date(date, tz = "Asia/Bangkok"),
    year = year(date),
    month = month(date, label = TRUE, abbr = F, locale = 'en_US.UTF-8'),
    wday = wday(date, label = TRUE, abbr = FALSE, locale = 'en_US.UTF-8', week_start = 1),  # Monday start
    week = stringi::stri_datetime_fields(date)$WeekOfMonth
  ) %>% 
  filter(year == 2018) %>% 
  group_by(date, month, wday, week) %>%
  summarise(regions_inban = sum(is_ban), .groups = "drop")

ggplot(ban_status_18, aes(x = wday, y = 5 - week, fill = regions_inban)) +  
  geom_tile(color = "white") +
  facet_wrap(~ month, ncol = 3) +
  scale_fill_gradientn(
    colors = c("#ccece6", "#66c2a4", "#238b45", "#024d38"),  # More distinct shades
    breaks = seq(0, 8, 1),  # Ensure only whole numbers in legend
    name = "Regions in Ban"  # Rename legend
  ) +
  labs(title = "Ban Status by Day (2018)") +
  theme_minimal() +
  coord_equal(expand = FALSE) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Center & enlarge title
  )

admin_units_north_1 <- read_sf("thai admin units/tha_admbnda_adm1_rtsd_20220121.shp") %>% 
  select(ADM1_EN) %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  st_transform(st_crs(boundaries_filtered))

bans_map <- admin_units_north_1 %>% 
  left_join(ban_status, by = "ADM1_EN")

bans_specific_date <- bans_map %>% 
  filter(
    date %in% c("2018-02-14","2018-02-15","2018-02-16")
  )

ggplot(bans_specific_date) +
  geom_sf(aes(fill = factor(is_ban))) +  # Convert is_ban to factor
  facet_wrap(~ date) +  # Create small maps for each date
  scale_fill_manual(
    values = c("0" = "grey", "1" = "darkred"), 
    name = "Ban Status",
    labels = c("No Ban", "Ban")  # Custom labels
  ) +
  labs(title = "Bans Status Over A Few Days") +  # Add title
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 15),  
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Center & enlarge title
        )  

grid_thai <- admin_info %>%
  crossing(date = dates) %>% 
  left_join(ban_status, by = c("date", "ADM1_EN")) %>% 
  left_join(
    grouped_adm3,
    by = c("date", "ADM1_EN", "ADM2_EN", "ADM3_EN")
  ) %>%
  # Replace NA values with 0 for fire_count and FALSE for is_ban
  mutate(
    savannah = replace_na(savannah, 0),
    grasslands = replace_na(grasslands, 0),
    crops = replace_na(crops, 0),
    forests = replace_na(forests, 0),
    fire_count_all = savannah+grasslands+crops+forests,
    fire_incidence = ifelse(fire_count_all > 0, 1, 0),
    month = month(date),
    year = year(date),
    month_year = paste(month,'_',year)
  ) 

print(coeftest(feols(
  grasslands
  ~ is_ban | ADM3_EN + date, 
  data = grid_thai, 
  vcov = cluster ~ ADM3_EN)
))

fires_filtered_start <- fires_filtered %>% 
  filter(days_to_closest_ban_start > -30, days_to_closest_ban_start < 30) 

fires_filtered_end <- fires %>% 
  filter(days_to_closest_ban_end > -30, days_to_closest_ban_end < 30) 

grouped_adm3_start <- fires_filtered_start %>% 
  group_by(days_to_closest_ban_start, land_use_cat) %>% 
  summarise(
    fire_count = n()
  ) 

grouped_adm3_end <- fires_filtered_end %>% 
  group_by(days_to_closest_ban_end, land_use_cat) %>% 
  summarise(
    fire_count = n()
  ) 

ggplot(grouped_adm3_start, 
       aes(x = days_to_closest_ban_start, y = fire_count)) +
  geom_line() +  
  labs(
    title = "Fire Counts Over Time",
    x = "Date compared to end",
    y = "Fire Count"
  )
  theme_minimal()

ggplot(grouped_adm3_start, aes(x = days_to_closest_ban_start, y = fire_count)) +
    geom_line(alpha = 0.3) +  # Keep the original line with transparency
    geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
    theme_minimal()
  
ggplot(grouped_adm3_end, aes(x = days_to_closest_ban_end, y = fire_count)) +
    geom_line(alpha = 0.3) +  # Keep the original line with transparency
    geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
    theme_minimal()
  
ggplot(grouped_adm3_end, 
       aes(x = days_to_closest_ban_end, y = fire_count)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
  facet_wrap(~land_use_cat, scales = "free_y") +  # Creates separate panels for each ADM1_EN
  labs(
    title = "Fire Counts Over Time by Region",
    x = "Days Relative to Ban Start",
    y = "Fire Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),  # Smaller facet labels
    axis.text = element_text(size = 7),   # Smaller axis text
    plot.title = element_text(size = 12)  # Smaller title
  )
