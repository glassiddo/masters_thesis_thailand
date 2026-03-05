rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest # twfe + sun and abraham event study
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data")

### some boring stuff ----
northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

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

dates <- c(seq(as.Date("2016-01-01"), as.Date("2016-06-30"), by="day"),
           seq(as.Date("2017-01-01"), as.Date("2017-06-30"), by="day"),
           seq(as.Date("2018-01-01"), as.Date("2018-06-30"), by="day"),
           seq(as.Date("2019-01-01"), as.Date("2019-06-30"), by="day"),
           seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by="day"))

admin_units <- read_sf("thai admin units/admin3_simplified_en.shp")
admin_units_north <- admin_units %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  mutate(
    id = row_number()
  )

admin_units_north_nogeom <- admin_units_north %>% 
  st_set_geometry(NULL)

bans <- fread("data/0211 data/Northern Thailand Crop Burning Ban.csv") %>%
  select(Region, `Ban start`, `Ban end`) %>%
  rename(ADM1_EN = Region,
         Ban_start = `Ban start`, 
         Ban_end = `Ban end`) %>%
  mutate(Ban_start = as.Date(Ban_start, format = "%Y-%m-%d"),
         Ban_end = as.Date(Ban_end, format = "%Y-%m-%d"),
         year = lubridate::year(Ban_start)) %>% 
  filter(!is.na(Ban_start))

## grid ----
grid <- expand.grid(
  date = dates,
  id = unique(admin_units_north$id),
  stringsAsFactors = FALSE
  ) %>%
  left_join(admin_units_north_nogeom, 
            by = "id") %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(bans, 
            by = c("ADM1_EN", "year")) %>% 
  mutate(
    days_from_ban_start = as.numeric(date - Ban_start),    
    days_from_ban_end = as.numeric(date - Ban_end),
    is_ban = ifelse(
      (!is.na(Ban_start) & !is.na(Ban_end) &
         year == lubridate::year(Ban_start) &
         date >= Ban_start & date <= Ban_end
      ), 1, 0
    ),
    is_ban_over = ifelse(is_ban == 0 & days_from_ban_end > 0, 
                         1, 0) # for event study of ban being over
  )

### burned data cleaning ----
read_burned_data <- function(year){
  read_sf(paste0("burned data 1102/Burned_Area_LandUse_Thailand_", year, ".shp"))
}
burned_data <- setNames(
  lapply(2016:2020, read_burned_data),
  paste0("bd", substr(2016:2020, 3, 4))
)

clean_bd <- function(bd, year){
  bd <- bd %>% 
    rename(
      land_use = label, 
      date = first
      ) %>% 
    mutate(
      actual_date = as.Date(date - 1, origin = paste0(year, "-01-01")),
      land_use_categories = case_when(
        land_use %in% c(10, 20, 30, 40) ~ "Cropland",  # Group all cropland-related values
        land_use %in% c(50, 60, 70, 80, 90) ~ "Forest",  # Group all forest-related values
        land_use == 100 ~ "Mosaic Tree/Shrub",  # Specific mosaic category
        land_use %in% c(120, 130) ~ "Shrubland/Grassland",  # Shrubland and grassland
        TRUE ~ "Other"  # Catch-all for any unexpected values or values with very few observations
      ),
      month = lubridate::month(actual_date)
    ) %>% 
    st_join(admin_units_north, join = st_within) %>% 
    #st_intersection(admin_units_north) %>% 
    filter(
      !is.na(ADM1_EN), # ignore fires outside of the north
      month < 7 # only look at january - july
      ) 
  return(bd)
}

burned_data_cleaned <- lapply(burned_data, clean_bd)

list2env(burned_data_cleaned, .GlobalEnv)

test_count <- bd18 %>%
  st_set_geometry(NULL) %>%
  filter(land_use_categories != "Other") %>%
  group_by(actual_date, land_use_categories) %>%
  summarise(N = n())

ggplot(test_count, aes(x = actual_date, y = N)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.4, se = FALSE, color = "blue") +
  facet_wrap(~land_use_categories, scales = "free_y") +
  theme_bw()

rm(test_count, admin_units, admin_units_north_nogeom,
   burned_data, bd16, bd17, bd18, bd19) # keep one to observe

### fires by land cleaning -----
fires <- read_sf("fires with admin units test/Firms_Thailand_Points_LandUse.shp") %>% 
  mutate(
    fire_id = row_number()
  ) %>% 
  st_transform(st_crs(admin_units_north)) %>% 
  st_join(admin_units_north, join = st_within) %>% 
  #st_intersection(admin_units_north) %>% # makes me lose about 35 fires
  #st_intersection(boundaries_filtered) # makes me lose about 300 fires?
  mutate(
    land_use_cat = case_when(
      land_use <= 5 ~ "forests",
      land_use >= 6 & land_use <=7 ~ "shrubs",
      land_use >= 8 & land_use <=9 ~ "savannah",
      land_use == 10 ~ "grasslands",
      land_use == 12 | land_use == 14 ~ "crops",
      TRUE ~ "Other"
    ),
    year = year(date)
  )

fires_for_join <- fires %>%
  st_set_geometry(NULL) %>% 
  mutate(date = as.Date(date)) %>% 
  select(fire_id, date, id, land_use_cat) %>% 
  group_by(date, id) %>% 
  summarise(
    N_fires = n(),
    N_fires_crops = sum(land_use_cat == "crops"),
    N_fires_shrubs = sum(land_use_cat == "shrubs"),
    N_fires_savannah = sum(land_use_cat == "savannah"),
    N_fires_forests = sum(land_use_cat == "forests"),
    N_fires_grasslands = sum(land_use_cat == "grasslands"),
    N_fires_other = sum(land_use_cat == "Other")
  )

### merge fires with grid -----
grid <- grid %>% 
  #filter(!is.na(Ban_start)) %>% 
  left_join(
    fires_for_join, by = c("date", "id")
  ) %>% 
  mutate(
    N_fires = replace_na(N_fires, 0),
    N_fires_crops = replace_na(N_fires_crops, 0),
    N_fires_shrubs = replace_na(N_fires_shrubs, 0),
    N_fires_savannah = replace_na(N_fires_savannah, 0),
    N_fires_forests = replace_na(N_fires_forests, 0),
    N_fires_grasslands = replace_na(N_fires_grasslands, 0),
    N_fires_other = replace_na(N_fires_other, 0)
  )

### viirs modis fires and merge ----
process_fires <- function(data_path) {
  read_sf(data_path) %>% 
    select(-c("INSTRUMENT", "SATELLITE", "VERSION")) %>% 
    filter(ACQ_DATE > "2016-01-01" & ACQ_DATE < "2020-12-31") %>% 
    filter(TYPE != 3) %>% 
    st_join(admin_units_north, join = st_within) %>% 
    filter(!is.na(id)) %>% 
    mutate(
      fire_id = row_number(),
      ACQ_HOUR = as.numeric(substr(ACQ_TIME, 1, 2)),  
      Time_Interval = case_when(
        ACQ_HOUR < 6  ~ "Before 6",
        ACQ_HOUR < 7 ~ "6-7",
        ACQ_HOUR < 8 ~ "7-8",
        ACQ_HOUR < 18 ~ "08-18",
        ACQ_HOUR < 19 ~ "18-19",
        ACQ_HOUR < 20 ~ "19-20",
        ACQ_HOUR < 24 ~ "After 20",
        TRUE          ~ "unknown"
      )
    )
}

select <- dplyr::select
modis <- process_fires("data/firms from nasa/fire_archive_M-C61_579607.shp") 
viirs <- process_fires("firms from nasa/fire_archive_SV-C2_578896.shp") 

summarize_fires <- function(data) {
  data %>%
    st_set_geometry(NULL) %>%
    rename(date = ACQ_DATE) %>%
    select(fire_id, date, id, DAYNIGHT, Time_Interval) %>%
    group_by(date, id) %>%
    summarise(
      N_fires = n(),
      N_fires_day = sum(DAYNIGHT == "D"),
      N_fires_night = sum(DAYNIGHT == "N"),
      N_fires_before_6 = sum(Time_Interval == "Before 6"),
      N_fires_6_7 = sum(Time_Interval == "6-7"),
      N_fires_7_8 = sum(Time_Interval == "7-8"),
      N_fires_8_18 = sum(Time_Interval == "8-18"),
      N_fires_18_19 = sum(Time_Interval == "18-19"),
      N_fires_19_20 = sum(Time_Interval == "19-20"),
      N_fires_after20 = sum(Time_Interval == "After 20"),
      .groups = "drop"
    )
}

viirs_summary <- summarize_fires(viirs) %>%
  rename_with(~paste0(.x, "_viirs"), starts_with("N_fires"))

modis_summary <- summarize_fires(modis) %>%
  rename_with(~paste0(.x, "_modis"), starts_with("N_fires"))

grid <- grid %>%
  left_join(viirs_summary, by = c("date", "id")) %>%
  left_join(modis_summary, by = c("date", "id")) %>% 
  mutate(across(starts_with("N_fires"), ~replace_na(.x, 0)))

### merge burns with grid -----
group_data <- function(bd){
  bd <- bd %>% 
    st_set_geometry(NULL) %>% 
    mutate(burnt_id = row_number()) %>% 
    select(burnt_id, actual_date, land_use_categories, id) %>% 
    rename(date=actual_date, land_use = land_use_categories) %>% 
    group_by(date, id) %>% 
    summarise(
      N_burnt = n(),
      N_burnt_Cropland = sum(land_use == "Cropland"),
      N_burnt_Forest = sum(land_use == "Forest"),
      N_burnt_Mosaic_TreeShrub = sum(land_use == "Mosaic Tree/Shrub"),
      N_burnt_Mosaic_ShrublandGrassland = sum(land_use == "Shrubland/Grassland"),
      N_burnt_Other = sum(land_use == "Other")
    )
}

burned_data_grouped <- lapply(burned_data_cleaned, group_data)
burned_data_merged <- bind_rows(burned_data_grouped)

grid <- grid %>% 
  left_join(
    burned_data_merged, by = c("date", "id")
  ) %>% 
  mutate(
    N_burnt = replace_na(N_burnt, 0),
    N_burnt_Cropland = replace_na(N_burnt_Cropland, 0),
    N_burnt_Forest = replace_na(N_burnt_Forest, 0),
    N_burnt_Mosaic_TreeShrub = replace_na(N_burnt_Mosaic_TreeShrub, 0),
    N_burnt_Mosaic_ShrublandGrassland = replace_na(N_burnt_Mosaic_ShrublandGrassland, 0),
    N_burnt_Other = replace_na(N_burnt_Other, 0)
  ) %>% 
  group_by(id) %>% 
  arrange(date) %>%
  mutate(
    N_burnt_lead1 = Hmisc::Lag(N_burnt, -1),
    N_burnt_lead2 = Hmisc::Lag(N_burnt, -2),
    N_burnt_lead3 = Hmisc::Lag(N_burnt, -3)
  ) %>%
  ungroup()

test <- grid %>% 
  filter(id == 200) %>% 
  select(N_burnt, N_burnt_lead1, date) %>% 
  filter(date %in% c("2018-03-07", "2018-03-08", "2018-03-09"))
print(test) # works well?
rm(test)


### admin units ----
boundaries <- read_sf("thai forest control units/408_Polygon_join.shp") %>% 
  mutate(ADM1_EN = sub("^จ\\.", "", PROV_NAM_T)) %>% 
  filter(ADM1_EN %in% northern_regions) %>% 
  select(ADM1_EN, Unit_Name) %>% 
  st_transform(st_crs(fires))

unit47 <- read_sf("thai forest control units/Unit_Zone47_Point_Edit.shp") 
unit48 <- read_sf("thai forest control units/Unit_Zone48_Point_Edit.shp")
# unit48 are units that are located in the east of thailand, so use UTM zone 48N
unit47 <- unit47 %>%
  rename(
    No_ = No,
    ADM1_EN = Provine,
    E = X,
    N = Y
  )
unit48 <- unit48 %>% st_transform(st_crs(unit47)) 
# converting all to EPSG:32647 (differences are marginal probably - maybe test)
units <- bind_rows(unit47, unit48) %>% 
  rename(
    ADM2_EN = Amphoe,
    ADM3_EN = Tambon
  ) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, Unit_Name) %>% 
  st_transform(st_crs(fires))
rm(unit47, unit48)

matched_fires <- fires %>% 
  st_intersection(boundaries) %>% # makes me lose about 300 fires?
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

matched_fires_flt <- matched_fires %>% # 
  mutate(distance_km = as.numeric(Distance)/1000) %>% # convert to numeric km
  left_join(units %>% st_set_geometry(NULL) %>% select(-ADM1_EN, -ADM2_EN, -ADM3_EN), by = "Unit_Name") %>% 
  select(-matching_units, -Distance)

rm(matched_fires, viirs, modis, modis_summary, viirs_summary, fires_for_join, burned_data,
   burned_data_merged, burned_data_cleaned, burned_data_grouped, bd20, fires)
rm(fires_with_units_for_join, matched_fires_flt)

matched_fires_flt <- matched_fires_flt %>%
  group_by(ADM1_EN) %>%
  mutate(
    quantile_breaks = list(quantile(distance_km)),  # Calculate quantiles for each group
    distance_group = cut(distance_km, breaks = quantile_breaks[[1]], include.lowest = TRUE, labels = FALSE)  # Assign quantile groups
  ) %>%
  ungroup()  # Ungroup to avoid unexpected behavior in downstream operations

fires_with_units_for_join <- matched_fires_flt %>% 
  st_set_geometry(NULL) %>% 
  mutate(date = as.Date(date)) %>% 
  select(fire_id, date, id, land_use_cat, distance_group) %>% 
  group_by(date, id, distance_group) %>% 
  summarise(
    N_fires = n(),
    N_fires_crops = sum(land_use_cat == "crops"),
    N_fires_shrubs = sum(land_use_cat == "shrubs"),
    N_fires_savannah = sum(land_use_cat == "savannah"),
    N_fires_forests = sum(land_use_cat == "forests"),
    N_fires_grasslands = sum(land_use_cat == "grasslands"),
    N_fires_other = sum(land_use_cat == "Other")
  ) %>% 
  pivot_wider(
    names_from = distance_group, 
    values_from = c(
      N_fires, N_fires_crops, N_fires_forests, N_fires_shrubs, N_fires_savannah, N_fires_grasslands, N_fires_other
      ),
    names_glue = "{.value}_distance_group_{distance_group}"
  )

grid_with_units <- grid %>% 
  select(id, date) %>% 
  left_join(
    fires_with_units_for_join, by = c("date", "id")
  ) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

grid <- grid %>% 
  left_join(grid_with_units, by = c("date", "id"))

### empirical -----
grid <- read_csv("grid_1702.csv")

grid <- grid %>% 
  mutate(
    binary_fire = ifelse(N_fires > 0, 1, 0)
  )

print(coeftest(feols(
  N_burnt_lead1
  #N_fires
  #binary_fire
  ~ is_ban | ADM3_EN + date, 
  data = grid, 
  vcov = cluster ~ ADM2_EN)
))
gc()

grouped_for_es <- grid %>%
  group_by(days_from_ban_start, days_from_ban_end, ADM1_EN, is_ban, is_ban_over) %>%
  summarise(
    across(starts_with("N_burnt"), sum, .names = "SUM_{col}"),  # Sum all columns starting with "N_burnt"
    across(starts_with("N_fires"), sum, .names = "SUM_{col}"),  # Sum all columns starting with "N_fires"
    across(starts_with("N_fires_"), sum, .names = "SUM_{col}")  # Sum all columns starting with "N_fires_"
  )

grouped_by_year <- grid %>% 
  group_by(days_from_ban_start, days_from_ban_end, ADM1_EN, is_ban, is_ban_over, year) %>%
  summarise(
    across(starts_with("N_burnt"), sum, .names = "SUM_{col}"),  # Sum all columns starting with "N_burnt"
    across(starts_with("N_fires"), sum, .names = "SUM_{col}"),  # Sum all columns starting with "N_fires"
    across(starts_with("N_fires_"), sum, .names = "SUM_{col}")  # Sum all columns starting with "N_fires_"
  )

aggregated_by_year_start <- grouped_by_year %>% 
  group_by(days_from_ban_start, year) %>%
  summarise(
    across(starts_with("SUM_"), sum, .names = "t{col}") 
  ) %>% 
  filter(days_from_ban_start >= -45, days_from_ban_start <= 45)

aggregated_by_year_end <- grouped_by_year %>% 
  group_by(days_from_ban_end, year) %>%
  summarise(
    across(starts_with("SUM_"), sum, .names = "t{col}") 
  ) %>% 
  filter(days_from_ban_end >= -45, days_from_ban_end <= 45)

#write.csv(grid, "grid_1702.csv")

grouped_es_start <- grouped_for_es %>% 
  filter(days_from_ban_start >= -45, days_from_ban_start <= 45)
grouped_es_end <- grouped_for_es %>% 
  filter(days_from_ban_end >= -45, days_from_ban_end <= 45)

aggregated_es_start <- grouped_for_es %>%
  group_by(days_from_ban_start) %>%
  summarise(
    across(starts_with("SUM_"), sum, .names = "t{col}") 
  ) %>% 
  filter(days_from_ban_start >= -45, days_from_ban_start <= 45)

aggregated_es_end <- grouped_for_es %>%
  group_by(days_from_ban_end) %>%
  summarise(
    across(starts_with("SUM_"), sum, .names = "t{col}")  
  ) %>% 
  filter(days_from_ban_end >= -45, days_from_ban_end <= 45)


ggplot(grouped_es_start, 
       aes(x = days_from_ban_start, y = SUM_N_fires_viirs)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
  facet_wrap(~ADM1_EN, scales = "free_y") +  # Creates separate panels for each ADM1_EN
  labs(
    title = "VIIRS - Fires by region",
    x = "Date compared to start",
    y = "Fires Count"
  ) +
  theme_minimal()

ggplot(grouped_es_end, 
       aes(x = days_from_ban_end, y = SUM_N_fires_distance_group_1)) +
  geom_line() +  
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
  facet_wrap(~ADM1_EN, scales = "free_y") +  # Creates separate panels for each ADM1_EN
  labs(
    title = "Burnt Pixel Counts Over Time",
    x = "Date compared to end",
    y = "Burnt area Count"
  ) +
  theme_minimal()

ggplot(aggregated_es_start, aes(x = days_from_ban_start)) +
#ggplot(aggregated_es_end, aes(x = days_from_ban_end)) +
  geom_line(aes(y = tSUM_N_burnt_Forest, color = "Forests")) +
  #geom_line(aes(y = tSUM_N_burnt_Mosaic_TreeShrub, color = "Mosaic trees/shrubs")) +
  geom_line(aes(y = tSUM_N_burnt_Cropland, color = "Crops")) +
  #geom_line(aes(y = tSUM_N_burnt_Mosaic_ShrublandGrassland, color = "Grasslands")) +
  #geom_line(aes(y = tSUM_N_fires_distance_group_1, color = "Closest 25%")) +
  #geom_line(aes(y = tSUM_N_fires_distance_group_4, color = "Furthest 25%")) +
  labs(
    title = "Total Pixels Burned Over Time - MODIS",
    subtitle = "1km resolution",
    caption = "Sum for all nine provinces, 2016-2020",
    x = "Days from Ban Start",
    #x = "Days from Ban End",
    y = "Total Pixels burned"
  ) +
  theme_minimal() +
  ylim(0, 1000)

#ggplot(aggregated_by_year_start, aes(x = days_from_ban_start, y = tSUM_N_fires_viirs)) +
ggplot(aggregated_by_year_end, aes(x = days_from_ban_end, y = tSUM_N_fires_viirs)) +
  geom_line() +
  facet_wrap(~year, scales = "free_y") +  # Creates separate panels for each ADM1_EN
  labs(
    title = "Fires by year - VIIRS",
    x = "Date compared to start",
    y = "Burnt area Count"
  ) +
  theme_minimal()


library(DIDmultiplegtDYN)
g1 <- grid %>% 
  #filter(ADM1_EN %in% c("Chiang Mai", "Nan"))
  filter(days_from_ban_end > -30 & days_from_ban_end < 30)

g2 <- grid %>% 
  filter(days_from_ban_start > -30 & days_from_ban_start < 30)

mod_dCDH24 = did_multiplegt_dyn(
  df = g2, 
  outcome = 'N_fires', 
  group = 'ADM3_EN', 
  time = 'date', 
  treatment = 'is_ban', # original regression params
  effects   = 15,                  # no. of post-treatment periods
  placebo   = 15,                  # no. of pre-treatment periods
  cluster   = 'ADM3_EN'                 # variable to cluster SEs on
)

