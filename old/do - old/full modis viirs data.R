rm(list=ls())
#dev.off()
gc()

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

setwd("C:/Users/iddo2/Documents/thesis data/data/")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

bans <- fread("ban dates.csv") %>% 
  select(Region, `Ban start`, `Ban end`) %>%
  rename(name = Region,
         Ban_start = `Ban start`, 
         Ban_end = `Ban end`) %>%
  mutate(Ban_start = as.Date(Ban_start, format = "%Y-%m-%d"),
         Ban_end = as.Date(Ban_end, format = "%Y-%m-%d"),
         year = lubridate::year(Ban_start)) %>% 
  filter(!is.na(Ban_start))


adm1 <- ne_states(returnclass = "sf") %>%
  filter(admin %in% c("Myanmar", "Laos", "Thailand")) %>%
  filter(
    latitude > 15, latitude < 26, longitude < 103 # ignore south of thailand and north of myanmar
    #admin == "Thailand" 
    # | 
    # (admin == "Myanmar" & name %in% c(
    #   "Shan", "Kayin", "Kayah", "Sagaing", "Mon", "Kachin", "Tanintharyi", 
    #   "Ayeyarwady", "Yangon", "Bago", "Mandalay"  
    #   )) |
    # (admin == "Laos" & name %in% c(
    #   "Phôngsali", "Oudômxai", "Louang Namtha", "Bokeo", "Xaignabouri",
    #   "Vientiane", "Vientiane [prefecture]", "Louangphrabang", "Xiangkhoang"
    #   )
    #  ) 
    ) %>% 
  dplyr::select(admin, name) %>% 
  mutate(id = row_number()) %>% 
  mutate(area = as.numeric(st_area(geometry)))

# ggplot(adm1) + geom_sf(aes(fill = admin))

admin_units <- read_sf("thai admin units/admin3_simplified_en.shp") %>% 
  mutate(
    adm3_id = row_number()
  )

process_fires <- function(data_path) {
  read_sf(data_path) %>% 
    mutate(month = month(ACQ_DATE), year = year(ACQ_DATE)) %>% 
    select(-c("INSTRUMENT", "VERSION")) %>% 
    #filter(ACQ_DATE > "2016-01-01" & ACQ_DATE < "2020-12-31") %>% 
    filter(TYPE != 3, month < 6, year < 2021) %>% 
    st_join(adm1, join = st_within) %>% 
    filter(!is.na(admin)) %>%
    mutate(
      fire_id = row_number()
    ) %>% 
    st_join(admin_units, join = st_within)
}
gc()

#modis <- process_fires("firms from nasa 2202 full/fire_archive_M-C61_581681.shp") 
#viirs <- process_fires("firms from nasa 2202 full/fire_archive_SV-C2_581682.shp")

#st_write(modis_my, "modis_with_region_myanmar.gpkg", append = FALSE)
modis <- read_sf("modis_with_region.gpkg")# %>% filter(ADM1_EN %in% northern_regions_en)
modis_my <- read_sf("modis_with_region_myanmar.gpkg")

dates <- c(
  # seq(as.Date("2011-01-20"), as.Date("2011-06-30"), by="day"),
  seq(as.Date("2012-01-01"), as.Date("2012-05-31"), by="day"),
  seq(as.Date("2013-01-01"), as.Date("2013-05-31"), by="day"),
  seq(as.Date("2014-01-01"), as.Date("2014-05-31"), by="day"),
  seq(as.Date("2015-01-01"), as.Date("2015-05-31"), by="day"),
  seq(as.Date("2016-01-01"), as.Date("2016-05-31"), by="day"),
  seq(as.Date("2017-01-01"), as.Date("2017-05-31"), by="day"),
  seq(as.Date("2018-01-01"), as.Date("2018-05-31"), by="day"),
  seq(as.Date("2019-01-01"), as.Date("2019-05-31"), by="day"),
  seq(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day")
  )


modis_nogeom <- modis %>% 
  st_set_geometry(NULL) %>% 
  # First, replace NAs with a separate category
  # mutate(
  #   elevation_category = case_when(
  #     is.na(above_north_avg_elev) ~ "unknown",
  #     above_north_avg_elev == 1 ~ "above_avg",
  #     above_north_avg_elev == 0 ~ "below_avg"
  #   )
  # ) %>%
  group_by(adm3_id, ACQ_DATE) %>% 
  summarise(
    n_fires_modis = n()
  )
  # Pivot to create separate columns
  pivot_wider(
    names_from = Price_bracket,
    values_from = n_fires_modis,
    values_fill = 0
  ) 
  mutate(
    #n_fires_total = (above_avg %||% 0) + (below_avg %||% 0) + (unknown %||% 0)
    n_fires_total = `3` + `1` + `2` + `4` + `5`
  ) #%>%
  #select(-unknown)


viirs_nogeom <- viirs %>% 
  st_set_geometry(NULL) %>% 
  group_by(admin, name, ACQ_DATE, .groups = "drop") %>% 
  summarise(n_fires_viirs = n())

grid <- expand.grid(
  date = dates,
  id = unique(adm1$id),
  stringsAsFactors = FALSE
  ) %>%
  mutate(year = year(date)) %>% 
  full_join(adm1 %>% st_set_geometry(NULL), by = "id") %>% 
  left_join(modis_nogeom %>% rename(date = Date, name = province), by = c("admin", "name", "date")) %>%
  left_join(pollution_for_join %>% rename(date = Date, name = province), by = c("name", "date")) %>% 
  #left_join(viirs_nogeom %>% rename(date = ACQ_DATE), by = c("admin", "name", "date")) %>% 
  mutate(
    n_fires_modis = replace_na(n_fires_modis, 0)
    #n_fires_viirs = replace_na(n_fires_viirs, 0)
    ) %>%
  mutate(
    across(
      # Select all columns that came from top_crop in pivot_wider
      # This assumes all crop columns are numeric and not part of your original grid data
      where(is.numeric) & !c(id),
      ~replace_na(., 0)
    )
  ) %>% 
  # mutate(
  #     total_crop = crop_N + crop_D,
  #     total_tree = tree_N + tree_D,
  #     total_other = other_N + other_D,
  #     total_N = rowSums(across(ends_with("_N"))),
  #     total_D = rowSums(across(ends_with("_D"))),
  #     total = total_N + total_D
  # ) %>% 
  left_join(bans, 
            by = c("name", "year")) %>% 
  mutate(
    days_from_ban_start = as.numeric(date - Ban_start),    
    days_from_ban_end = as.numeric(date - Ban_end),
    is_ban = case_when(
      is.na(Ban_start) ~ NA,
      (!is.na(Ban_start) & !is.na(Ban_end) &
         year == lubridate::year(Ban_start) &
         date >= Ban_start & date <= Ban_end
      ) ~ 1, 
      TRUE ~ 0
    ),
    is_ban_over = ifelse(is_ban == 0 & days_from_ban_end > 0, 
                         1, 0) # for event study of ban being over
  )
  # mutate(
  #   total_crops = rowSums(select(., contains("crops"))),
  #   total_forests = rowSums(select(., contains("forests"))),
  #   total_other = rowSums(select(., contains("other"))),
  #   total_rice = rowSums(select(., contains("rice"))),
  #   total_rubber = rowSums(select(., contains("rubber"))),
  #   total_maize = rowSums(select(., contains("maize")))
  # )


# ggplot(grid %>% filter(year == 2017), aes(x = date, y = rice_crops)) +
#   geom_line() +
#   #geom_smooth(method = "loess", span = 0.4, se = FALSE, color = "blue") +
#   facet_wrap(~admin, scales = "free_y") +
#   theme_bw()

## with bans -----

grouped_for_es_start <- grid %>%
  group_by(days_from_ban_start, admin) %>%
  summarise(
    across(
      where(is.numeric) & 
        !c(year, id, is_ban, is_ban_over, Ban_start, Ban_end, days_from_ban_end),
      ~sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>% 
  filter(days_from_ban_start >= -30, days_from_ban_start <= 30)

grouped_for_es_end <- grid %>%
  group_by(days_from_ban_end, admin) %>%
  summarise(
    # viirs_tot = sum(n_fires_viirs),
    # modis_tot = sum(n_fires_modis)
    across(
      where(is.numeric) & 
        !c(year, id, is_ban, is_ban_over, Ban_start, Ban_end, days_from_ban_start),
      ~sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>% 
  filter(days_from_ban_end >= -30, days_from_ban_end <= 30)

ggplot(
  grouped_for_es_end, 
  aes(
    x = days_from_ban_end, 
    #y = mean_PM2.5_prov
    y = n_fires_modis
    )
  ) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") + 
  #facet_wrap(~name, scales = "free_y") +  # Creates separate panels for each ADM1_EN
  labs(
    title = "Fires by region",
    x = "Date compared to x",
    y = "Fires Count"
  ) +
  theme_minimal()

library(bunching)
bandwidth <- 5
bins_each <- round(30/bandwidth,0)

expanded_fires <- grouped_for_es_start %>%
  uncount(
    weights = round(mean_PM2.5_prov), 
    .remove = FALSE
    )  # Repeat each row viirs_tot times

plot_hist(z_vector = expanded_fires$days_from_ban_start,
          zstar = -1, 
          binwidth = bandwidth,
          bins_l = bins_each, 
          bins_r = bins_each,
          #binv = "max",
          p_title = "Kink", 
          p_title_size = 15)$plot

bun <- bunchit(
  z_vector = expanded_fires$days_from_ban_start,
  zstar = -1, 
  t0 = 0,
  t1 = 0.1,
  binwidth = bandwidth,
  bins_l = bins_each, 
  bins_r = bins_each
  #binv = "max",
)

bun$b
bun$b_sd
bun$e
bun$e_sd
