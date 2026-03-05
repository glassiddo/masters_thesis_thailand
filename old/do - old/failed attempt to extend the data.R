rm(list=ls())
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest # twfe + sun and abraham event study
)
setwd("C:/Users/iddo2/Documents/thesis data/data/")

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

all_units_1 <- read_sf("2. intermediate/units/adm1_units.shp")
all_units_full <- read_sf("2. intermediate/units/adm3_units.shp")
all_units_1 <- all_units_1 %>% 
  left_join(all_units_full %>% 
              select(adm0_id, ADM0_EN, adm1_id, ADM1_EN, adm1_area) %>% 
              st_drop_geometry() %>% 
              distinct(), 
            by = "adm1_id") 

thai <- all_units_1 %>% 
  filter(ADM0_EN == "Thailand")

myan <- all_units_1 %>% 
  filter(ADM0_EN == "Myanmar")

process_fires <- function(data_path, country) {
  read_sf(data_path) %>%
    rename(date = ACQ_DATE) %>% 
    mutate(month = month(date), year = year(date)) %>%
    select(-c("INSTRUMENT", "VERSION")) %>%
    filter(
      TYPE == 0, # only presumed vegatation fires; exclude offshore, volcanos and static sources
      CONFIDENCE > 30,  # ignore fires that have 0 certainty,
      year > 2003 # some weird stuff in 2003 so ignore
      # generally would be better to exclude fires below 30 but not excluded yet
    ) %>%
    st_join(country, join = st_within) %>% # for each fire, check in which adm2 it is
    filter(!is.na(adm1_id)) %>% # remove fires outside
    mutate(
      fire_id = row_number() # give unique identifier for each fire
    ) 
}

setwd("C:/Users/iddo2/Downloads/fires")

fires_thai <- process_fires(
  "thailand/fire_archive_M-C61_620555.shp",
  thai
) 

fires_myan <- process_fires(
  "myanmar/fire_archive_M-C61_620556.shp",
  myan
) 

modis_thai <- fires_thai %>% 
  dtplyr::lazy_dt() %>% 
  group_by(adm1_id, date) %>% 
  summarise(
    total_fires = n(),
    # by day/night
    day_fires = sum(DAYNIGHT == "D"),
    night_fires = sum(DAYNIGHT == "N"),
    .groups = "drop"
  ) %>% 
  as.data.frame()

modis_myan <- fires_myan %>% 
  dtplyr::lazy_dt() %>% 
  group_by(adm1_id, date) %>% 
  summarise(
    total_fires = n(),
    # by day/night
    day_fires = sum(DAYNIGHT == "D"),
    night_fires = sum(DAYNIGHT == "N"),
    .groups = "drop"
  ) %>% 
  as.data.frame()

modis_comb <- bind_rows(modis_myan, modis_thai) %>% 
  mutate(date = as.Date(date)) %>% 
  dtplyr::lazy_dt() 

dates <- c(
  seq(as.Date("2003-01-01"), as.Date("2020-12-31"), by="day")
)

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

setwd("C:/Users/iddo2/Documents/thesis data/data/")
units_info <- fread("2. intermediate/units1_with_avgs.csv")
#bans_grid <- fread("2. intermediate/bans_grid_1405.csv")
bans_grid <- fread("2. intermediate/bans_grid_real0206.csv")
gdp_data <- fread("2. intermediate/gdp_data.csv")

grid <- expand.grid(
  date = dates,
  adm1_id = unique(all_units_1$adm1_id),
  stringsAsFactors = FALSE
  ) %>% 
  dtplyr::lazy_dt() %>% 
  mutate(year = year(date)) %>% 
  full_join(all_units_1 %>% st_drop_geometry(), by = "adm1_id") %>% 
  left_join(gdp_data, by = "ADM0_EN") %>% 
  left_join(units_info, by = "adm1_id") %>% 
  left_join(modis_comb, by = c("adm1_id", "date")) 

num_cols <- grid %>%
  as.data.frame() %>%  # dtplyr can't be used here directly for tidyselect
  select(where(is.numeric)) %>%
  select(-adm1_id, -adm1_area, -year, -adm0_id, 
         -tree_share, -urban_share, -crop_share, -avg_elevation, -gdp_2005) %>%
  names()

grid <- grid %>% 
  mutate(
    across(all_of(num_cols), ~replace_na(., as.integer(0)))
  ) %>%
  as.data.frame() %>% 
  mutate(
    treated_unit = ifelse(ADM1_EN %in% northern_regions_en, 1, 0),
    post_treatment_first_version = ifelse(year > 2013, 1, 0),
    post_treatment_second_version = ifelse(year > 2016, 1, 0),
    is_treated_first = ifelse(treated_unit == 1 & post_treatment_first_version == 1, 1, 0),
    is_treated_second = ifelse(treated_unit == 1 & post_treatment_second_version == 1, 1, 0)
  ) %>% 
  left_join(bans_grid, by = c("ADM1_EN", "year")) %>%
  group_by(year) %>%
  mutate(
    Ban_start = as.Date(Ban_start),
    Ban_end = as.Date(Ban_end),
    days_from_ban_start = as.integer(date - Ban_start),
    days_from_ban_end = as.integer(date - Ban_end)
  ) %>%
  ungroup() %>%
  mutate(
    is_ban_theory = ifelse(
      days_from_ban_start >= 0 & days_from_ban_end < 0, 1, 0),
    is_ban_actual_1 = ifelse(
      is_ban_theory == 1 & is_treated_first == 1, 1, 0),
    is_ban_actual_2 = ifelse(
      is_ban_theory == 1 & is_treated_second == 1, 1, 0),
    event_time_years_first = as.factor(year - 2013),
    event_time_years_second = as.factor(year - 2016)
  ) %>%
  group_by(date) %>% 
  mutate(time_id = cur_group_id()) %>%
  ungroup() 
  
fwrite(grid, "grid_long_data.csv", append = F)
