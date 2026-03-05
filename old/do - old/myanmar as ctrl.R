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

sf_use_s2(FALSE)

adm1 <- ne_states(returnclass = "sf") %>%
  filter(admin %in% c("Myanmar", "Laos", "Thailand")) %>%
  filter(
    latitude > 15, latitude < 26, longitude < 103 # ignore south of thailand and north of myanmar
  ) %>% 
  dplyr::select(admin, name) %>% 
  mutate(id = row_number()) %>% 
  mutate(area = as.numeric(st_area(geometry)))

my_adm1 <- adm1 %>% 
  st_set_geometry(NULL) %>% 
  filter(
    admin == "Myanmar"
  ) %>% 
  select(name)  %>% 
  distinct() %>% 
  pull()

my3 <- read_sf(
  "admin units elsewhere/mmr_adm_250k_mimu_20240215_ab_shp/mmr_polbnda_adm3_250k_mimu_20240215.shp"
  ) %>% 
  filter(
    ADM1_EN %in% my_adm1
  ) %>% 
  mutate(
    adm3_id_my = row_number()
  ) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, adm3_id_my)


  
#process_fires_my <- function(data_path) {
#   read_sf(data_path) %>% 
#     mutate(month = month(ACQ_DATE), year = year(ACQ_DATE)) %>% 
#     select(-c("INSTRUMENT", "VERSION")) %>% 
#     #filter(ACQ_DATE > "2016-01-01" & ACQ_DATE < "2020-12-31") %>% 
#     filter(TYPE != 3, month < 6, year < 2021) %>% 
#     st_join(adm1, join = st_within) %>% 
#     filter(!is.na(admin)) %>%
#     mutate(
#       fire_id_my = row_number()
#     ) %>% 
#     st_join(my3, join = st_within) %>% 
#     filter(!is.na(ADM3_EN))
# }


#modis_my <- process_fires_my("firms from nasa 2202 full/fire_archive_M-C61_581681.shp") 

modis_my <- read_sf("modis_with_region_myanmar.gpkg") %>% 
  st_set_geometry(NULL) %>% 
  group_by(adm3_id_my, ACQ_DATE) %>% 
  summarise(
    n_fires_modis = n()
  ) 

grid <- fread("temp_grid_1904.csv") %>% 
  #filter(ADM1_EN %in% northern_regions_en) %>% 
  mutate(month = month(date)) %>% 
  filter(month < 6, year > 2011) %>% 
  mutate(
    date = as.Date(date),
    adm3_id = as.character(adm3_id)
  )

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

grid_my <- expand.grid(
  date = dates,
  adm3_id_my = unique(my3$adm3_id_my),
  stringsAsFactors = FALSE
  ) %>% 
  mutate(year = year(date)) %>% 
  full_join(my3 %>% st_set_geometry(NULL), by = "adm3_id_my") %>% 
  left_join(modis_my %>% rename(date = ACQ_DATE), by = c("adm3_id_my", "date")) %>%
  mutate(
    n_fires_modis = replace_na(n_fires_modis, 0)
  ) %>%
  mutate(
    across(
      where(is.numeric), #& !c(id),
      ~replace_na(., 0)
    )
  ) %>% 
  filter(!ADM1_EN %in% c("Rakhine", "Chin")) %>%  # very west
  mutate(
    date = as.Date(date),
    adm3_id = as.character(paste0(adm3_id_my, "-my"))
  ) %>% 
  select(-adm3_id_my)

grid_mixed <- bind_rows(grid, grid_my) %>% 
  mutate(
    is_ban = replace_na(is_ban, 0)
  )
  
feols(n_fires_modis ~ is_ban | date + adm3_id, data = grid_mixed, family = "poisson")

grid_mixed <- grid_mixed %>% 
  mutate(
    is_ever_treated = ifelse(ADM1_EN %in% northern_regions_en & year > 2016, 1, 0),
    is_treated = ifelse(is_ever_treated & is_ban == 1, 1, 0),
    is_treated_over = ifelse(is_ever_treated & is_ban_over == 1, 1, 0),
    adm1_id = as.character(ADM1_EN)
  ) %>% 
  group_by(adm3_id) %>%
  mutate(time_id = row_number()) %>%
  ungroup() %>% 
  group_by(time_id, days_from_ban_start, days_from_ban_end, adm1_id, is_ever_treated, is_treated, is_treated_over) %>%
  summarise(
    n_fires_modis = sum(n_fires_modis),
    fires_binary = ifelse(n_fires_modis > 0, 1, 0)
  ) %>%
  ungroup()

model <- feols(
  fires_binary ~ i(days_from_ban_end,is_ever_treated, ref = "-10") | adm1_id + date,
  data = grid_mixed %>% filter(days_from_ban_end > -25, days_from_ban_end < 25)
)

summary(model)


pre_treatment_means <- grid_mixed_fixed %>%
  filter(year <= 2016) %>%
  group_by(ADM1_EN) %>%
  summarise(pre_mean_fires = mean(n_fires_modis, na.rm = TRUE))

# Step 2: Join with main data
grid_mixed_fixed <- grid_mixed_fixed %>%
  left_join(pre_treatment_means, by = "ADM1_EN") %>%
  mutate(
    fires_rel = n_fires_modis / pre_mean_fires  # or use subtraction if you prefer
  )
