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

population_myanmar <- fread(
  "population/householdpopulationbaseddatasetmimutownshipsabbreviated.csv"
  ) %>% 
  select(
    name_st, name_dt, name_ts, pop_t, pop_m, pop_f, hh_t
  ) %>% 
  rename(
    ADM1_EN = name_st,
    ADM2_EN = name_dt, 
    ADM3_EN = name_ts, 
    Total_pop = pop_t, 
    Male_pop = pop_m, 
    Female_pop = pop_f, 
    HH_num = hh_t
  )

my3_pop <- my3 %>% 
  st_set_geometry(NULL) %>% 
  left_join(population_myanmar, by = c("ADM1_EN", "ADM2_EN", "ADM3_EN"))

write.csv(my3_pop, "population/population_myanmar_cleaned.csv")
