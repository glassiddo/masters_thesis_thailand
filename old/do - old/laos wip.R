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

lao_adm1 <- adm1 %>% 
  st_set_geometry(NULL) %>% 
  filter(
    admin == "Laos"
  ) %>% 
  select(name)  %>% 
  distinct() %>% 
  pull()

lao3 <- read_sf(
  "admin units elsewhere/laos_population_census_2015/Laos_Population_Census_2015.shp"
  ) %>% 
  rename(
    Total_pop = unpdeaa01,
    ADM1_EN = urcne,
    ADM2_EN = uscne,
    ADM3_EN = uucne
  ) %>% 
  # filter(
  #   ADM1_EN %in% lao_adm1
  # ) %>% 
  mutate(
    ADM2_EN = gsub(" District", "", ADM2_EN),
    ADM3_EN = gsub("B\\. ", "", ADM3_EN)
  ) %>% 
  mutate(
    adm3_id_lao = row_number()
  ) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, adm3_id_lao, Total_pop) %>% 
  mutate(
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2]
  )
  

process_fires_lao <- function(data_path) {
  read_sf(data_path) %>%
    mutate(month = month(ACQ_DATE), year = year(ACQ_DATE)) %>%
    select(-c("INSTRUMENT", "VERSION")) %>%
    filter(TYPE != 3, month < 6, year < 2021) %>%
    st_join(adm1, join = st_within) %>%
    filter(!is.na(admin)) %>%
    mutate(
      fire_id_lao = row_number()
    ) %>%
    st_join(lao3, join = st_within) %>%
    filter(!is.na(ADM3_EN))
}


#modis_lao <- process_fires_lao("firms from nasa 2202 full/fire_archive_M-C61_581681.shp") 

#write_sf(modis_lao, "modis_with_region_laos.gpkg")

modis_lao <- read_sf("modis_with_region_laos.gpkg") %>% 
  st_set_geometry(NULL) %>% 
  group_by(adm3_id_lao, ACQ_DATE) %>% 
  summarise(
    n_fires_modis = n()
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

grid_lao <- expand.grid(
  date = dates,
  adm3_id_lao = unique(lao3$adm3_id_lao),
  stringsAsFactors = FALSE
  ) %>% 
  mutate(year = year(date)) %>% 
  full_join(lao3 %>% st_set_geometry(NULL) %>% select(-centroid), by = "adm3_id_lao") %>% 
  left_join(modis_lao %>% rename(date = ACQ_DATE), by = c("adm3_id_lao", "date")) %>%
  left_join(admin_climate_results %>% 
              rename(adm3_id_lao = id) %>% 
              select(adm3_id_lao, date, starts_with("avg")),
            by = c("adm3_id_lao", "date")) %>% 
  mutate(
    n_fires_modis = replace_na(n_fires_modis, 0),
    across(
      where(is.numeric), #& !c(id),
      ~replace_na(., 0)
    ),
    ADM0_EN = "Laos",
    date = as.Date(date),
    year = year(date),
    adm3_id = as.character(paste0(adm3_id_lao, "-lao")),
    is_ban = 0
    #adm1_id = as.character(ADM1_EN)
  ) %>% 
  group_by(adm3_id) %>%
  mutate(time_id = row_number()) %>%
  ungroup() %>% 
  select(-adm3_id_lao)

grid <- fread("grid_mixed0805.csv") %>% mutate(date = as.Date(date))
grid_mixed <- bind_rows(grid, grid_lao)
### ban dates are missing will fill them

fwrite(grid_mixed, "grid_mixed0805_with_laos_partial.csv")

feols(n_fires_modis ~ is_ban | date + adm3_id, data = grid_mixed)
