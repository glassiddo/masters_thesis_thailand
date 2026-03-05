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

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

adm1 <- ne_states(returnclass = "sf") %>%
  filter(admin %in% c("Myanmar", "Thailand", "Laos")) %>%
  filter(
    latitude > 11, # ignore south of thailand
    #!name %in% c("Tanintharyi", "Chin", "Rakhine", "Sagaing", "Kachin") 
  ) %>%
  # ignore south of thailand and north of myanmar
  select(admin, name) %>% 
  rename(
    ADM0_EN = admin,
    ADM1_EN = name
  ) %>% 
  mutate(
      ADM1_EN = case_when(
        ADM1_EN == "Oudômxai" ~ "Oudomxai",
        ADM1_EN == "Phôngsali" ~ "Phongsaly",
        ADM1_EN == "Louangphrabang" ~ "Louangphabang",
        ADM1_EN == "Louang Namtha" ~ "Louangnamtha",
        ADM1_EN == "Xaignabouri" ~ "Xaignabouly",
        TRUE ~ ADM1_EN
      )  # simplify non english letters or clean names in laos
  ) 

# ggplot() + geom_sf(data=adm1, aes(fill = ADM0_EN))

adm1_names <- adm1 %>% 
  st_set_geometry(NULL) %>% 
  select(ADM1_EN) %>% 
  distinct() %>% 
  pull()

thailand <- 
  read_sf(
    "1. raw/admin units/thailand/admin3_simplified_en.shp"
  ) %>% 
  filter(
    ADM1_EN %in% adm1_names
  ) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN) %>% 
  mutate(
    ADM0_EN = "Thailand"
  )

myanmar <- 
  read_sf(
    "1. raw/admin units/myanmar/mmr_polbnda_adm4_250k_mimu_20240215.shp"
  ) %>% 
  filter(
    ADM1_EN %in% adm1_names | 
      ADM1_EN %in% c(
        "Shan (East)", "Shan (North)", "Shan (South)", "Bago (East)", "Bago (West)",
        "Nay Pyi Taw"
      )
  ) %>% 
  mutate(
    ADM4_EN = case_when(
      is.na(ADM4_EN) ~ ADM3_EN,
      TRUE ~ ADM4_EN
    )
  ) %>% 
  select(ADM1_EN, ADM2_EN, ADM3_EN, ADM4_EN) %>% 
  st_make_valid() %>%
  # transform adm2_en into adm1_en and adm3_en into adm2_en as otherwise units are 
  # much larger than the ones in thailand and laos
  group_by(ADM2_EN, ADM3_EN, ADM4_EN) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  rename(
    ### take one admin level lower for myanmar as equivaent to thailand
    ADM1_EN = ADM2_EN,
    ADM2_EN = ADM3_EN,
    ADM3_EN = ADM4_EN
  ) %>% 
  mutate(
    ADM0_EN = "Myanmar"
  )
  

laos <- read_sf("1. raw/admin units/laos/Laos_Population_Census_2015.shp") %>%
  rename(
    Total_pop = unpdeaa01,
    ADM1_EN = urcne,
    ADM2_EN = uscne,
    ADM3_EN = uucne
  ) %>%
  filter(
    ADM1_EN %in% adm1_names |
      ADM1_EN == "Vientiane Capital"
  ) %>%
  select(ADM1_EN, ADM2_EN, ADM3_EN) %>%
  mutate(
    ADM0_EN = "Laos",
    ADM2_EN = gsub(" District", "", ADM2_EN), # clean name
    ADM3_EN = gsub("B\\. ", "", ADM3_EN)
  )
  
all_units <- bind_rows(thailand, myanmar, laos) %>% 
  mutate(
    adm3_area = as.numeric(st_area(geometry)),
    adm3_id = row_number() # unique identifier
  ) %>%
  group_by(ADM0_EN, ADM1_EN, ADM2_EN) %>%
  mutate(
    adm2_id = cur_group_id(),
    adm2_area = sum(adm3_area)
  ) %>%
  group_by(ADM0_EN, ADM1_EN) %>%
  mutate(
    adm1_id = cur_group_id(),
    adm1_area = sum(adm2_area)
  ) %>%
  group_by(ADM0_EN) %>%
  mutate(
    adm0_id = cur_group_id()
  ) %>%
  ungroup()

#ggplot() + geom_sf(data = all_units, aes(fill = adm1_area)) + scale_fill_viridis_c()

write_sf(all_units, "2. intermediate/units/adm3_units.shp")

#### this dissolves geometries into larger units -----

### into admin 2 --------

# ### using the following code it would take too long; so using data.table
# all_units_adm2 <- read_sf("2. intermediate/units/adm3_units.shp") %>% 
#   select(adm2_id) %>% 
#   #st_make_valid() %>%
#   group_by(adm2_id) %>%
#   summarise(geometry = st_union(geometry), .groups = "drop")

# convert to dt while keeping structure of sf, otherwise there are some bugs
dt_adm2 <- read_sf("2. intermediate/units/adm3_units.shp") %>% 
  dplyr::select(adm2_id) %>% 
  as.data.table()

result_adm2 <- dt_adm2[, .(geometry = list(st_union(geometry))), by = adm2_id]

# convert "back" into sf
all_units_adm2 <- st_sf(
  adm2_id = result_adm2$adm2_id,
  geometry = st_sfc(unlist(result_adm2$geometry, recursive = FALSE)),
  crs = st_crs("EPSG:4326")
)

### same with adm1 -----
dt_adm1 <- read_sf("2. intermediate/units/adm3_units.shp") %>% 
  dplyr::select(adm1_id) %>% 
  as.data.table()

result_adm1 <- dt_adm1[, .(geometry = list(st_union(geometry))), by = adm1_id]

all_units_adm1 <- st_sf(
  adm1_id = result_adm1$adm1_id,
  geometry = st_sfc(unlist(result_adm1$geometry, recursive = FALSE)),
  crs = st_crs("EPSG:4326")
)

#### write them -----
write_sf(all_units_adm1, "2. intermediate/units/adm1_units.shp")
write_sf(all_units_adm2, "2. intermediate/units/adm2_units.shp")

all_units_1_with_details <- all_units_adm1 %>% 
  #st_drop_geometry() %>% 
  left_join(all_units %>% 
              select(adm0_id, ADM0_EN, adm1_id, ADM1_EN, adm1_area) %>% 
              st_drop_geometry() %>% 
              distinct(), 
            by = "adm1_id")

write_sf(all_units_1_with_details, 
         "2. intermediate/units/adm1_units_detailed.shp")