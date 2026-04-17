source("do/setup.R")

modis_fires <- read_sf(here(build.dir, "modis_with_region_2206.gpkg"))
viirs_fires <- read_sf(here(build.dir, "viirs_with_region_2906.gpkg"))

#### distances ----
quantiles_n <- 2 # theoretically could choose to actually use quantiles, rather than top/bottom 50%

modis_distances <- readRDS(here(build.dir, "fires", "modis_with_distances.rds")) |>  
  left_join(
    modis_fires |>  
    st_drop_geometry() |> 
    select(adm0_id, adm1_id, fire_id),
    join_by(fire_id)
  ) |>  
  # compute quantiles within adm1 and adm0
  mutate(
    road_q_province = ntile(distance_to_road, quantiles_n),
    urban_q_province = ntile(distance_to_urban, quantiles_n),
    .by = c(adm0_id, adm1_id)
  ) |> 
  mutate(
    road_q_country = ntile(distance_to_road, quantiles_n),
    urban_q_country = ntile(distance_to_urban, quantiles_n),
    .by = adm0_id
  ) |> 
  select(-adm0_id, -adm1_id)

#### crops and land use ----

modis_crops <- readRDS(here(build.dir, "fires", "modis_with_crops.rds"))
modis_landuse <- readRDS(here(build.dir, "fires", "modis_with_landuse.rds"))

##### summarise to days -----

modis_joined <- modis_fires |>  
  st_drop_geometry() |>  
  left_join(modis_distances, join_by(fire_id)) |> 
  left_join(modis_crops, join_by(fire_id)) |>  
  left_join(modis_landuse, join_by(fire_id)) 
  #left_join(modis_elevation, join_by(fire_id))
  
#### viirs -----
viirs_summary <- viirs_fires |>  
  st_drop_geometry() |>  
  summarise(
    total_fires_viirs = n(),
    # by day/night
    day_fires_viirs = sum(DAYNIGHT == "D"),
    night_fires_viirs = sum(DAYNIGHT == "N"),
    .by = c(adm1_id, date)
  )

rm(viirs_fires)
#### full summary level 1-----

modis_summary <- modis_joined |>  
  group_by(adm1_id, date) |>  
  summarise(
    total_fires = n(),
    # by day/night
    day_fires = sum(DAYNIGHT == "D"),
    night_fires = sum(DAYNIGHT == "N"),
    # by satellite
    terra_fires = sum(SATELLITE == "Terra"),
    aqua_fires = sum(SATELLITE == "Aqua"),
    # by satellite and day/night
    terra_night_fires = sum(SATELLITE == "Terra" & DAYNIGHT == "N"),
    terra_day_fires = sum(SATELLITE == "Terra" & DAYNIGHT == "D"),
    aqua_night_fires = sum(SATELLITE == "Aqua" & DAYNIGHT == "N"),
    aqua_day_fires = sum(SATELLITE == "Aqua" & DAYNIGHT == "D"),
    # only high confidence fires
    high_confidence_fires50 = sum(CONFIDENCE > 50),
    high_confidence_fires75 = sum(CONFIDENCE > 75),
    # by frp
    q1_frp_fires = sum(FRP < 20.8), 
    q2_frp_fires = sum(FRP > 20.8 & FRP > 11.3),
    # by frp and satellite's overpass
    q1_frp_fires_aqua_day = sum(FRP < 20.8 & SATELLITE == "Aqua" & DAYNIGHT == "D"), 
    q2_frp_fires_aqua_day = sum(FRP > 20.8 & FRP > 11.3 & SATELLITE == "Aqua" & DAYNIGHT == "D"), 
    # by road quantiles
    q1_roads_prov = sum(road_q_province == 1, na.rm = T), # closer than median
    q2_roads_prov = sum(road_q_province == 2, na.rm = T), # further than median
    q1_roads_country = sum(road_q_country == 1, na.rm = T), 
    q2_roads_country = sum(road_q_country == 2, na.rm = T), 
    # by urban quantiles
    q1_urban_prov = sum(urban_q_province == 1, na.rm = T), 
    q2_urban_prov = sum(urban_q_province == 2, na.rm = T), 
    q1_urban_country = sum(urban_q_country == 1, na.rm = T), 
    q2_urban_country = sum(urban_q_country == 2, na.rm = T), 
    # by crop production
    q1_crop_prod = sum(crop_production_q == 1, na.rm = T), # lowest production
    q2_crop_prod = sum(crop_production_q == 2, na.rm = T),
    q3_crop_prod = sum(crop_production_q == 3, na.rm = T),
    q4_crop_prod = sum(crop_production_q == 4, na.rm = T),
    # by dominant crop
    rice_dom = sum(max_crop == "rice", na.rm = T),
    maize_dom = sum(max_crop == "maiz", na.rm = T),
    cass_dom = sum(max_crop == "cass", na.rm = T),
    # # # by land use
    crop75_fires = sum(crop75 == 1, na.rm = T),
    crop50_fires = sum(crop50 == 1, na.rm = T),
    crop33_fires = sum(crop33 == 1, na.rm = T),
    forest50_fires = sum(forest50 == 1, na.rm = T),
    crop50_aqua_day_fires = sum(
      crop50 == 1 & SATELLITE == "Aqua" & DAYNIGHT == "D", na.rm = T
      ),
    crop50_aqua_night_fires = sum(
      crop50 == 1 & SATELLITE == "Aqua" & DAYNIGHT == "N", na.rm = T
    ),
    crop50_terra_day_fires = sum(
      crop50 == 1 & SATELLITE == "Terra" & DAYNIGHT == "D", na.rm = T
    ),
    crop50_terra_night_fires = sum(
      crop50 == 1 & SATELLITE == "Terra" & DAYNIGHT == "N", na.rm = T
      ),
    forest50_aqua_day_fires = sum(
      forest50 == 1 & SATELLITE == "Aqua" & DAYNIGHT == "D", na.rm = T
    ),
    forest50_aqua_night_fires = sum(
      forest50 == 1 & SATELLITE == "Aqua" & DAYNIGHT == "N", na.rm = T
    ),
    forest50_terra_day_fires = sum(
      forest50 == 1 & SATELLITE == "Terra" & DAYNIGHT == "D", na.rm = T
    ),
    forest50_terra_night_fires = sum(
      forest50 == 1 & SATELLITE == "Terra" & DAYNIGHT == "N", na.rm = T
    ),
    .groups = "drop"
  )

#### without duplicates ----

#### write modis summarised ----
saveRDS(
  modis_summary, 
  here(build.dir, "modis_summarised.rds"), 
  append = F
  )

saveRDS(
  viirs_summary, 
  here(build.dir, "viirs_summarised.rds"), 
  append = F
)
