source("code/setup.R")

## compute distances of each fire from roads in its country

modis_fires <- read_sf(here(build.dir, "modis_with_region_2206.gpkg"))

compute_distances <- function(country_name, country_id) {
  # filter to country
  # as i only want to ocmpute distances to things within the same country
  modis_country <- modis_fires %>% filter(adm0_id == country_id) %>% select(fire_id)
  
  # vectorise the fires sf
  modis_vec <- vect(modis_country)
  
  # read the files
  country_lower <- tolower(country_name) 
  roads_distances <- rast(here(build.dir, "roads", paste0("road_distance_", country_lower, ".tif")))
  urban_distances <- rast(here(build.dir, "roads", paste0("distance_gt20_km_", country_lower, ".tif")))  
  
  # compute distances
  distances_roads <- extract(roads_distances, modis_vec)
  distances_urban <- extract(urban_distances, modis_vec)
  
  # attach to the fires
  modis_country <- modis_country %>% 
    mutate(
      distance_to_road = distances_roads$distance,
      distance_to_urban = distances_urban$distance
    ) %>% 
    st_drop_geometry()
  
  return(modis_country)
}

# crs dont match so automatically transforms to crs of the raster
thai <- compute_distances("Thailand", 3)
laos <- compute_distances("Laos", 1)
myanmar <- compute_distances("Myanmar", 2)

fires_distances <- bind_rows(thai, myanmar, laos)

fwrite(fires_distances, here(build.dir, "fires_with_distances.csv"))
