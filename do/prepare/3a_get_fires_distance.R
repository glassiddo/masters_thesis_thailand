source("do/setup.R")

## compute distances of each fire from roads in its country

countries <- c("Laos", "Thailand", "Myanmar")

compute_distances <- function(df, country_name) {
  # filter to country
  # as i only want to ocmpute distances to things within the same country
  df_country <- df |> 
    filter(ADM0_EN == country_name) |> 
    select(fire_id)
  
  # read the files
  country_lower <- tolower(country_name) 
  roads_distances <- rast(here(build.dir, "roads", paste0("road_distance_", country_lower, ".tif")))
  urban_distances <- rast(here(build.dir, "urban", paste0("distance_gt20_km_", country_lower, ".tif")))  
  
  
  # vectorise the fires sf
  df_vec <- vect(df_country)
  
  # compute distances
  distances_roads <- extract(roads_distances, df_vec)
  distances_urban <- extract(urban_distances, df_vec)
  
  # attach to the fires
  df_country <- df_country |> 
    mutate(
      distance_to_road = distances_roads$distance,
      distance_to_urban = distances_urban$distance
    ) |> 
    st_drop_geometry()
  
  return(df_country)
}

# crs dont match so automatically transforms to crs of the raster
modis_fires <- read_sf(here(build.dir, "fires", "modis_with_region.gpkg"))

modis_distances <- map_dfr(countries, ~compute_distances(modis_fires, .x))

saveRDS(modis_distances, here(build.dir, "fires", "modis_with_distances.rds"))

rm(modis_distances, modis_fires)

viirs_fires <- read_sf(here(build.dir, "fires", "viirs_with_region.gpkg"))

viirs_distances <- map_dfr(countries, ~compute_distances(viirs_fires, .x))

saveRDS(viirs_distances, here(build.dir, "fires", "viirs_with_distances.rds"))
