source("code/setup.R")

## remove fires taking place in the same day or in consecutive days 

### function to remove duplicates and repeated fires -------
remove_fire_duplicates_repeated <- function(fires_data, fire_source) {
  
  # define grid sizes corresponding roughly to metric distances at ~15°N latitude 
  grid_sizes <- list(
    "viirs" = 0.00336,  # approximately 375m at 18°N latitude, roughly around north thailand
    "modis" = 0.0095      # approximately 1km at 18°N
  )
  
  grid_size <- grid_sizes[[fire_source]]
  
  ### this code recognizes repeated fires - ones in the same 1km for modis (or viirs 375m)
  ### that take place in the same day or in repeated days, which might be double counted
  fires_lazy <- fires_data %>%
    # drop geometry, convert to data table format (dtplyr)
    st_drop_geometry() %>%
    lazy_dt() %>%
    ### get ids for coordinates, 
    ### round based on the grid size to get stuff in the same parameter
    ### not gonna work perfectly but rough approximation
    ### especially as its robustness and not main specification, 
    ### could survive with some inaccuracy and missed repeated fires
    mutate(
      grid_x = round(LONGITUDE / grid_size),
      grid_y = round(LATITUDE / grid_size)
    ) %>% 
    mutate(
      ### set an id for spatial cluster (each 1km bounding box)
      spatial_cluster_id = as.numeric(as.factor(paste(grid_x, grid_y))),
      ### and one for spatial x day combination
      spatial_day_cluster_id = as.numeric(as.factor(paste(date, grid_x, grid_y)))
    ) %>% 
    ### now recognize which ones were taken in the same spatial cluster in repeated days
    arrange(spatial_cluster_id, date) %>%
    group_by(spatial_cluster_id) %>%
    mutate(
      # compute the gap from the last fire in the same spatial cluster
      # the idea is to recognize duplicates
      # so if there's only one fire in the entire period in the spatial cluster
      # day_gap = 1, temporal group is 0 
      # so there's a single spatial_temporal_cluster_id overall
      # if theres more than one fire, spatial_temporal_cluster_id is different
      # except for cases where there are repeated fires (including fires in the same day)
      day_gap = c(1, diff(date)),
      temporal_group = cumsum(day_gap > 1)
    ) %>%
    ungroup() %>% 
    mutate(
      spatial_temporal_cluster_id = 
        as.numeric(as.factor(paste(spatial_cluster_id, temporal_group)))
    ) %>% 
    as_tibble()
  
  duplicates_single_fire_day <- fires_lazy %>% 
    lazy_dt() %>% 
    group_by(spatial_day_cluster_id) %>% 
    filter(n() > 1) %>% 
    select(spatial_day_cluster_id) %>% 
    distinct() %>% 
    as_tibble() %>% 
    pull()
  
  duplicates_repeated_fires <- fires_lazy %>% 
    lazy_dt() %>% 
    group_by(spatial_temporal_cluster_id) %>% 
    filter(n() > 1) %>% 
    select(spatial_temporal_cluster_id) %>% 
    distinct() %>% 
    as_tibble() %>% 
    pull()
  
  without_single_day_fires <- fires_lazy %>% 
    filter(
      !spatial_day_cluster_id %in% duplicates_single_fire_day
    )
  
  without_repeated_fires <- fires_lazy %>% 
    filter(
      !spatial_temporal_cluster_id %in% duplicates_repeated_fires
    )
  
  return(list(
    without_single_day_fires = without_single_day_fires,
    without_repeated_fires = without_repeated_fires
  ))
}

### run for modis -------
modis_fires <- st_read(here(build.dir, "modis_with_region_2206.gpkg"))
modis_filtered <- remove_fire_duplicates_repeated(modis_fires, "modis")

modis_no_duplicate <- modis_filtered$without_single_day_fires
modis_no_repeated <- modis_filtered$without_repeated_fires

fwrite(modis_no_duplicate, here(build.dir, "fires", "modis_no_duplicate_0407.csv"))
fwrite(modis_no_repeated, here(build.dir, "fires", "modis_no_repeated_0407.csv"))

### run for viirs -------
viirs_fires <- st_read(here(build.dir, "viirs_with_region_2906.gpkg"))
viirs_filtered <- remove_fire_duplicates_repeated(viirs_fires, "viirs")

viirs_no_duplicate <- viirs_filtered$without_single_day_fires
viirs_no_repeated <- viirs_filtered$without_repeated_fires

fwrite(viirs_no_duplicate, here(build.dir, "fires", "viirs_no_duplicate_0407.csv"))
fwrite(viirs_no_repeated, here(build.dir, "fires", "viirs_no_repeated_0407.csv"))
