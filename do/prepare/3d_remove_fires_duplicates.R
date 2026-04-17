source("do/setup.R")

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
  fires_lazy <- fires_data |> 
    # drop geometry, convert to data table format (dtplyr)
    st_drop_geometry() |> 
    lazy_dt() |> 
    ### get ids for coordinates, 
    ### round based on the grid size to get stuff in the same parameter
    ### not gonna work perfectly but rough approximation
    ### especially as its robustness and not main specification, 
    ### could survive with some inaccuracy and missed repeated fires
    mutate(
      grid_x = round(LONGITUDE / grid_size),
      grid_y = round(LATITUDE / grid_size)
    ) |> 
    mutate(
      ### set an id for spatial cluster (each 1km bounding box)
      spatial_cluster_id = as.numeric(as.factor(paste(grid_x, grid_y))),
      ### and one for spatial x day combination
      spatial_day_cluster_id = as.numeric(as.factor(paste(date, grid_x, grid_y)))
    ) |> 
    ### now recognize which ones were taken in the same spatial cluster in repeated days
    arrange(spatial_cluster_id, date) |> 
    mutate(
      # compute the gap from the last fire in the same spatial cluster
      # the idea is to recognize duplicates
      # so if there's only one fire in the entire period in the spatial cluster
      # day_gap = 1, temporal group is 0 
      # so there's a single spatial_temporal_cluster_id overall
      # if theres more than one fire, spatial_temporal_cluster_id is different
      # except for cases where there are repeated fires (including fires in the same day)
      day_gap = c(1, diff(date)),
      temporal_group = cumsum(day_gap > 1),
      .by = spatial_cluster_id
    ) |> 
    mutate(
      spatial_temporal_cluster_id = 
        as.numeric(as.factor(paste(spatial_cluster_id, temporal_group)))
    ) 

  col_name_dup <- paste0(fire_source, "_no_duplicates")
  col_name_rep <- paste0(fire_source, "_no_repeated")
  
  without_single_day_fires <- fires_lazy |> 
    group_by(spatial_day_cluster_id) |> 
    filter(n() == 1) |> 
    ungroup() |> 
    summarise(
      !!col_name_dup := n(),
      .by = c(adm1_id, date)
    ) |> 
    as_tibble()
  
  without_repeated_fires <- fires_lazy |> 
    group_by(spatial_temporal_cluster_id) |> 
    filter(n() == 1) |> 
    ungroup() |> 
    summarise(
      !!col_name_rep := n(),
      .by = c(adm1_id, date)
    ) |> 
    as_tibble()
  
  return(list(
    without_single_day_fires = without_single_day_fires,
    without_repeated_fires = without_repeated_fires
  ))
}

### run for modis -------
modis_fires <- st_read(here(build.dir, "fires", "modis_with_region.gpkg"))
modis_filtered <- remove_fire_duplicates_repeated(modis_fires, "modis")

rm(modis_fires)
gc()

saveRDS(modis_filtered$without_single_day_fires, here(build.dir, "fires", "modis_no_duplicate.rds"))
saveRDS(modis_filtered$without_repeated_fires, here(build.dir, "fires", "modis_no_repeated.rds"))

rm(modis_filtered)
gc()

### run for viirs -------
viirs_fires <- st_read(here(build.dir, "fires", "viirs_with_region.gpkg"))
viirs_filtered <- remove_fire_duplicates_repeated(viirs_fires, "viirs")

saveRDS(viirs_filtered$without_single_day_fires, here(build.dir, "fires", "viirs_no_duplicate.rds"))
saveRDS(viirs_filtered$without_repeated_fires, here(build.dir, "fires", "viirs_no_repeated.rds"))
