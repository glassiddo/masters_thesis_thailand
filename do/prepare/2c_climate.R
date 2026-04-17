source("do/setup.R")

## this takes era5 climatic variables. 
## it gets values from the raster for each grid cell
## then for each admin unit in my data, check for intersect with the cells
## and calculate the average for each month/year for each admin unit 
## based on all cells that intersect
## (not accounting for share of intersection, to simplify - could be changed)

process_raster <- function(file_path, value_name) {
  rast(file_path) |> 
    as.data.frame(xy = TRUE) |> 
    pivot_longer(
      cols = -c(x, y), # select all columns except for x and y
      names_to = "layer",
      values_to = value_name  
    ) |> 
    filter(!is.na(.data[[value_name]])) |> # .data[[]] for dynamic column reference
    mutate(
      year = as.numeric(paste0("20", str_extract(layer, "\\d{2}(?=_\\d{2}$)" ))),
      month = as.numeric(str_extract(layer, "\\d{2}$")), 
      .keep = "unused" 
    ) 
}

v_wind <- process_raster("data/raw/climatic/ERA5_v_wind_2004_2012_MultiBand.tif", "v_wind")
u_wind <- process_raster("data/raw/climatic/ERA5_u_wind_2004_2012_MultiBand.tif", "u_wind")
skin_temp <- process_raster("data/raw/climatic/ERA5_skin_temp_2004_2012_MultiBand.tif", "skin_temp")
temper <- process_raster("data/raw/climatic/ERA5_temperature_2004_2012_MultiBand.tif", "temperature")
precip <- process_raster("data/raw/climatic/ERA5_precipitation_2004_2012_MultiBand.tif", "precipitation")
dew_temp <- process_raster("data/raw/climatic/ERA5_dew_temp_2004_2012_MultiBand.tif", "dew_temp")

climatic <- v_wind |> 
  full_join(u_wind, by = c("x", "y", "year", "month")) |> 
  full_join(skin_temp, by = c("x", "y", "year", "month")) |> 
  full_join(temper, by = c("x", "y", "year", "month")) |> 
  full_join(precip, by = c("x", "y", "year", "month")) |> 
  full_join(dew_temp, by = c("x", "y", "year", "month")) 

create_grid_polygons <- function(x, y, res_x = 0.1, res_y = 0.1) {
  xmin <- x - res_x/2
  xmax <- x + res_x/2
  ymin <- y - res_y/2
  ymax <- y + res_y/2
  
  geom <- st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
  
  return(geom)
}

rm(temper, u_wind, v_wind, precip, skin_temp, dew_temp)

grid_sf <- climatic |>
  select(x, y) |>
  distinct() |> 
  mutate(grid_id = row_number()) |>
  rowwise() |>
  mutate(geometry = list(create_grid_polygons(x, y))) |>
  st_as_sf(crs = 4326)

climatic <- climatic |>
  left_join(grid_sf |> st_set_geometry(NULL), by = c("x", "y")) |> 
  select(-x, -y, grid_id, year, month, everything())


summarize_to_admin_units <- function(admin_units, admin_id_col, grid_sf, era_combined) {
  
  # which grid cells intersect with each administrative unit
  grid_admin_intersects <- st_intersects(admin_units, grid_sf)
  
  # mapping between admin units and grid cells
  mapping_list <- lapply(1:nrow(admin_units), function(admin_idx) {
    admin_id <- admin_units[[admin_id_col]][admin_idx]
    grid_indices_in_admin <- grid_admin_intersects[[admin_idx]]
    
    # actual grid IDs
    grid_ids_in_admin <- grid_sf$grid_id[grid_indices_in_admin]
    
    # data frame with proper replication of admin_id
    return(data.frame(
      admin_id = rep(admin_id, length(grid_ids_in_admin)), 
      grid_id = grid_ids_in_admin
    ))
  })
  
  # combine all
  grid_to_admin_mapping <- bind_rows(mapping_list)
  # merge with the admin mapping
  admin_climate_merged <- era_combined |>
    inner_join(grid_to_admin_mapping, by = "grid_id", relationship = "many-to-many")
  
  # convert to dt for speed
  admin_climate_merged <- admin_climate_merged |>
    as.data.table()
  
  # calculate averages by year, month, and admin id
  admin_climate_results <- admin_climate_merged[, .(
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_skin_temp = mean(skin_temp, na.rm = TRUE),
    avg_precipitation = mean(precipitation, na.rm = TRUE),
    avg_u_wind = mean(u_wind, na.rm = TRUE),
    avg_v_wind = mean(v_wind, na.rm = TRUE),
    avg_dew_temp = mean(dew_temp, na.rm = TRUE)
  ), by = .(admin_id, year, month)]
  
  return(admin_climate_results)
}

all_units_adm1 <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  select(adm1_id)
  
admin1_climate_results <- summarize_to_admin_units(
  admin_units = all_units_adm1,
  admin_id_col = "adm1_id",
  grid_sf = grid_sf,
  era_combined = climatic
)

saveRDS(admin1_climate_results, here(build.dir, "climate", "adm1_climatic_date.rds"))

