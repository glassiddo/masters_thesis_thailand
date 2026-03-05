rm(list=ls())
#dev.off()
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  dtplyr
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data/")

#### clean data and match to grid ------
era_temp <- rast("1. raw/wind etc/era5_temp_full.tif")
era_surf <- rast("1. raw/wind etc/era5_surface_full.tif")  
era_precip <- rast("1. raw/wind etc/era5_precip_full.tif")
era_uWind <- rast("1. raw/wind etc/era5_uWind_full.tif")
era_vWind <- rast("1. raw/wind etc/era5_vWind_full.tif")


# Get dates (assuming same for all variables)
clean_date_names <- function(date_names) {
  # Extract just the 8-digit date part from the beginning
  clean_dates <- str_extract(date_names, "^\\d{8}")
  return(clean_dates)
}

# Function to convert raster to long format dataframe
raster_to_df <- function(rast_obj, var_name) {
  # Get coordinates
  coords <- terra::xyFromCell(rast_obj, 1:ncell(rast_obj))
  
  # Get clean dates
  date_names <- names(rast_obj)
  clean_dates <- clean_date_names(date_names)
  
  # Create empty dataframe to store results
  result_df <- data.frame()
  
  # Process in chunks to manage memory
  chunk_size <- 100
  n_dates <- nlyr(rast_obj)
  n_chunks <- ceiling(n_dates / chunk_size)
  
  for(i in 1:n_chunks) {
    # Get date range for this chunk
    start_idx <- (i-1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_dates)
    
    # Extract values for this chunk
    chunk_data <- terra::extract(rast_obj[[start_idx:end_idx]], coords, cells=TRUE)
    
    # Remove the ID column
    if("ID" %in% colnames(chunk_data)) {
      chunk_data <- chunk_data[, -which(colnames(chunk_data) == "ID")]
    }
    
    # Replace original column names with clean dates
    chunk_dates <- clean_dates[start_idx:end_idx]
    colnames(chunk_data)[colnames(chunk_data) != "cell"] <- chunk_dates
    
    # Reshape to long format
    chunk_long <- chunk_data %>%
      pivot_longer(
        cols = -cell,
        names_to = "date",
        values_to = var_name
      ) %>%
      mutate(x = coords[cell, 1],
             y = coords[cell, 2])
    
    # Append to result
    result_df <- bind_rows(result_df, chunk_long)
    
    cat("Processed chunk", i, "of", n_chunks, "for", var_name, "\n")
  }
  
  # Clean up and return
  result_df <- result_df %>%
    filter(!is.na(x) & !is.na(y)) %>%
    select(x, y, date, !!var_name)
  
  return(result_df)
}

# Create individual dataframes
temp_df <- raster_to_df(era_temp, "temperature")
surf_df <- raster_to_df(era_surf, "surface_temp")
precip_df <- raster_to_df(era_precip, "precipitation")
uWind_df <- raster_to_df(era_uWind, "u_wind")
vWind_df <- raster_to_df(era_vWind, "v_wind")

# Merge all dataframes
era_combined <- temp_df %>%
  full_join(surf_df, by = c("x", "y", "date")) %>%
  full_join(precip_df, by = c("x", "y", "date")) %>%
  full_join(uWind_df, by = c("x", "y", "date")) %>%
  full_join(vWind_df, by = c("x", "y", "date"))

rm(
  temp_df, surf_df, precip_df, uWind_df, vWind_df,
  era_precip, era_surf, era_uWind, era_vWind, era_temp, 
  dates, raster_to_df, clean_date_names
  )

# create geometry for the combined dataset - resolution is 0.25*0.25 as the original era5
create_grid_polygons <- function(x, y, res_x = 0.25, res_y = 0.25) {
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

# create grid to get unique ids
grid_sf <- era_combined %>%
  select(x, y) %>%
  distinct() %>% 
  mutate(grid_id = row_number()) %>%
  rowwise() %>%
  mutate(geometry = list(create_grid_polygons(x, y))) %>%
  st_as_sf(crs = 4326)

# Join with grid geometry
era_combined <- era_combined %>%
  left_join(grid_sf %>% st_set_geometry(NULL), by = c("x", "y")) %>%
  #filter(!is.na(grid_id)) %>% 
  # Convert date column to proper Date objects
  mutate(date = as.Date(date, format = "%Y%m%d"))

#### summarise to units ------------
# Function to summarize climate data to administrative units
# Function to summarize climate data to administrative units
summarize_to_admin_units <- function(admin_units, admin_id_col, grid_sf, era_combined) {
  
  # Find which grid cells intersect with each administrative unit
  grid_admin_intersects <- st_intersects(admin_units, grid_sf)
  
  # Create mapping between admin units and grid cells
  mapping_list <- lapply(1:nrow(admin_units), function(admin_idx) {
    admin_id <- admin_units[[admin_id_col]][admin_idx]
    grid_indices_in_admin <- grid_admin_intersects[[admin_idx]]
    
    # Check if there are any intersecting grid cells
    if (length(grid_indices_in_admin) == 0) {
      return(NULL)
    }
    
    # Get the actual grid IDs
    grid_ids_in_admin <- grid_sf$grid_id[grid_indices_in_admin]
    
    # Create data frame with proper replication of admin_id
    return(data.frame(
      admin_id = rep(admin_id, length(grid_ids_in_admin)), 
      grid_id = grid_ids_in_admin
    ))
  })
  
  # Combine all mappings
  grid_to_admin_mapping <- dplyr::bind_rows(mapping_list)
  
  # Merge climate data with admin mapping
  admin_climate_merged <- era_combined %>%
    inner_join(grid_to_admin_mapping, by = "grid_id", relationship = "many-to-many") %>% 
    as.data.table()
  
  # Calculate averages by admin unit and date
  admin_climate_results <- admin_climate_merged[, .(
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_surface_temp = mean(surface_temp, na.rm = TRUE),
    avg_precipitation = mean(precipitation, na.rm = TRUE),
    avg_u_wind = mean(u_wind, na.rm = TRUE),
    avg_v_wind = mean(v_wind, na.rm = TRUE)
  ), by = .(admin_id, date)]
  
  return(admin_climate_results)
}

# Load administrative units
all_units_adm2 <- read_sf("2. intermediate/units/adm2_units.shp") 
all_units_adm1 <- read_sf("2. intermediate/units/adm1_units.shp") 
# Apply function to both administrative levels
admin2_climate_results <- summarize_to_admin_units(
  admin_units = all_units_adm2,
  admin_id_col = "adm2_id",
  grid_sf = grid_sf,
  era_combined = era_combined
)

admin1_climate_results <- summarize_to_admin_units(
  admin_units = all_units_adm1,
  admin_id_col = "adm1_id",  # assuming the column name is "adm1_id"
  grid_sf = grid_sf,
  era_combined = era_combined
)

# Clean up intermediate objects after both runs
rm(era_combined, grid_sf)


admin1_climate_results <- admin1_climate_results %>% 
  as.data.frame() %>% 
  filter(!is.na(avg_precipitation)) %>% 
  rename(adm1_id = admin_id) %>% 
  left_join(all_units_adm1 %>% st_set_geometry(NULL), by = "adm1_id")

admin2_climate_results <- admin2_climate_results %>% 
  as.data.frame() %>% 
  filter(!is.na(avg_precipitation)) %>% 
  rename(adm2_id = admin_id) %>% 
  left_join(all_units_adm2 %>% st_set_geometry(NULL), by = "adm2_id")

gc()

fwrite(admin1_climate_results, "2. intermediate/adm1_climatic_date.csv")
fwrite(admin2_climate_results, "2. intermediate/adm2_climatic_date.csv")
