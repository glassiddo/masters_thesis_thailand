library(ncdf4)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(exactextractr)
library(ggplot2)

setwd("C:/Users/iddo2/Documents/thesis data")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# Load administrative boundaries
admin_units <- read_sf("data/thai admin units/admin3_simplified_en.shp") %>%
  filter(ADM1_EN %in% northern_regions_en) %>% 
  mutate(id = row_number())

# Set directory for NetCDF files
nc_dir <- "C:/Users/iddo2/Downloads/CROPGRIDSv1.08_NC_maps"
setwd(nc_dir)

# List all NetCDF files in the folder
nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)

# Initialize an empty list to store results
crop_results <- list()

# Loop through each NetCDF file
for (nc_file in nc_files[1:3]) {
  crop_name <- tools::file_path_sans_ext(basename(nc_file))  # Extract filename without extension
  cat("Processing:", crop_name, "\n")
  
  #if polygons
  #crop_raster <- rast(nc_file)
  #crop_values <- exact_extract(crop_raster, admin_units, 'mean', stack_apply = FALSE)
  
  #if points
  crop_raster <- stack(nc_file)
  crop_values <- extract(crop_raster, modis, df = T)
  
  crop_df <- crop_values
  crop_df$crop <- crop_name
  
  # Append to results list
  crop_results[[crop_name]] <- crop_df
}


### MAYBE SUM ALL OF THOSE TO GET HOW MANY HA ARE CROPS - THEN GET SHARE OF CROP PER CELL AND USE MINIMUM TO ESTABLISH AS AGRICULTURAL 
# Combine all results into a single data frame
all_crops_df <- bind_rows(crop_results) %>%
  mutate(crop = gsub("CROPGRIDSv1.08_", "", crop)) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = crop,
    values_from = layer
  )

zero_cols <- sapply(all_crops_df, function(x) all(x < 100 | is.na(x)))
all_crops_df <- all_crops_df[, c(TRUE, !zero_cols[-1])]

top_crop_indices <- max.col(all_crops_df[-1], ties.method = "first")
top_crop_names <- names(all_crops_df)[-1][top_crop_indices]

all_crops_df <- all_crops_df %>%
  select(ID) %>%
  mutate(top_crop = top_crop_names)

rm(zero_cols, top_crop_indices, top_crop_names, crop_raster, 
   crop_results, crop_values, crop_df, nc_file, nc_files, nc_dir, crop_name)

a <- a %>%
  filter(rowSums(select(., -ID) >= 200) > 0)

## Merge with admin units
# admin_units_with_crops <- admin_units %>% 
#   left_join(all_crops_df, by = "id")

# MERGE WITH FIRES
modis <- modis %>% 
  mutate(ID = row_number()) %>% 
  left_join(all_crops_df, by = "ID") %>% 
  select(-ID)

ggplot() + geom_sf(data = modis %>% filter(year(ACQ_DATE) == 2019), aes(color = top_crop)) + 
  scale_color_viridis_d(option = "C")

modis_nogeom <- modis %>% 
  st_set_geometry(NULL) %>% 
  mutate(land_use_cat = case_when(
    land_use_cat == "crops" ~ "crops",
    land_use_cat == "forests" ~ "forests",
    TRUE ~ "other"
    )
  ) %>% 
  filter(land_use_cat %in% c("crops", "forests", "other")) %>%
  group_by(admin, name, ACQ_DATE, top_crop, land_use_cat) %>% 
  summarise(n_fires_modis = n()) %>% 
  pivot_wider(
    id_cols = c(ACQ_DATE, admin, name),
    names_from = c(top_crop, land_use_cat),
    values_from = n_fires_modis,
    values_fill = 0
  ) %>% 
  rename(date = ACQ_DATE)

# # Save the results
# write.csv(admin_units_with_crops, "admin_units_with_crops.csv", row.names = FALSE)
# 
# cat("Processing completed. Results saved to admin_units_with_crops.csv\n")

#ggplot(admin_units_with_crops) + geom_sf(aes(fill = maize_value.mean.croparea))

nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)

# Initialize modis data (assuming it's already loaded)
# If modis is not yet defined, load it before running this script
# Example: modis <- read_sf("path/to/modis_shapefile.shp")

# Loop through all NetCDF files
for (nc_file in nc_files) {
  crop_name <- tools::file_path_sans_ext(basename(nc_file))  # Extract filename without extension
  cat("Processing:", crop_name, "\n")
  
  # Load raster data
  crop_raster <- stack(nc_file)
  
  # Extract values for modis regions
  crop_values <- extract(crop_raster, modis, df = TRUE)
  
  # Rename extracted variable (assuming only one layer in each file)
  names(crop_values)[2] <- crop_name
  
  # Bind extracted values to modis
  modis <- cbind(modis, crop_values[, -1])  # Remove "ID" column
  
}

# Filter for Chiang Mai
modis_with_crops1 <- modis %>% filter(ADM1_EN == "Chiang Mai")

# Plot results
ggplot() + 
  geom_sf(data = modis_with_crops1, aes(color = `crop_values....1..101`))

cat("Processing completed.\n")

