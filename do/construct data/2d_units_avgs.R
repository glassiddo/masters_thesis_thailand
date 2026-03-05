source("code/setup.R")

## get some avg values for several variables in each admin unit
## e.g. the avg share of pixels that are classified as crops in each admin unit

# load admin 1 units
admin_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp"))

# load raster data
t_raster <- rast(here(build.dir, "SEA_combined_layers.tif"))

# process crops to get the total agricultural production -----

prod_sf <- fread(here(raw.dir, "crops", "spam2010v2r0_global_prod", "spam2010V2r0_global_P_TA.csv")) %>%  
  filter(name_cntr %in% c("Thailand", "Lao People's Democratic Republ", "Myanmar")) %>% 
  select(-alloc_key, -crea_date, -year_data, -prod_level, -rec_type, -tech_type, -cell5m, -source) %>% 
  st_as_sf(coords=c("x","y")) %>% 
  rename_with(~ gsub("_a$", "", .), ends_with("_a")) %>%  # remove the _a for all columns
  select(-c("iso3", "unit", "name_cntr", "name_adm1", "name_adm2")) %>% 
  mutate(
    total_prod = rowSums(select_if(st_drop_geometry(.), is.numeric), na.rm = TRUE),
  ) %>% 
  select(total_prod) %>% 
  st_set_crs(st_crs("EPSG:4326")) # might need to do everything in another crs

# join crop data with admin units
admin_units <- admin_units %>% 
  st_join(prod_sf, join = st_nearest_feature)

# extract values from raster and compute the mean for each admin unit 
tree_share   <- exact_extract(t_raster$trees, admin_units, 'mean')
crop_share   <- exact_extract(t_raster$crops, admin_units, 'mean')
urban_share  <- exact_extract(t_raster$urban_binary, admin_units, 'mean')
avg_elev     <- exact_extract(t_raster$elevation_mean, admin_units, 'mean')
  
admin_units$tree_share   <- tree_share
admin_units$crop_share   <- crop_share
admin_units$urban_share  <- urban_share
admin_units$avg_elevation <- avg_elev

admin_units <- admin_units %>% 
  st_drop_geometry() %>% 
  select(adm1_id, urban_share, crop_share, tree_share, avg_elevation)

fwrite(admin_units, here(build.dir, "units1_with_avgs.csv"))

