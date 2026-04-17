source("do/setup.R")
### this takes spam dataset, and for each fire spot determine what is the dominant crop there
## and in each quantile of crop production out of the country (out of fire spots), the fire spot is located in

prod_sf <- fread(here(
  raw.dir, "crops", "spam2010v2r0_global_harv_area", "spam2010V2r0_global_H_TA.csv")) |>  # _TA	all technologies together, ie complete crop
  filter(name_cntr %in% c(
    "Thailand", "Lao People's Democratic Republ", "Myanmar"
    )) |> 
  select(-alloc_key, -crea_date, -year_data, -prod_level, -rec_type, -tech_type, -cell5m, -source) |> 
  st_as_sf(coords=c("x","y")) 

# Create a new variable reflecting the crop with the highest value
# first get all the cols that reflect crops
crop_cols <- grep("_a$", names(prod_sf), value = TRUE)

prod_sf <- prod_sf |>
  rowwise() |> 
  mutate(max_crop = gsub("_a$", "", crop_cols[which.max(c_across(all_of(crop_cols)))])) |>
  ungroup() |>
  rename_with(~ gsub("_a$", "", .), ends_with("_a")) |>  # remove the _a for all columns
  select(-c("iso3", "unit", "name_cntr", "name_adm1", "name_adm2")) |> 
  mutate(
    total_prod = rowSums(pick(where(is.numeric)), na.rm = TRUE),
    crop_production_q = ntile(total_prod, 4) 
    # sum all columns
  ) |> 
  select(max_crop, crop_production_q) |> 
  st_set_crs(st_crs("EPSG:4326"))

st_write(prod_sf, here(build.dir, "top_crop_grid.gpkg"), append = FALSE)

modis_fires <- read_sf(here(build.dir, "fires", "modis_with_region.gpkg")) |> 
  select(fire_id) |> 
  st_join(prod_sf, join = st_nearest_feature) |> 
  st_drop_geometry() 

saveRDS(modis_fires, here(build.dir, "fires", "modis_with_crops.rds"))

rm(modis_fires)

viirs_fires <- read_sf(here(build.dir, "fires", "viirs_with_region.gpkg")) |> 
  select(fire_id) |> 
  st_join(prod_sf, join = st_nearest_feature) |> 
  st_drop_geometry() 

saveRDS(viirs_fires, here(build.dir, "fires", "viirs_with_crops.rds"))

rm(viirs_fires)