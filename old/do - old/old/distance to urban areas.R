setwd("C://users/iddo2/Documents/thesis data/data/urban areas GHS SMOD")
library(terra)

## load rasters - downloads from GHS SMOD for 2010. only download four 
## for example the southwest of thailand is lacking probably

ras1 <- terra::rast("GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0_R7_C28.tif")
ras2 <- terra::rast("GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0_R7_C29.tif")
ras3 <- terra::rast("GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0_R8_C28.tif")
ras4 <- terra::rast("GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0_R8_C29.tif")

# merge
ras_merged <- terra::merge(ras1, ras2, ras3, ras4)

# plot(ras_merged)

# Then reproject the merged raster
ras_merged_projected <- terra::project(ras_merged, "EPSG:4326")

# create a mask where 1 if large urban (30)
# could be similarly done for other values but takes a lot of time to run
# so in addition will take the respective value for each place (equivalent to nearest feature?)
# probably reasonable assumption that distance from large urban centre 
# is a good proxy for smaller urban areas
urban_areas_mask <- ras_merged == 30 
urban_areas_mask <- ras_merged > 20

# convert to polygons. dissolve = true cause merging neighbouring cells. only keep urban
urban_polygons <- terra::as.polygons(
  urban_areas_mask, dissolve = TRUE
  ) %>% 
  st_as_sf() %>% 
  st_transform(crs = "EPSG:4326") %>% 
  rename(large_urban = GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0_R7_C28) %>% 
  filter(large_urban == 1) 

# get the relevant admin regions - adm1 in full modis viirs data works for that
adm1 <- adm1

# split the urban areas by country as i want distance to urban within the same country
urban_by_adm <- st_intersection(urban_polygons, adm1)
ggplot(data = urban_by_adm, aes(fill = name)) + geom_sf()
urban_polygons_simp <- urban_by_adm %>% 
  st_simplify(dTolerance = 3000)
ggplot(data = urban_polygons_simp, aes(fill = name)) + geom_sf() + theme(legend.position = 'none')

# function to compute distance to urban areas
compute_urban_distances <- function(fires, urban_areas) {
  fires_with_urban <- fires %>%
    #left_join(st_drop_geometry(urban_areas), by = "name") %>%  # Join to get corresponding urban areas
    rowwise() %>%
    mutate(
      distance_to_urban = ifelse(
        !is.na(name),  # Ensure the fire location has a valid administrative 
        as.numeric(st_distance(geometry, urban_areas$geometry[urban_areas$name == name])) 
        / 1000, # in km 
        NA_real_
      ) 
    ) %>%
    ungroup()
  
  return(fires_with_urban)
}

### compute the nearest feature for each point. doing it per country would take a lot of time
### so instead just taking the nearest value (?) using terra package while keeping it as raster

modis_vec <- vect(modis)
extracted_values <- extract(ras_merged_projected, modis_vec)
result <- cbind(modis, extracted_values) %>% 
  st_set_geometry(NULL) %>% 
  select(fire_id, tail(last_col())) %>% 
  rename(label_urban = tail(last_col()))

rm(ras1, ras2, ras3, ras4, modis_vec, ras_merged, ras_merged_projected, 
   extracted_values, urban_polygons, urban_areas_mask)

# use the function
start.time <- Sys.time()

modis_flt <- compute_urban_distances(modis %>% 
                                       filter(year > 2014) %>% 
                                       sample_n(20000)
                                       #filter(fire_id < 30000)
                                     , urban_polygons_simp) %>% 
  left_join(result, by = "fire_id")

end.time <- Sys.time()

time.taken <- end.time - start.time
time.taken

modis_flt <- modis_flt %>% 
  group_by(name) %>%
  mutate(quantile = ntile(distance_to_urban, n = 5))  # n = 4 for quantiles
  ungroup()

ggplot() + 
  geom_sf(data = adm1 %>% filter(admin == "Thailand")) +
  geom_sf(data = modis_flt, aes(color = quantile)) +
  scale_color_viridis_c() 
  theme(legend.position = "none")

ggplot(data = modis_flt, aes(x = label_urban, y = distance_to_urban)) + 
  geom_point() + 
  geom_smooth()

