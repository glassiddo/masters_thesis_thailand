rm(list=ls())
setwd("C:/Users/iddo2/Documents/thesis data/data")
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table, 
  lubridate, ggplot2, readxl,  fixest,
  geodata, spData, sf, terra, maps, sp, raster # spatial analysis
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

wdpa_sf_points1 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-points(1).shp")
wdpa_sf_points2 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-points.shp")
wdpa_sf_points3 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-points(0).shp")
wdpa_sf_polygons1 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-polygons.shp")
wdpa_sf_polygons2 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-polygons(1).shp")
wdpa_sf_polygons3 <- read_sf("1. raw/WDPA/WDPA_WDOECM_Dec2024_Public_AS_shp-polygons(2).shp")

wdpa_sf_points <- bind_rows(wdpa_sf_points1, wdpa_sf_points2, wdpa_sf_points3)
wdpa_sf_polygons <- bind_rows(wdpa_sf_polygons1, wdpa_sf_polygons2, wdpa_sf_polygons3)

target_countries <- c("THA", "MMR", "LAO")

wdpa_sf_points <- wdpa_sf_points %>%
  filter(grepl(paste(target_countries, collapse="|"), ISO3))

wdpa_sf_polygons <- wdpa_sf_polygons %>%
  filter(grepl(paste(target_countries, collapse="|"), ISO3))

rm(wdpa_sf_points1, wdpa_sf_points2, wdpa_sf_points3, 
   wdpa_sf_polygons1, wdpa_sf_polygons2, wdpa_sf_polygons3)

# Create buffer zones around points using st_buffer depending on their reported area

wdpa_sf_points <- wdpa_sf_points %>%
  mutate(buffer_radius_km = sqrt(REP_AREA / pi)) 

wdpa_sf_points_buffered <- st_buffer(
  wdpa_sf_points, 
  dist = wdpa_sf_points$buffer_radius_km * 1000
  ) %>% 
  # converting km to meters 
  select(
    -buffer_radius_km
  )

wdpa_sf <- wdpa_sf_polygons
# rbind(wdpa_sf_polygons, wdpa_sf_points) # ignoring buffered around points!!
# seems like the buffering overestimates massively

rm(wdpa_sf_points, wdpa_sf_points_buffered, wdpa_sf_polygons)


