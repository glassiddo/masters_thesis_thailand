rm(list=ls())
pacman::p_load(
  dplyr, haven, tidyverse, ggplot2, 
  sf, # spatial analysis
  rnaturalearth, rnaturalearthdata # country/continent maps
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

last_year <- 2024 # last year for analysis (latest date available in year)

### create grid -----
thailand <- ne_countries(scale = "medium", returnclass = "sf", country = "Thailand")
myanmar <- ne_countries(scale = "medium", returnclass = "sf", country = "Myanmar")
laos <- ne_countries(scale = "medium", returnclass = "sf", country = "Laos")
area <- rbind(thailand, myanmar, laos) %>% # ignores borders
  st_union() %>% 
  st_sf() 

bbox_coords <- matrix(c(
  95.0, 23.0,
  95.0, 13.0,
  106.0, 13.0,
  106.0, 23.0,
  95.0, 23.0  # Close the polygon by repeating first point
  ), 
  ncol = 2, byrow = TRUE
  )

# Create a polygon from the coordinates
bbox_poly <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = 4326)  

bounded_area <- st_intersection(area, st_as_sf(bbox_poly))
print(st_area(bounded_area))
print(st_area(area))

generate_grid <- function(cell_size) {
  large_grid <- st_make_grid(bounded_area, 
                             cellsize = cell_size, 
                             what = "polygons") %>% 
    st_sf() 
  
  grid <- st_intersection(large_grid, bounded_area) %>% # only take land
    st_transform(bounded_area, crs = "EPSG:32647") %>% 
    mutate(
      gid = row_number(),
      area_grid = st_area(.) %>% as.numeric()
    )
  return(grid)
}

grid005 <- generate_grid(cell_size = 0.05)
grid01 <- generate_grid(cell_size = 0.1)
grid025 <- generate_grid(cell_size = 0.25)
grid05 <- generate_grid(cell_size = 0.5)

ggplot() +
  geom_sf(data = grid01, color = "black", fill = NA, size = 0.5, linetype = "dashed") +
  labs(title = "Grid") +
  theme_minimal()
