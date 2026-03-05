setwd("C:/Users/iddo2/Documents/thesis data/data")
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table, 
  lubridate, ggplot2, readxl,  fixest,
  lattice,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

# Forest protection unit boundaries
boundaries <- read_sf("1. raw/thai forest control units/408_Polygon_join.shp")
# Area of responsibility of the forest protection unit
area_resp <- read_sf("1. raw/thai forest control units/Unit_Erase_DNP_Dissolve.shp") 
# Locations of the forest protection unit
unit47 <- read_sf("1. raw/thai forest control units/Unit_Zone47_Point_Edit.shp") 
unit48 <- read_sf("1. raw/thai forest control units/Unit_Zone48_Point_Edit.shp")
# unit48 are units that are located in the east of thailand, so use UTM zone 48N
unit47 <- unit47 %>%
  rename(
    No_ = No,
    Province = Provine,
    E = X,
    N = Y
  )
unit48 <- unit48 %>% st_transform(st_crs(unit47)) 
# converting all to EPSG:32647 (differences are marginal probably - maybe test)
units <- bind_rows(unit47, unit48) %>% 
  rename(
    District = Amphoe,
    Subdistrict = Tambon
    )
rm(unit47, unit48)

#st_write(units, "thai forest control units/units.shp")

ggplot() +
  geom_sf(data = boundaries, aes(
    fill = No_
  )) +
  geom_sf(data = units, aes(
    color = No_
  )) +
  scale_fill_viridis_c() + # continous
  #scale_fill_viridis_d() + # discrete
  theme_minimal()
#theme(legend.position="none")

all_units_thai <- read_sf("2. intermediate/units/adm2_units.shp") %>% 
  filter(ADM0_EN == "Thailand") 

units <- units %>% 
  select(Province, District, Subdistrict, Unit_Name)

#thailand_fires <- read_sf("modis_with_region_1405.gpkg") %>% 
  filter(ADM0_EN == "Thailand") %>% 
  st_transform(st_crs(boundaries)) %>% 
  st_within(boundaries)

matched_fires <- fires_in_boundaries %>%
  rowwise() %>%
  mutate(
    matching_units = list(units[units$Unit_Name == .data$Unit_Name,]),
    Matching_Unit_Name = if(nrow(matching_units) > 0) {
      matching_units$Unit_Name[1]
    } else {
      NA_character_
    },
    Distance = if(nrow(matching_units) > 0) {
      st_distance(geometry, matching_units$geometry, by_element = TRUE)
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

units_nogeom <- units %>% st_set_geometry(NULL)
matched_fires_df <- matched_fires %>% 
  mutate(Distance_Numeric = as.numeric(Distance)/1000) %>% 
  left_join(units_nogeom, by = "Unit_Name") %>% 
  filter(Province == "ตาก")
units_filtered <- units %>% 
  filter(Province == "ตาก")
boundaries_filtered <- boundaries %>% 
  filter(PROV_NAM_T == "จ.ตาก")

ggplot(matched_fires_df) +
  geom_sf(data = boundaries_filtered, color = "black") +
  geom_sf(aes(color = Distance_Numeric), size = 2) +
  geom_sf(data = units_filtered, color = "red", shape = 4, size = 3) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Fire Locations with Distance to Matching Unit")

f <- read_sf("fires with admin units test/fire_points_with_land_cover.shp")
# dates are wrong
ggplot(f) +
  geom_sf(aes(color = LC_Type1), size = 2) +
  scale_color_viridis_c() +
  theme_minimal()
  #labs(title = "Fire Locations with Distance to Matching Unit")
