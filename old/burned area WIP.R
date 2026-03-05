library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

### need to also add land use and confidence of fires

### 10-30 is crop land, 40 is possible crop land
#https://developers.google.com/earth-engine/datasets/catalog/ESA_CCI_FireCCI_5_1#bands
setwd("C:/Users/iddo2/Documents/thesis data/data/")

### rerun the code 

### this includes areas in the countries (myanmar/laos/thailand)
### which arent included in the actual relevant admin units i kept

# years <- 2012:2020
# for (y in years){
#   # read every dataframe like the following, while adjusting the namein the end
# }

years <- 2012:2020
folder_path <- "1. raw/burned area"

# Read all shapefiles, add year column
burned_list <- lapply(years, function(y) {
  sf_obj <- read_sf(file.path(folder_path, paste0("burned_areas_details_", y, ".shp")))
  sf_obj$year <- y
  return(sf_obj)
})

burned_all <- bind_rows(burned_list) %>% 
  mutate(
    date = lubridate::ymd(paste0(year, "01-01")),
    date = `yday<-`(date, BurnDate)
  ) %>% 
  dplyr::select(-year, -BurnDate) %>% 
  mutate(
    burn_id = row_number(),
    land_use_type = case_when(
      LandCover %in% c("10", "20", "30") ~ "crop", # could also use 40
      LandCover %in% c("50", "60", "70", "80", "90") ~ "tree", # could also use 100
      TRUE ~ "other"
    )
  ) 

all_units_adm2 <- read_sf("2. intermediate/units/adm2_units.shp") 

dates <- c(
  seq(as.Date("2012-01-01"), as.Date("2020-12-31"), by="day")
)

## intersect burned areas with admin units ----
#admin_burn_intersect <- st_intersection(burned, all_units_adm2) 

# faster way
within_indices <- st_within(burned_all, all_units_adm2)
burned_all$adm2_id <- sapply(within_indices, function(x) if(length(x) > 0) x[1] else NA)
  
# summarise
admin_burn_nogeom <- burned_all %>% 
  st_drop_geometry() 

admin_burn_nogeom <- admin_burn_nogeom %>% 
  group_by(
    date, adm2_id
  ) %>% 
  summarise(
    n_burns = n(),
    sum_burns = sum(sum, na.rm = T),
    n_burns_crop = sum(land_use_type == "crop"),
    n_burns_tree = sum(land_use_type == "tree"),
    n_burns_mid = sum(Confidence > 12),
    n_burns_high = sum(Confidence > 25)
    ) %>% 
  ungroup()
  
### grid  
grid <- expand.grid(
  date = dates,
  adm2_id = unique(all_units_adm2$adm2_id),
  stringsAsFactors = FALSE
  ) %>% 
  left_join(
    admin_burn_nogeom, by = c("adm2_id", "date")
  ) %>% 
  left_join(
    all_units_adm2 %>% st_drop_geometry(), by = "adm2_id"
  ) %>% 
  mutate(
    n_burns = replace_na(n_burns, 0),
    sum_burns = replace_na(sum_burns, 0),
    n_burns_crop = replace_na(n_burns_crop, 0),
    n_burns_tree = replace_na(n_burns_tree, 0),
    n_burns_mid = replace_na(n_burns_mid, 0),
    n_burns_high = replace_na(n_burns_high, 0)
  )

grid_burned_adm1 <- grid_burned %>% 
  filter(ADM0_EN != "Laos") %>% 
  group_by(ADM1_EN, date) %>%
  summarise(
    n_burns = sum(n_burns),
    sum_burns = sum(sum_burns),
    n_burns_crop = sum(n_burns_crop)
  ) %>% 
  ungroup()

grid_adm1 <- grid_adm1 %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(grid_burned_adm1, by = c("date", "ADM1_EN"))

rm(grid_burned_adm1)

event_model <- feols(
  n_burns_crop ~ i(event_time_years, treated_unit, ref = "0") 
  + avg_temperature + avg_surface_temp + avg_precipitation
  + avg_u_wind + avg_v_wind 
  | ADM1_EN + date,
  cluster = ~ADM1_EN,
  data = grid_adm1 #, weights = ~weights,
)

iplot(event_model, 
      xlab = "Years relative to treatment",
      main = "Event Study of Treatment on Fires")

