setwd("C:/Users/iddo2/Documents/thesis data/data")
## values are from 2013

# harv <- fread("crops from harvard dataverse/spam2010v2r0_global_harv_area/spam2010V2r0_global_H_TA.csv") %>% # _TA	all technologies together, ie complete crop
#   filter(name_cntr=="Thailand") %>% 
#   select(-crea_date, -year_data, -prod_level, -rec_type, -tech_type, -cell5m, -source) 
# 
# yield <- fread("crops from harvard dataverse/spam2010v2r0_global_yield/spam2010V2r0_global_Y_TA.csv") %>%  # _TA	all technologies together, ie complete crop
#   filter(name_cntr=="Thailand") %>% 
#   select(-crea_date, -year_data, -prod_level, -rec_type, -tech_type, -cell5m, -source) 

prod <- fread("crops from harvard dataverse/spam2010v2r0_global_prod/spam2010V2r0_global_P_TA.csv") %>%  # _TA	all technologies together, ie complete crop
  filter(name_cntr %in% c("Thailand", "Lao People's Democratic Republ", "Myanmar")) %>% 
  select(-crea_date, -year_data, -prod_level, -rec_type, -tech_type, -cell5m, -source) 

# essentially to remove some numeric / irrelevant vars

prod_sf <- prod %>% st_as_sf(coords=c("x","y"))

# Create a new variable reflecting the crop with the highest value
# first get all the cols that reflect crops
crop_cols <- grep("_a$", names(prod_sf), value = TRUE)

prod_sf <- prod_sf %>%
  rowwise() %>% 
  mutate(max_crop = gsub("_a$", "", crop_cols[which.max(c_across(all_of(crop_cols)))])) %>% # get the max crop and remove _a 
  ungroup() %>%
  rename_with(~ gsub("_a$", "", .), ends_with("_a")) %>%  # remove the _a for all columns
  select(-c("iso3", "unit", "name_cntr", "name_adm1", "name_adm2")) 
  #st_set_crs(st_crs(modis_samp))

rm(prod)

# plot
ggplot(prod_sf) + geom_sf(aes(color = max_crop)) + scale_colour_viridis_d()

prod_top <- prod_sf %>% 
  select(max_crop)

st_write(prod_top, "2. intermediate/top_crop_grid.gpkg")

# prices_long_for_join <- prices_long %>% 
#   rename(year = Year, month = Month, crop_for_p = Crop) %>% 
#   select(year, month, Price, crop_for_p)

modis_samp <- modis %>% 
  filter(name %in% northern_regions_en) %>% 
  #sample_n(1000) %>% 
  st_join(prod_top, join = st_nearest_feature) %>% 
  mutate(
    crop_for_p = case_when(
      max_crop == "rice" ~ "Rice", # also have prices for glutinous, use rice prices
      max_crop == "maiz" ~ "Maize",
      max_crop == "cass" ~ "Cassava",
      TRUE ~ NA
      )
  ) %>% 
  mutate(
    Quarter_Fire = paste0("Q", ceiling(month / 3), " ", year), # Create quarter labels
    Quarter_Fire = as.yearqtr(Quarter_Fire, format = "Q%q %Y"),
    Price_Quarter = as.yearqtr(paste(as.numeric(substr(Quarter_Fire, 1, 4)) - 1, "Q4"))
  ) %>% 
  left_join(prices_long_q %>% 
              rename(Price_Quarter = Quarter, crop_for_p = Crop), by = c("Price_Quarter", "crop_for_p")) 

modis_samp <- modis_samp %>% 
  mutate(
    Price_bracket = case_when(
      Price > 95 ~ ">95",
      Price > 90 & Price <= 95 ~ "90-95",
      Price > 85 & Price <= 90 ~ "85-90",
      Price < 85 ~ "<85",
      TRUE ~ NA
    )
  )

ggplot(modis_samp %>% filter(year > 2015)) + geom_sf(aes(color = Price_backet)) + scale_colour_viridis_d()
