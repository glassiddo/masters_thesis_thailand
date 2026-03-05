source("do/setup.R")

dates <- c(
  seq(as.Date("2004-01-01"), as.Date("2020-12-31"), by="day")
)

all_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) %>% 
  st_drop_geometry()

units_info <- fread(here(build.dir, "units1_with_avgs.csv"))

modis_nogeom <- fread(here(build.dir, "modis_summarised.csv"))
viirs_nogeom <- fread(here(build.dir, "viirs_summarised.csv"))

modis_no_duplicate <- fread(here(build.dir, "modis_no_duplicate.csv"))
modis_no_repeated <- fread(here(build.dir, "modis_no_repeated.csv"))
viirs_no_duplicate <- fread(here(build.dir, "viirs_no_duplicate.csv"))
viirs_no_repeated <- fread(here(build.dir, "viirs_no_repeated.csv"))

climate <- fread(here(build.dir, "adm1_climatic_date.csv"))

### combine all ----
all_units <- all_units %>%
  left_join(units_info, by = "adm1_id")

grid <- expand.grid(
  date = dates,
  adm1_id = unique(all_units$adm1_id),
  stringsAsFactors = FALSE
  ) %>%
  mutate(
    year = year(date),
    month = month(date),
    month_name = month(date, label = T),
    week = week(date),
    half_month = if_else(day(date) <= ceiling(days_in_month(date) / 2), 1, 2),
    half_month_id = (month - 1) * 2 + half_month
  ) %>%
  full_join(all_units, by = "adm1_id") %>%
  #left_join(gdp_data, by = "ADM0_EN") %>%
  left_join(modis_nogeom %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>% 
  left_join(viirs_nogeom %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>%  
  left_join(modis_no_duplicate %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>%  
  left_join(modis_no_repeated %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>%  
  left_join(viirs_no_duplicate %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>%  
  left_join(viirs_no_repeated %>% mutate(date = as.Date(date)), by = c("adm1_id", "date")) %>%  
  mutate(
    across(
      # Select all columns that came from top_crop in pivot_wider
      # This assumes all crop columns are numeric and not part of your original grid data
      where(is.numeric) & !c(adm1_id),
      ~replace_na(., 0)
      #### IT FILLS URBAN SHARES AS 0 IN UNITS WHERE THE URBAN RASTER DOESNT EXIST!!!
    )
  ) %>%
  mutate(
    treated_unit = ifelse(ADM1_EN %in% northern_regions_en, 1, 0),
    post_treatment_weak = ifelse(year > 2012, 1, 0),
    post_treatment_strong = ifelse(year > 2016, 1, 0),
    is_treated_weak = ifelse(treated_unit == 1 & post_treatment_weak == 1, 1, 0),
    is_treated_strong = ifelse(treated_unit == 1 & post_treatment_strong == 1, 1, 0),
    year_id = as.numeric(factor(year))
  ) 


### replace the pre 2012 values for viirs (empty) with modis  ------
grid <- grid %>% 
  mutate(
    viirs_fixed = ifelse(date < "2013-01-01", total_fires, total_fires_viirs),
    viirs_no_duplicate_fixed = ifelse(date < "2013-01-01", modis_no_duplicates, viirs_no_duplicates),
    viirs_no_repeated_fixed = ifelse(date < "2013-01-01", modis_no_repeated, viirs_no_repeated),
    night_fires_mixed = ifelse(date < "2013-01-01", total_fires, night_fires),
    day_fires_mixed = ifelse(date < "2013-01-01", total_fires, day_fires),
    aqua_fires_mixed = ifelse(date < "2013-01-01", total_fires, aqua_fires),
    terra_fires_mixed = ifelse(date < "2013-01-01", total_fires, terra_fires)
  ) 

### write ----

fwrite(grid, here(out.dir, "grid_1606.csv"))
