source("do/setup.R")

dates <- c(
  seq(as.Date("2004-01-01"), as.Date("2020-12-31"), by="day")
)

all_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  st_drop_geometry()

units_info <- readRDS(here(build.dir, "units", "units1_with_avgs.rds"))

fire_files <- c(
  "modis_summarised.rds", "viirs_summarised.rds", 
  "modis_no_duplicate.rds", "modis_no_repeated.rds", 
  "viirs_no_duplicate.rds", "viirs_no_repeated.rds"
  )

fire_data_list <- map(fire_files, ~readRDS(here(build.dir, "fires", .x)) |> 
                        mutate(date = as.Date(date)))

climate <- readRDS(here(build.dir, "adm1_climatic_date.rds"))

### combine all ----
all_units <- all_units |> 
  left_join(units_info, by = "adm1_id")

grid <- expand.grid(
  date = dates,
  adm1_id = unique(all_units$adm1_id),
  stringsAsFactors = FALSE
  ) |> 
  mutate(
    year = year(date),
    month = month(date),
    month_name = month(date, label = T),
    week = week(date),
    half_month = ifelse(day(date) <= ceiling(days_in_month(date) / 2), 1, 2),
    half_month_id = (month - 1) * 2 + half_month
  ) |> 
  full_join(all_units, join_by(adm1_id)) |> 
  reduce(fire_data_list, left_join, join_by(c(adm1_id, date))) |> 
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
    treated_unit = as.numeric(ADM1_EN %in% northern_regions_en),
    post_treatment_weak = as.numeric(year > 2012),
    post_treatment_strong = as.numeric(year > 2016),
    is_treated_weak = treated_unit * post_treatment_weak,
    is_treated_strong = treated_unit * post_treatment_strong,
    year_id = as.numeric(factor(year))
  )

### replace the pre 2012 values for viirs (empty) with modis  ------
grid <- grid |> 
  mutate(
    is_pre_2013 = date < "2013-01-01",
    viirs_fixed = ifelse(is_pre_2013, total_fires, total_fires_viirs),
    viirs_no_duplicate_fixed = ifelse(is_pre_2013, modis_no_duplicates, viirs_no_duplicates),
    viirs_no_repeated_fixed = ifelse(is_pre_2013, modis_no_repeated, viirs_no_repeated),
    across(c(night_fires, day_fires, aqua_fires, terra_fires), 
           ~ifelse(is_pre_2013, total_fires, .), 
           .names = "{.col}_mixed")
  ) |> 
  select(-is_pre_2013)

### write ----

fwrite(grid, here(out.dir, "grid_1606.csv"))
