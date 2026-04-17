source("do/setup.R")

dates <- c(
  seq(as.Date("2004-01-01"), as.Date("2020-12-31"), by="day")
)

all_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  st_drop_geometry()

units_info <- readRDS(here(build.dir, "units", "units1_with_avgs.rds"))

fire_modis <- readRDS(here(build.dir, "fires", "modis_summarised.rds")) |> 
  mutate(date = as.Date(date))

fire_viirs <- readRDS(here(build.dir, "fires", "viirs_summarised.rds")) |> 
  mutate(date = as.Date(date))

climate <- readRDS(here(build.dir, "climate", "adm1_climatic_date.rds"))

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
  mutate(
    treated_unit = as.numeric(ADM1_EN %in% northern_regions_en),
    post_treatment_weak = as.numeric(year > 2012),
    post_treatment_strong = as.numeric(year > 2016),
    is_treated_weak = treated_unit * post_treatment_weak,
    is_treated_strong = treated_unit * post_treatment_strong,
    year_id = as.numeric(factor(year)),
    is_pre_2013 = ifelse(year < 2013, 1, 0)
  ) |> 
  left_join(fire_modis, join_by(adm1_id, date)) |> 
  left_join(fire_viirs, join_by(adm1_id, date)) 

### replace the pre 2013 values for viirs (empty) with modis  ------

viirs_cols <- names(grid)[stringr::str_ends(names(grid), "_viirs")]
viirs_to_modis <- sub("_viirs$", "_modis", viirs_cols)
has_modis_equivalent <- viirs_to_modis %in% names(grid)

viirs_cols <- viirs_cols[has_modis_equivalent]
viirs_to_modis <- viirs_to_modis[has_modis_equivalent]

for (i in seq_along(viirs_cols)) {
  viirs_col <- viirs_cols[[i]]
  modis_col <- viirs_to_modis[[i]]
  grid[[viirs_col]] <- ifelse(grid$is_pre_2013, grid[[modis_col]], grid[[viirs_col]])
}

grid <- grid |>
  mutate(across(matches("(_modis|_viirs)$"), ~replace_na(., 0))) 

### write ----

saveRDS(grid, here(out.dir, "grid.rds"))
