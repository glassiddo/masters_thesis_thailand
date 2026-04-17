source("do/setup.R")

grid <- readRDS(here(out.dir, "grid.rds"))

climate_data <- readRDS(here(build.dir, "climate", "adm1_climatic_date.rds")) |> 
  filter(year < 2013) |>
  rename(adm1_id = admin_id)

# define units
all_units <- grid |>
  pull(adm1_id) |>
  unique()

treated_units <- grid |>
  filter(ADM1_EN %in% northern_regions_en) |>
  pull(adm1_id) |>
  unique()

control_units <- setdiff(all_units, treated_units) 

# key for names of units
names_key <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  st_drop_geometry() |> 
  select(adm1_id, ADM1_EN, ADM0_EN) |>
  mutate(adm1_id = as.character(adm1_id))

# function based on pensynth
# source("do/empirical/helpers/f_sc_many_treated.R")
# function to calculate MSPE and related plots
source("do/empirical/helpers/f_sc_mspe.R")

pre_treatment_year <- 2012 
# years id for synthetic
n_start <- grid |> filter(year == pre_treatment_year) |> pull(year_id) |> min()
n_end <- max(grid$year_id)

n_just_before <- n_start - 1 

# to adjust these to take averages over time, need to adjust the function...
synth_special_predictors <- list(
  list("crop_share", n_just_before, "mean"),
  list("urban_share", n_just_before, "mean"),
  list("tree_share", n_just_before, "mean"),
  list("avg_elevation", n_just_before, "mean"),
  list("avg_temperature", n_just_before, "mean"),
  list("avg_skin_temp", n_just_before, "mean"),
  list("avg_dew_temp", n_just_before, "mean"),
  list("avg_precipitation", n_just_before, "mean"),
  list("avg_u_wind", n_just_before, "mean"),
  list("avg_v_wind", n_just_before, "mean")
)

### function to create a filtered summarised data for a given period
create_data_summary <- function(period_name,
                                period_type, period_values) {
  
  constant_vars <- c(
    "crop_share", "tree_share", "urban_share", 
    "avg_elevation", 
    "adm1_area"
  )  
  
  fire_vars <- grep("fires|q[1-4]_|_dom$", names(grid), value = TRUE)

  climate_vars <- setdiff(
    grep("^avg_", names(climate_data), value = TRUE),
    c("avg_elevation") # avg_elevation is a constant var
  )

  group_vars <- c("adm1_id", "ADM1_EN", "year", "year_id")
  
  if (period_type == "month") {
    # filter for a given month
    data_filtered <- grid |> filter(month %in% period_values)
    climate_filtered <- climate_data |> filter(month %in% period_values)
    
  } else if (period_type == "year") {
    # no filtering needed - use full year
    data_filtered <- grid
    climate_filtered <- climate_data
    
  } else if (period_type == "half_month") {
    data_filtered <- grid |> filter(half_month_id %in% period_values)
    
    # get the respective month climate value
    month_values <- unique(ceiling(period_values / 2))
    climate_filtered <- climate_data |> filter(month %in% month_values)    
    
  } else if (period_type == "month_range") {
    data_filtered <- grid |> filter(month >= min(period_values) & month <= max(period_values))
    climate_filtered <- climate_data |> filter(month >= min(period_values) & month <= max(period_values))
  }
  
  summary_data <- data_filtered |>
    group_by(!!!syms(group_vars)) |>
    summarise(
      # constant vars (mean)
      across(all_of(constant_vars), ~mean(., na.rm = TRUE)),
      # fire vars (sum + asinh)
      across(all_of(fire_vars), ~sum(., na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      period = period_name,
      # normalize all fire variables by area 
      across(all_of(fire_vars), ~. / adm1_area)
    )
  
  # summarise climate variables. they're originally by month 
  # so summarising by month shouldn't change the values
  climate_summary <- climate_filtered |>
    group_by(!!!syms(dplyr::intersect(group_vars, names(climate_filtered)))) |>
    summarise(
      across(all_of(climate_vars), ~mean(., na.rm = TRUE)),
      .groups = "drop"
    )
  
  # join fires/admin data with climate data
  summary_data <- summary_data |>
    left_join(climate_summary, by = dplyr::intersect(group_vars, names(climate_summary)))
  
  return(summary_data)
}

# each data should have 2,414 rows 17 years and 142 units  (142×17=2,414)
periods_to_run <- list(
  list(period_name = "Jan-May", period_type = "month_range", period_values = c(1, 5)),
  # list(period_name = "January", period_type = "month", period_values = 1),
  # list(period_name = "February", period_type = "month", period_values = 2),
  # list(period_name = "March", period_type = "month", period_values = 3),
  # list(period_name = "April", period_type = "month", period_values = 4),
   list(period_name = "May", period_type = "month", period_values = 5)
  # list(period_name = "Full Year", period_type = "year", period_values = NULL)
)

## iterate over the periods
datasets_to_run <- purrr::map(
  periods_to_run,
  function(p) create_data_summary(p$period_name, p$period_type, p$period_values)
)

names(datasets_to_run) <- map_chr(periods_to_run, "period_name")

####### run it -----------
outcomes_list <- list(
  # list(dependent = "aqua_fires", predictors = c("aqua_fires")),
  # list(dependent = "terra_fires", predictors = c("terra_fires")),
  list(dependent = "total_fires_modis", predictors = c("total_fires_modis"))
)

gc()

n_begin <- 1 # MUST BE BELOW 8
