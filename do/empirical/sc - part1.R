rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table,
  tidyverse, stargazer, ggplot2, purrr, readxl, plotrix, classInt,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  fixest, ggfixest, # sun abraham even study
  cowplot, patchwork, # for plotting 
  stargazer,
  kableExtra,
  pensynth, Synth, ggpubr,
  here
)
select <- dplyr::select
intersect <- dplyr::intersect
options(digits=3)
options(scipen=999)
set.seed(123)

grid <- fread(here("data", "grid_1606.csv"))
climate_data <- fread(here("data", "2. intermediate", "adm1_climatic_date.csv")) %>% 
  filter(year < 2013) %>%
  rename(adm1_id = admin_id)

# define units
all_units <- grid %>%
  pull(adm1_id) %>%
  unique()

treated_units <- grid %>%
  filter(ADM1_EN %in% northern_regions_en) %>%
  pull(adm1_id) %>%
  unique()

control_units <- setdiff(all_units, treated_units) 


# key for names of units
names_key <- read_sf("2. intermediate/units/adm1_units_detailed.shp") %>% 
  select(adm1_id, ADM1_EN, ADM0_EN) %>%
  mutate(adm1_id = as.character(adm1_id))

# function based on pensynth
source("create_multi_synth_dataprep.R") 

pre_treatment_year <- 2012 
# years id for synthetic
n_start <- grid %>% filter(year == pre_treatment_year) %>% pull(year_id) %>% min()
n_end <- max(grid$year_id)

n_just_before <- n_start - 1 

# to adjust these to take averages over time, need to adjust the function...
synth_special_predictors <- list(
  list("crop_share", n_just_before, c("mean")),
  list("urban_share", n_just_before, "mean"),
  list("tree_share", n_just_before, "mean"),
  list("avg_elevation", n_just_before, "mean")
  ,list("avg_temperature", n_just_before, "mean"),
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
  
  fire_vars <- names(grid) %>%
    grep("fires|q[1-4]_|_dom$", ., value = TRUE)

  climate_vars <- names(climate_data) %>%
    grep("^avg_", ., value = TRUE) %>%
    setdiff(c("avg_elevation")) # avg_elevation is a constant var

  group_vars <- c("adm1_id", "ADM1_EN", "year", "year_id")
  
  if (period_type == "month") {
    # filter for a given month
    data_filtered <- grid %>% filter(month %in% period_values)
    climate_filtered <- climate_data %>% filter(month %in% period_values)
    
  } else if (period_type == "year") {
    # no filtering needed - use full year
    data_filtered <- grid
    climate_filtered <- climate_data
    
  } else if (period_type == "half_month") {
    data_filtered <- grid %>% filter(half_month_id %in% period_values)
    
    # get the respective month climate value
    month_values <- unique(ceiling(period_values / 2))
    climate_filtered <- climate_data %>% filter(month %in% month_values)    
    
  } else if (period_type == "month_range") {
    data_filtered <- grid %>% filter(month >= min(period_values) & month <= max(period_values))
    climate_filtered <- climate_data %>% filter(month >= min(period_values) & month <= max(period_values))
  }
  
  summary_data <- data_filtered %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(
      # constant vars (mean)
      across(all_of(constant_vars), ~mean(., na.rm = TRUE)),
      # fire vars (sum + asinh)
      across(all_of(fire_vars), ~sum(., na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      period = period_name,
      # normalize all fire variables by area 
      across(all_of(fire_vars), ~. / adm1_area)
    )
  
  # summarise climate variables. they're originally by month 
  # so summarising by month shouldn't change the values
  climate_summary <- climate_filtered %>%
    group_by(!!!syms(dplyr::intersect(group_vars, names(climate_filtered)))) %>%
    summarise(
      across(all_of(climate_vars), ~mean(., na.rm = TRUE)),
      .groups = "drop"
    )
  
  # join fires/admin data with climate data
  summary_data <- summary_data %>%
    left_join(climate_summary, by = dplyr::intersect(group_vars, names(climate_summary)))
  
  return(summary_data)
}

# each data should have 2,414 rows 17 years and 142 units  (142×17=2,414)
periods_to_run <- list(
  list(period_name = "Jan-May", period_type = "month_range", period_values = c(1, 5)),
  list(period_name = "January", period_type = "month", period_values = 1),
  list(period_name = "February", period_type = "month", period_values = 2),
  list(period_name = "March", period_type = "month", period_values = 3),
  list(period_name = "April", period_type = "month", period_values = 4),
  list(period_name = "May", period_type = "month", period_values = 5),
  list(period_name = "Full Year", period_type = "year", period_values = NULL)
)

## iterate over the periods
datasets_to_run <- pmap(periods_to_run, create_data_summary)

names(datasets_to_run) <- map_chr(periods_to_run, "period_name")

####### run it -----------
outcomes_list <- list(
  # list(dependent = "aqua_fires", predictors = c("aqua_fires")),
  # list(dependent = "terra_fires", predictors = c("terra_fires")),
  list(dependent = "total_fires", predictors = c("total_fires"))
)

gc()

n_begin <- 1 # MUST BE BELOW 8

calculate_mspe <- function(dat, fit, outcome_name, return_full_data = FALSE, eps = 1e-8) {
  # Extract components with proper names
  Y1 <- dat$Y1plot
  Y0 <- dat$Y0plot
  W <- fit$w_opt
  unit_names <- colnames(Y1)
  n_time <- nrow(Y1)  
  
  # Calculate synthetic control
  synth_matrix <- Y0 %*% W
  colnames(synth_matrix) <- unit_names
  
  prop_diff <- function(a, s, idx) {
    mean( (a[idx] - s[idx]) / pmax(s[idx], eps) * 100 )
  }
  
  # Calculate MSPE metrics for each unit
  results <- map_dfr(unit_names, function(unit) {
    actual <- Y1[, unit]
    synth <- synth_matrix[, unit]
    
    n_total <- length(actual)  
    post_strong_indices <- (n_total - 3):n_total # 2020-2017, last 4 years
    post_weak_indices <- (n_total - 7):(n_total - 4) # 2013-2016, 4 years before
    pre_indices <- 1:(n_total - 8) # whatever comes before
    
    pre_mspe <- ##sqrt(
      mean((actual[pre_indices] - synth[pre_indices])^2)
    #)
    post_weak_mspe <- ##sqrt(
      mean((actual[post_weak_indices] - synth[post_weak_indices])^2)
    #)
    post_strong_mspe <- #sqrt(
      mean((actual[post_strong_indices] - synth[post_strong_indices])^2)
    #)
    
    tibble(
      unit = unit,
      pre_mspe = pre_mspe,
      post_weak_mspe = post_weak_mspe,
      post_strong_mspe = post_strong_mspe,
      ratio_weak = post_weak_mspe / pre_mspe,
      ratio_strong = post_strong_mspe / pre_mspe,
      treatment_weak_effect = mean(actual[post_weak_indices] - synth[post_weak_indices]),
      treatment_strong_effect = mean(actual[post_strong_indices] - synth[post_strong_indices]),
      treatment_weak_effect_perc = prop_diff(actual, synth, post_weak_indices),
      treatment_strong_effect_perc = prop_diff(actual, synth, post_strong_indices),
      outcome = outcome_name
    )
  })
  
  if (return_full_data) {
    indices <- 1:(n_end-n_begin+1)
    n_obs <- max(indices)
    
    Y1_trimmed <- Y1[indices, , drop = FALSE]
    synth_trimmed <- synth_matrix[indices, , drop = FALSE]
    
    long_data <- tibble(
      time = rep(indices, times = 2 * length(unit_names)),
      value = c(c(Y1_trimmed), c(synth_trimmed)),
      type = rep(rep(c("actual", "synthetic"), each = n_obs), times = length(unit_names)),
      unit = rep(unit_names, each = 2 * n_obs),
      outcome = outcome_name
    )
    
    list(
      results = results,
      plot_data = long_data,
      diff_data = long_data %>% 
        pivot_wider(names_from = type, values_from = value) %>%
        mutate(difference = actual - synthetic)
    )
  } else {
    results
  }
}
