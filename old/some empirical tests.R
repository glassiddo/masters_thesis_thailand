rm(list=ls())
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest, # twfe + sun and abraham event study
  broom
)
setwd("C:/Users/iddo2/Documents/thesis data/data/")

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

grid <- fread("grid_1405.csv") %>%
  mutate(month = month(date)) %>% 
  filter(year > 2012, year < 2020) %>% 
  filter(ADM0_EN != "Laos")

grid_adm1 <- fread("grid_adm1_1405.csv") %>%
  mutate(month = month(date)) %>% 
  filter(year > 2012) %>% ##, year < 2020) %>% 
  filter(ADM0_EN != "Laos") 
  #left_join(weights_adm1, by = "ADM1_EN")

weights_adm1 <- grid %>% 
  select(ADM1_EN, adm2_id) %>% 
  distinct() %>%
  group_by(ADM1_EN) %>% 
  summarise(
    weights = length(adm2_id)
  )

climatic_controls <- c(
  "avg_temperature", "avg_surface_temp", "avg_precipitation",
  "avg_u_wind", "avg_v_wind"
  )

population_controls <- c(
  "Total_pop", "Male_pop", "HH_num"
  # "Female_pop", # colinear with male pop and total pop 
  )

outcome_variables <- c(
  "total_fires", "day_fires", "night_fires", "terra_fires", "aqua_fires",
  "high_confidence_fires50", "high_confidence_fires75", 
  "q1_roads_prov", "q2_roads_prov", "q1_roads_country", "q2_roads_country",
  "q1_urban_prov", "q2_urban_prov", "q1_urban_country", "q2_urban_country",
  "q1_crop_prod", "q2_crop_prod", "q3_crop_prod", "q4_crop_prod",
  "rice_dom", "maize_dom", "cass_dom",
  "q1_elevation_prov", "q2_elevation_prov", 
  "q1_elevation_country", "q2_elevation_country",
  "crop75_fires", "crop50_fires", "crop33_fires", "forest50_fires"
  #"binary_fire", "log_fires"
  )

fixed_effects <- c("ADM1_EN", "date")

twfe_simple <- c("is_treated")
twfe_climate <- c("is_treated", climatic_controls)
twfe_clim_pop <- c("is_treated", climatic_controls, population_controls)

controls <- twfe_clim_pop

models <- lapply(outcome_variables, function(outcome) {
  formula <- reformulate(controls, response = outcome)
  feols(formula, data = grid, fixef = fixed_effects)
})

# Name the list
names(models) <- outcome_variables

# Example: summary of one model
summary(models$forest_)

aggregate <- grid %>% 
  group_by(year, ADM1_EN, event_time_years, treated_unit) %>% 
  summarise(
    total_fires = sum(total_fires),
    avg_temperature = mean(avg_temperature, na.rm = TRUE),
    avg_surface_temp = mean(avg_surface_temp, na.rm = TRUE),
    avg_precipitation = mean(avg_precipitation, na.rm = TRUE),
    avg_u_wind = mean(avg_u_wind, na.rm = TRUE),
    avg_v_wind = mean(avg_v_wind, na.rm = TRUE)
  ) %>% 
  ungroup()

event_model <- feols(
  total_fires ~ i(event_time_years_first, treated_unit, ref = "0") 
  #+ avg_temperature + avg_surface_temp + avg_precipitation
  #+ avg_u_wind + avg_v_wind 
  | adm1_id + year,
  cluster = ~adm1_id,
  #data = grid_adm1 #, weights = ~weights,
  data = grid
)

iplot(event_model, 
      xlab = "Years relative to treatment",
      main = "Event Study of Treatment on Fires")

#### actual ban ----

## biased
feols(
  total_fires ~ is_ban_actual 
  + avg_temperature + avg_surface_temp + avg_precipitation
  + avg_u_wind + avg_v_wind 
  #+ Total_pop + Male_pop + #Female_pop + 
  #HH_num
  | ADM1_EN + date,
  cluster = ~ADM0_EN,
  data = grid
)

### bins 
bin_start <- -15
bin_end <- 15
bin_space <- 3
breaks_seq <- seq(bin_start, bin_end, by = bin_space)
labels_seq <- paste(head(breaks_seq, -1), "to", tail(breaks_seq, -1) - 1)

h_bin_data <- grid_adm1 %>%
  #filter(ADM1_EN %in% northern_regions_en) %>%
  mutate(
    days_bin = cut(days_from_ban_start, 
                   breaks = breaks_seq,
                   labels = labels_seq,
                   include.lowest = TRUE)
  ) %>% 
  filter(!is.na(days_bin)) 

# Create a list to store models for each bin
bin_models <- list()
unique_bins <- unique(h_bin_data$days_bin)
unique_bins <- unique_bins[!is.na(unique_bins)]

# Run separate models for each bin
for(bin in unique_bins) {
  bin_data <- h_bin_data %>% filter(days_bin == bin)
  
  bin_models[[as.character(bin)]] <- feols(
    n_burns ~ is_treated + 
      avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind
    | 
      year +
      ADM1_EN,
    data = bin_data,
    cluster = ~ADM1_EN
  )
}

results <- lapply(names(bin_models), function(bin) {
  model_summary <- tidy(bin_models[[bin]])
  treated_row <- model_summary[model_summary$term == "is_treated", ]
  treated_row$bin <- bin  # keep track of the bin
  return(treated_row)
})

results_df <- do.call(rbind, results)
results_df <- results_df[, c("bin", "estimate", "std.error", "statistic", "p.value")]
results_df$stars <- symnum(results_df$p.value, corr = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", ""))
print(results_df, n = 100)

##############
# feols(
#   q2_roads_prov ~ is_ban + 
#     Total_pop + 
#     avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind | 
#     ADM1_EN + time_id, 
#   data = grid
# )


### by month -----
library(lfe)      # or fixest if using feols
library(broom)    # for tidy
library(dplyr)
library(lubridate)

# If you have a 'date' column, extract month
grid <- grid %>%
  mutate(month = month(date)) # replace with your actual date column

# Filter or keep all months (optional)
# grid <- grid %>% filter(ADM1_EN %in% northern_regions_en)

# Create list to store models
month_models <- list()
unique_months <- sort(unique(grid$month))

# Estimate separate models for each month
for (m in unique_months) {
  month_data <- grid %>% filter(month == m)
  
  month_models[[as.character(m)]] <- feols(
    q1_crop_prod ~ is_treated + 
      avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind
    | 
      year + ADM1_EN,
    data = month_data,
    cluster = ~ADM1_EN
  )
}

# Collect results
month_results <- lapply(names(month_models), function(m) {
  model_summary <- tidy(month_models[[m]])
  treated_row <- model_summary[model_summary$term == "is_treated", ]
  treated_row$month <- m
  return(treated_row)
})

# Combine and format
results_df <- do.call(rbind, month_results)
results_df <- results_df[, c("month", "estimate", "std.error", "statistic", "p.value")]
results_df$stars <- symnum(results_df$p.value, corr = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", ""))

# Optional: make month a factor for better plotting or sorting
results_df$month <- factor(month.abb[as.numeric(results_df$month)], levels = month.abb)

print(results_df)
