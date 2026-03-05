rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table,
  tidyverse, stargazer, ggplot2, purrr, readxl, plotrix, classInt,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  fixest, ggfixest, # sun abraham even study
  stargazer,
  kableExtra,
  pensynth, Synth, ggpubr,
  cowplot, patchwork
)
select <- dplyr::select
intersect <- dplyr::intersect
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data/")
grid <- fread("grid_1606.csv")

climate_data <- fread("2. intermediate/adm1_climatic_date.csv") %>%
  filter(year < 2013) %>%
  rename(adm1_id = admin_id)

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai",
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# Define units
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

# Main time periods
n_begin <- grid %>% filter(year == 2004) %>% pull(year_id) %>% min()
n_end <- max(grid$year_id)

n_just_before <- grid %>% filter(year == 2012) %>% pull(year_id) %>% min()

# to adjust these to take averages over time, need to adjust the function...
synth_special_predictors <- list(
  list("crop_share", n_just_before, c("mean")),
  list("urban_share", n_just_before, "mean"),
  list("tree_share", n_just_before, "mean"),
  list("avg_elevation", n_just_before, "mean")
  #list("adm1_area", n_just_before, "mean"),
  ,list("avg_temperature", n_just_before, "mean"),
  list("avg_skin_temp", n_just_before, "mean"),
  list("avg_dew_temp", n_just_before, "mean"),
  list("avg_precipitation", n_just_before, "mean"),
  list("avg_u_wind", n_just_before, "mean"),
  list("avg_v_wind", n_just_before, "mean")
)

### function to create a filtered summarised data for a given period
create_data_summary <- function(group_vars, period_name,
                                period_type, period_values) {
  
  constant_vars <- c(
    "crop_share", "tree_share", "urban_share",
    "avg_elevation",
    "adm1_area"
  )
  
  fire_vars <- c(
    "total_fires", "night_fires", "day_fires",
    "terra_fires", "aqua_fires",
    "terra_night_fires", "terra_day_fires",
    "aqua_night_fires", "aqua_day_fires",
    "high_confidence_fires50", "high_confidence_fires75",
    "q1_roads_prov", "q2_roads_prov",
    "q1_roads_country", "q2_roads_country",
    "q1_urban_prov", "q2_urban_prov",
    "q1_frp_fires", "q2_frp_fires",
    "q1_frp_fires_aqua_day", "q2_frp_fires_aqua_day",
    "q1_urban_country", "q2_urban_country",
    "q1_crop_prod", "q2_crop_prod",
    "q3_crop_prod", "q4_crop_prod",
    "rice_dom", "maize_dom", "cass_dom",
    "total_fires_viirs", "night_fires_viirs", "day_fires_viirs", "viirs_fixed",
    "modis_no_duplicates", "modis_no_repeated",
    "viirs_no_duplicate_fixed", "viirs_no_repeated_fixed",
    "crop75_fires", "crop50_fires", "crop33_fires", "forest50_fires",
    "crop50_aqua_day_fires", "crop50_aqua_night_fires",
    "crop50_terra_day_fires", "crop50_terra_night_fires",
    "forest50_aqua_day_fires", "forest50_aqua_night_fires",
    "forest50_terra_day_fires", "forest50_terra_night_fires",
    "night_fires_mixed", "day_fires_mixed", "terra_fires_mixed", "aqua_fires_mixed"
  )
  
  climate_vars <- c(
    "avg_temperature", "avg_dew_temp", "avg_skin_temp",
    "avg_precipitation", "avg_u_wind", "avg_v_wind"
  )
  
  # group by unit and years
  group_vars <- c("adm1_id", "ADM1_EN", "year", "year_id")
  
  if (period_type == "month") {
    data_filtered <- grid %>% filter(month %in% period_values)
    climate_filtered <- climate_data %>% filter(month %in% period_values)
    
  } else if (period_type == "year") {
    data_filtered <- grid
    climate_filtered <- climate_data
    
  } else if (period_type == "half_month") {
    data_filtered <- grid %>% filter(half_month_id %in% period_values)
    month_values <- unique(ceiling(period_values / 2))
    climate_filtered <- climate_data %>% filter(month %in% month_values)
    
  } else if (period_type == "month_range") {
    data_filtered <- grid %>% filter(month >= min(period_values) & month <= max(period_values))
    climate_filtered <- climate_data %>% filter(month >= min(period_values) & month <= max(period_values))
  }
  
  summary_data <- data_filtered %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(
      across(all_of(constant_vars), ~mean(., na.rm = TRUE)),
      across(all_of(fire_vars), ~sum(., na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      period = period_name,
      across(all_of(fire_vars), ~. / adm1_area)
    )
  
  climate_summary <- climate_filtered %>%
    group_by(!!!syms(dplyr::intersect(group_vars, names(climate_filtered)))) %>%
    summarise(
      across(all_of(climate_vars), ~mean(., na.rm = TRUE)),
      .groups = "drop"
    )
  
  summary_data <- summary_data %>%
    left_join(climate_summary, by = dplyr::intersect(group_vars, names(climate_summary)))
  
  return(summary_data)
}

jan_summary <- create_data_summary(
  period_name = "January",
  period_type = "month",
  period_values = 1
)

feb_summary <- create_data_summary(
  period_name = "February",
  period_type = "month",
  period_values = 2
)

march_summary <- create_data_summary(
  period_name = "March",
  period_type = "month",
  period_values = 3
)

april_summary <- create_data_summary(
  period_name = "April",
  period_type = "month",
  period_values = 4
)

may_summary <- create_data_summary(
  period_name = "May",
  period_type = "month",
  period_values = 5
)

jan_may_summary <- create_data_summary(
  period_name = "Jan-May",
  period_type = "month_range",
  period_values = c(1, 5)
)

jan_april_summary <- create_data_summary(
  period_name = "Jan-April",
  period_type = "month_range",
  period_values = c(1, 4)
)

yearly_summary <- create_data_summary(
  period_name = "Full Year",
  period_type = "year",
  period_values = NULL
)

datasets_to_run <- list(
  jan_may_summary,
  jan_summary,
  feb_summary,
  march_summary,
  april_summary,
  may_summary
)

outcomes_list <- list(
  list(dependent = "total_fires", predictors = c("total_fires"))
)

gc()

lambda_treated <- 10
lambda_placebo <- 10

# --- The main analysis function ---
run_synth_analysis_backdate <- function(outcomes_list, datasets_to_run, treated_units,
                                        control_units, backdating_periods, names_key) {
  all_results <- list()
  summary_df <- data.frame()
  
  for (period_def in backdating_periods) {
    validation_years <- period_def$validation_years
    
    # Dynamically set the end of the pre-treatment period
    pre_end_year_id <- grid %>% filter(year == (2012 - validation_years)) %>% pull(year_id) %>% min()
    
    # These are fixed
    n_begin <- grid %>% filter(year == 2004) %>% pull(year_id) %>% min()
    n_start <- grid %>% filter(year == 2012) %>% pull(year_id) %>% min()
    n_mid <- grid %>% filter(year == 2016) %>% pull(year_id) %>% min()
    n_end <- max(grid$year_id)
    
    for (outcome in outcomes_list) {
      dependent_var <- outcome$dependent
      predictors <- outcome$predictors
      
      for (period_data in datasets_to_run) {
        current_period_name <- unique(period_data$period)
        period_data <- as.data.frame(period_data)
        
        dat <- create_multi_synth_dataprep(
          data = period_data,
          predictors = predictors,
          special.predictors = synth_special_predictors,
          dependent = dependent_var,
          unit.variable = "adm1_id",
          time.variable = "year_id",
          treatment.identifier = treated_units,
          controls.identifier = control_units,
          time.predictors.prior = n_begin:pre_end_year_id,
          time.optimize.ssr = n_begin:pre_end_year_id,
          time.plot = n_begin:n_end
        )
        
        fit <- cv_pensynth(
          X1 = dat$X1, X0 = dat$X0,
          Z1 = dat$Z1, Z0 = dat$Z0,
          nlambda = lambda_treated, verbose = FALSE
        )
        
        weights <- fit$w_opt
        colnames(weights) <- colnames(dat$Y1plot)
        rownames(weights) <- colnames(dat$Y0plot)
        
        # Calculate mspe for treated units using the updated function
        treated_output <- calculate_mspe_backdate(
          dat, fit, outcome_name = dependent_var,
          return_full_data = TRUE,
          validation_years = validation_years
        )
        mspe <- treated_output$results
        
        # Run placebo tests
        placebo_mspe_results <- lapply(seq_along(control_units), function(i) {
          placebo_dat <- list(
            Y1plot = dat$Y0plot[, i, drop = FALSE],
            Y0plot = dat$Y0plot[, -i, drop = FALSE],
            X1 = dat$X0[, i, drop = FALSE],
            X0 = dat$X0[, -i, drop = FALSE],
            Z1 = dat$Z0[, i, drop = FALSE],
            Z0 = dat$Z0[, -i, drop = FALSE]
          )
          
          placebo_fit <- cv_pensynth(
            X1 = placebo_dat$X1, X0 = placebo_dat$X0,
            Z1 = placebo_dat$Z1, Z0 = placebo_dat$Z0,
            nlambda = lambda_placebo, verbose = FALSE
          )
          
          calculate_mspe_backdate(
            placebo_dat, placebo_fit, outcome_name = dependent_var,
            return_full_data = FALSE,
            validation_years = validation_years
          ) %>%
            mutate(unit = colnames(dat$Y0plot)[i])
        })
        
        placebo_mspe_df <- bind_rows(placebo_mspe_results)
        
        # Calculate p-values based on the new results
        post_strong_mspe_t <- mean(mspe$post_strong_mspe)
        post_weak_mspe_t <- mean(mspe$post_weak_mspe)
        pre_mspe_t <- mean(mspe$pre_mspe)
        validation_mspe_t <- mean(mspe$validation_mspe)
        
        ratio_treated_strong <- post_strong_mspe_t / pre_mspe_t
        ratio_treated_weak <- post_weak_mspe_t / pre_mspe_t
        ratio_treated_validation <- validation_mspe_t / pre_mspe_t
        
        placebo_ratios_strong <- placebo_mspe_df$post_strong_mspe / placebo_mspe_df$pre_mspe
        placebo_ratios_weak <- placebo_mspe_df$post_weak_mspe / placebo_mspe_df$pre_mspe
        placebo_ratios_validation <- placebo_mspe_df$validation_mspe / placebo_mspe_df$pre_mspe
        
        extreme_count_strong <- sum(placebo_ratios_strong >= ratio_treated_strong)
        extreme_count_weak <- sum(placebo_ratios_weak >= ratio_treated_weak)
        extreme_count_validation <- sum(placebo_ratios_validation >= ratio_treated_validation)
        
        p_value_strong <- (1 + extreme_count_strong) / (1 + length(control_units))
        p_value_weak <- (1 + extreme_count_weak) / (1 + length(control_units))
        p_value_validation <- (1 + extreme_count_validation) / (1 + length(control_units))
        
        # Store results
        all_results[[paste0(dependent_var, "_", current_period_name, "_validation_years_", validation_years)]] <- list(
          Y1 = dat$Y1plot,
          Y0 = dat$Y0plot,
          weights = weights,
          mspe_data = bind_rows(mspe, placebo_mspe_df) %>%
            left_join(names_key %>% st_drop_geometry(), by = c("unit" = "adm1_id")) %>%
            mutate(is_treated = unit %in% treated_units),
          plot_data = treated_output$plot_data,
          period = current_period_name,
          dependent_var = dependent_var,
          p_value_strong = p_value_strong,
          p_value_weak = p_value_weak,
          p_value_validation = p_value_validation,
          treatment_effects = c(
            mean(mspe$treatment_strong_effect),
            mean(mspe$treatment_weak_effect))
        )
        
        summary_df <- rbind(summary_df, data.frame(
          outcome = dependent_var,
          period = current_period_name,
          validation_years = validation_years,
          backdate_end_year = (2012 - validation_years),
          p_value_strong = p_value_strong,
          p_value_weak = p_value_weak,
          p_value_validation = p_value_validation,
          treatment_strong_effect = mean(mspe$treatment_strong_effect),
          treatment_weak_effect = mean(mspe$treatment_weak_effect),
          treatment_validation_effect = mean(mspe$treatment_validation_effect),
          treatment_weak_effect_perc = mean(mspe$treatment_weak_effect_perc),
          treatment_strong_effect_perc = mean(mspe$treatment_strong_effect_perc),
          treatment_validation_effect_perc = mean(mspe$treatment_validation_effect_perc)
        ))
      }
    }
  }
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_df
  ))
}

# --- The helper function for calculating MSPE ---
calculate_mspe_backdate <- function(dat, fit, outcome_name, return_full_data = FALSE, eps = 1e-8, validation_years) {
  # Extract components
  Y1 <- dat$Y1plot
  Y0 <- dat$Y0plot
  W <- fit$w_opt
  unit_names <- colnames(Y1)
  
  synth_matrix <- Y0 %*% W
  colnames(synth_matrix) <- unit_names
  
  prop_diff <- function(a, s, idx) {
    mean((a[idx] - s[idx]) / pmax(s[idx], eps) * 100)
  }
  
  results <- map_dfr(unit_names, function(unit) {
    actual <- Y1[, unit]
    synth <- synth_matrix[, unit]
    
    n_total <- length(actual)
    
    post_strong_indices <- (n_total - 3):n_total
    post_weak_indices <- (n_total - 7):(n_total - 4)
    validation_indices <- (n_total - 7 - validation_years):(n_total - 8)
    pre_indices <- 1:(n_total - 8 - validation_years)    
    
    # Calculate MSPE for each period
    pre_mspe <- mean((actual[pre_indices] - synth[pre_indices])^2)
    validation_mspe <- mean((actual[validation_indices] - synth[validation_indices])^2)
    post_weak_mspe <- mean((actual[post_weak_indices] - synth[post_weak_indices])^2)
    post_strong_mspe <- mean((actual[post_strong_indices] - synth[post_strong_indices])^2)
    
    tibble(
      unit = unit,
      pre_mspe = pre_mspe,
      validation_mspe = validation_mspe,
      post_weak_mspe = post_weak_mspe,
      post_strong_mspe = post_strong_mspe,
      ratio_validation = validation_mspe / pre_mspe,
      ratio_weak = post_weak_mspe / pre_mspe,
      ratio_strong = post_strong_mspe / pre_mspe,
      treatment_validation_effect = mean(actual[validation_indices] - synth[validation_indices]),
      treatment_weak_effect = mean(actual[post_weak_indices] - synth[post_weak_indices]),
      treatment_strong_effect = mean(actual[post_strong_indices] - synth[post_strong_indices]),
      treatment_validation_effect_perc = prop_diff(actual, synth, validation_indices),
      treatment_weak_effect_perc = prop_diff(actual, synth, post_weak_indices),
      treatment_strong_effect_perc = prop_diff(actual, synth, post_strong_indices),
      outcome = outcome_name
    )
  })
  
  if (return_full_data) {
    n_begin_global <- min(grid$year_id)
    n_end_global <- max(grid$year_id)
    n_obs <- n_end_global - n_begin_global + 1
    
    long_data <- tibble(
      time = rep(n_begin_global:n_end_global, times = 2 * length(unit_names)),
      value = c(c(Y1), c(synth_matrix)),
      type = rep(rep(c("actual", "synthetic"), each = n_obs), times = length(unit_names)),
      unit = rep(unit_names, each = 2 * n_obs),
      outcome = outcome_name
    )
    
    list(
      results = results,
      plot_data = long_data %>%
        mutate(year = time + min(grid$year) - 1),
      diff_data = long_data %>%
        pivot_wider(names_from = type, values_from = value) %>%
        mutate(difference = actual - synthetic) %>%
        mutate(year = time + min(grid$year) - 1)
    )
  } else {
    results
  }
}


##### plots -----
# --- Plotting Functions (Updated) ---
create_unit_plot <- function(Y1, Y0, weights, unit_id, names_key, backdate_year) {
  actual <- Y1[, as.character(unit_id)]
  synthetic <- Y0 %*% weights[, as.character(unit_id)]
  
  plot_df <- data.frame(
    year = 2003 + n_begin + (1:length(actual)) - 1,
    actual = actual,
    synthetic = synthetic
  )
  
  unit_name <- names_key %>%
    filter(adm1_id == as.character(unit_id)) %>%
    pull(ADM1_EN)
  
  ggplot(plot_df, aes(x = year)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 1.05) +
    geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1.05) +
    geom_vline(xintercept = backdate_year, linetype = "dotted", color = "darkgray", linewidth = 1.25) +
    geom_vline(xintercept = 2013, linetype = "dashed", color = "black", linewidth = 1.05) +
    geom_vline(xintercept = 2017, linetype = "dashed", color = "black", linewidth = 1.05) +
    scale_color_manual(values = c("Actual" = "#2E86AB", "Synthetic" = "#A23B72")) +
    labs(title = unit_name) +
    scale_x_continuous(breaks = seq(2004, 2020, by = 5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank()
    )
}

create_plots <- function(results, northern_regions_ids, names_key) {
  unit_plots_list <- list()
  
  for (result_key in names(results)) {
    result <- results[[result_key]]
    
    validation_years <- as.numeric(str_extract(result_key, "\\d+$"))
    backdate_end_year <- 2012 - validation_years
    
    plot_list_units <- lapply(northern_regions_ids, function(id) {
      create_unit_plot(result$Y1, result$Y0, result$weights, id, names_key, backdate_end_year)
    })
    
    legend_plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
      geom_line(aes(color = "Actual"), linewidth = 1.25) +
      geom_line(aes(color = "Synthetic"), linewidth = 1.25) +
      scale_color_manual(values = c("Actual" = "#2E86AB", "Synthetic" = "#A23B72")) +
      guides(color = guide_legend(override.aes = list(linewidth = 2.5))) +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.width = unit(2, "cm")
      )
    
    combined_plot <- ggarrange(
      plotlist = plot_list_units,
      ncol = 3, nrow = 3,
      common.legend = TRUE,
      legend.grob = get_legend(legend_plot),
      legend = "bottom"
    )
    
    final_plot <- combined_plot +
      plot_annotation(
        title = paste("Backdating test:", backdate_end_year),
        theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      )
    
    unit_plots_list[[result_key]] <- final_plot
  }
  
  list(
    unit_plots = unit_plots_list
  )
}
    
    
# --- Execute Analysis and Plotting ---
northern_regions_ids <- names_key %>%
  filter(ADM1_EN %in% northern_regions_en) %>%
  pull(adm1_id) %>%
  as.numeric()

# Define the backdating scenarios
backdating_periods <- list(
  list(validation_years = 0, backdate_end_year = 2012),
  list(validation_years = 1, backdate_end_year = 2011),
  list(validation_years = 2, backdate_end_year = 2010),
  list(validation_years = 3, backdate_end_year = 2009)
)

backdating_analysis_results <- run_synth_analysis_backdate(
  outcomes_list = outcomes_list,
  datasets_to_run = datasets_to_run,
  treated_units = treated_units,
  control_units = control_units,
  backdating_periods = backdating_periods,
  names_key = names_key
)

# Now, call the create_plots function with the results
all_plots <- create_plots(
  results = backdating_analysis_results$detailed_results,
  northern_regions_ids = northern_regions_ids,
  names_key = names_key
)

results <- backdating_analysis_results$summary_table
print(results)

all_plots$unit_plots$`total_fires_Jan-May_validation_years_3`

