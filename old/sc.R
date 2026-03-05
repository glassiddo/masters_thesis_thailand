rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table,
  tidyverse, stargazer, ggplot2, purrr, readxl, plotrix, classInt,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  fixest, ggfixest, # sun abraham even study
  stargazer,
  kableExtra,
  pensynth, Synth, ggpubr
)
select <- dplyr::select
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

# years id for synthetic
n_start <- grid %>% filter(year == 2012) %>% pull(year_id) %>% min()
n_mid <- grid %>% filter(year == 2016) %>% pull(year_id) %>% min()
n_end <- max(grid$year_id)

n_just_before <- n_start - 1 

# to adjust these to take averages over time, need to adjust the function...
synth_special_predictors <- list(
  list("crop_share", n_just_before, c("mean")),
  list("urban_share", n_just_before, "mean"),
  list("tree_share", n_just_before, "mean"),
  list("avg_elevation", n_just_before, "mean"),
  list("adm1_area", n_just_before, "mean"),
  list("avg_temperature", n_just_before, "mean"),
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
    "avg_elevation", "adm1_area"
  )
  
  fire_vars <- c(
    "total_fires", "night_fires", "day_fires", 
    "terra_fires", "aqua_fires",
    "high_confidence_fires50", "high_confidence_fires75",
    "q1_roads_prov", "q2_roads_prov",          
    "q1_roads_country", "q2_roads_country",        
    "q1_urban_prov", "q2_urban_prov",           
    "q1_urban_country", "q2_urban_country",       
    "q1_crop_prod", "q2_crop_prod",            
    "q3_crop_prod", "q4_crop_prod",            
    "rice_dom", "maize_dom",              
    "cass_dom", "q1_elevation_prov",       
    "q2_elevation_prov", "q1_elevation_country",    
    "q2_elevation_country"
  )
  
  climate_vars <- c(
    "avg_temperature", "avg_dew_temp", "avg_skin_temp",
    "avg_precipitation", "avg_u_wind", "avg_v_wind"
  )
  
  # group by unit and years
  group_vars <- c("adm1_id", "ADM1_EN", "year", "year_id")
  
  if (period_type == "month") {
    # filter for a given month
    data_filtered <- grid %>% filter(month %in% period_values)
    climate_filtered <- climate_data %>% filter(month %in% period_values)
    
  } else if (period_type == "year") {
    # no filtering needed - use full year
    data_filtered <- grid
    climate_filtered <- climate_data
    
  } else if (period_type == "biweek") {
    data_filtered <- grid %>% filter(biweek_period %in% period_values)
    climate_filtered <- climate_data %>% filter(biweek_period %in% period_values)
    
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
      across(all_of(fire_vars), ~asinh(sum(., na.rm = TRUE))),
      .groups = "drop"
    ) %>%
    mutate(period = period_name)
  
  # summarise climate variables. they're originally by month 
  # so summarising by month shouldn't change the values
  climate_summary <- climate_filtered %>%
    group_by(!!!syms(intersect(group_vars, names(climate_filtered)))) %>%
    summarise(
      across(all_of(climate_vars), ~mean(., na.rm = TRUE)),
      .groups = "drop"
    )
  
  # join fires/admin data with climate data
  summary_data <- summary_data %>%
    left_join(climate_summary, by = intersect(group_vars, names(climate_summary)))
  
  return(summary_data)
}

# each data should have 2,414 rows 17 years and 142 units  (142×17=2,414)
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

# dry season (Jan-Apr) fires and climate for each unit-year
dry_season <- create_data_summary(
  period_name = "Dry Season (Jan-Apr)",
  period_type = "month_range",
  period_values = c(1, 4)
)

yearly_summary <- create_data_summary(
  period_name = "Full Year",
  period_type = "year",
  period_values = NULL
)

datasets_to_run <- list(
  yearly_summary, dry_season,
  jan_summary, feb_summary, march_summary, april_summary, may_summary
)

####### run it -----------
outcomes_list <- list(
  list(dependent = "total_fires", predictors = c("total_fires"))
  #,list(dependent = "night_fires", predictors = c("night_fires"))
  #,list(dependent = "day_fires", predictors = c("day_fires"))
)


# function to calculate unit-specific RMSPEs
calculate_rmspe <- function(
    dat, 
    fit, 
    outcome_name, 
    return_full_data = FALSE    
    ) {
  
  # Extract components with proper names
  Y1 <- dat$Y1plot
  Y0 <- dat$Y0plot
  W <- fit$w_opt
  unit_names <- colnames(Y1)
  n_time <- nrow(Y1)  
  
  # Calculate synthetic control
  synth_matrix <- Y0 %*% W
  colnames(synth_matrix) <- unit_names
  
  results <- map_dfr(unit_names, function(unit) {
    actual <- Y1[, unit]
    synth <- synth_matrix[, unit]
    
    # Period calculations
    pre_rmspe <- sqrt(mean((actual[1:n_start] - synth[1:n_start])^2))
    post_weak_rmspe <- sqrt(mean((actual[(n_start+1):n_mid] - synth[(n_start+1):n_mid])^2))
    post_strong_rmspe <- sqrt(mean((actual[(n_mid+1):n_time] - synth[(n_mid+1):n_time])^2))
    
    tibble(
      unit = unit,
      pre_rmspe = pre_rmspe,
      post_weak_rmspe = post_weak_rmspe,
      post_strong_rmspe = post_strong_rmspe,
      ratio_weak = post_weak_rmspe / pre_rmspe,
      ratio_strong = post_strong_rmspe / pre_rmspe,
      treatment_weak_effect = mean(actual[(n_start+1):n_mid] - synth[(n_start+1):n_mid]),
      treatment_strong_effect = mean(actual[(n_mid+1):n_time] - synth[(n_mid+1):n_time]),
      outcome = outcome_name
    )
  })
  if (return_full_data) {
    long_data <- tibble(
      time = rep(1:n_time, times = 2 * length(unit_names)),
      value = c(c(Y1), c(synth_matrix)),
      type = rep(rep(c("actual", "synthetic"), each = n_time), times = length(unit_names)),
      unit = rep(unit_names, each = n_time * 2),
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

# Function to get top N weights for a given treated unit and weight matrix
get_top_weights <- function(weights_matrix, treated_unit_id, num_top = 5, names_key_df) {
  unit_weights_vec <- weights_matrix[, as.character(treated_unit_id), drop = FALSE] %>% as.numeric()
  names(unit_weights_vec) <- rownames(weights_matrix)
    
  # sort
  top_n_weights <- unit_weights_vec %>%
    sort(decreasing = TRUE) %>%
    head(num_top)
    
  # convert to data frame and add names
  top_weights_df <- data.frame(
    control_adm1_id = names(top_n_weights),
    weight = as.numeric(top_n_weights)
    ) %>%
    left_join(names_key_df %>% st_drop_geometry(), by = c("control_adm1_id" = "adm1_id")) %>%
    select(control_adm1_id, ADM1_EN, ADM0_EN, weight) %>%
    arrange(desc(weight))
    
    return(top_weights_df)
}

# Function to create a map for synthetic control weights for a single treated unit
create_single_synth_weights_map <- function(treated_unit_id, weights_matrix, current_period_name, names_key) {
  # get the weights for the specific treated unit
  unit_weights <- weights_matrix[, as.character(treated_unit_id), drop = FALSE]
  
  # map the units to names
  map_data <- names_key %>%
    left_join(
      as.data.frame(unit_weights) %>%
        rownames_to_column(var = "adm1_id") %>%
        rename(weight = !!as.character(treated_unit_id)),
      by = "adm1_id"
    ) %>%
    mutate(
      weight = ifelse(is.na(weight), 0, weight), # replace NAs with 0
      weight_category = case_when(
        adm1_id == as.character(treated_unit_id) ~ "Treated Unit",
        adm1_id %in% treated_units & 
          adm1_id != as.character(treated_unit_id) ~ "Other treated",
        weight == 0 ~ "No Weight",
        weight <= 0.05 ~ "0.0 - 0.05", 
        weight <= 0.1 ~ "0.05 - 0.1",
        weight <= 0.2 ~ "0.1 - 0.2",
        weight <= 0.3 ~ "0.2 - 0.3",
        weight <= 0.5 ~ "0.3 - 0.5",
        TRUE ~ "0.5+"
      )
    )
  
  # Define colors
  colors <- c(
    "Treated Unit" = "#2d3748",       # Dark grey/black for treated
    "Other treated" = "#c0c3c8",         # light grey for other
    "0.0 - 0.05" = "#fee0d2",         # Lighter red
    "0.05 - 0.1" = "#fcbba1",
    "0.1 - 0.2" = "#fc9272",
    "0.2 - 0.3" = "#fb6a4a",
    "0.3 - 0.5" = "#ef3b2c",
    "0.5+" = "#cb181d"                # Darkest red
  )
  
  # Order categories for legend
  weight_category_levels <- c("Treated Unit", "0.5+", "0.3 - 0.5", "0.2 - 0.3",
                              "0.1 - 0.2", "0.05 - 0.1", "0.0 - 0.05", "Other treated")
  map_data$weight_category <- factor(map_data$weight_category, levels = weight_category_levels)
  
  treated_unit_name <- names_key %>%
    filter(adm1_id == as.character(treated_unit_id)) %>%
    pull(ADM1_EN)
  
  p <- ggplot(map_data) +
    geom_sf(aes(fill = weight_category),
            color = "white",
            size = 0.3) +
    scale_fill_manual(
      name = "Weight Category",
      values = colors,
      breaks = weight_category_levels,
      drop = FALSE # Ensure all levels are shown even if no data
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey60"),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      panel.background = element_rect(fill = "#f8fafc", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(
      title = paste("Synthetic Control Weights for", treated_unit_name),
      subtitle = paste("Period:", current_period_name, "| Outcome:", tools::toTitleCase(dependent_var)),
      caption = paste("Treated unit in dark grey, control unit weights in red shades.")
    )
  
  return(p)
}

summary_df <- data.frame()
plot_data_list <- list()
plot_list <- list()
weights_data_list  <- list() # to store maps of synthetic weights
top_weights_tables_list <- list() # to store top weights tables

# loop over outcome variables
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
      time.predictors.prior = 1:n_start,
      time.optimize.ssr = 1:n_start,
      time.plot = 1:n_end
    )
    
    fit <- cv_pensynth(
      X1 = dat$X1, X0 = dat$X0,
      Z1 = dat$Z1, Z0 = dat$Z0,
      nlambda = 100, verbose = FALSE
    )
    
    weights <- fit$w_opt 
    colnames(weights) <- colnames(dat$Y1plot)
    rownames(weights) <- colnames(dat$Y0plot)    
    
    # The actual 'weights' object should retain its full matrix form for calculation
    weights_display <- weights %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "control_adm1_id") %>%
      #mutate(across(-control_adm1_id, ~ ifelse(. < 0.05, 0, .))) %>% # Filter small weights for display
      #filter(!if_all(-control_adm1_id, ~ . == 0)) %>%
      tibble::column_to_rownames(var = "control_adm1_id")
    

    # RMSPE (treated) - now returns unit-specific values
    output <- calculate_rmspe(dat, fit, outcome_name = dependent_var,
                              return_full_data = TRUE)
    
    rmspe <- output$results        # For analysis
    plot_data <- output$plot_data  # For visualization
    diff_data <- output$diff_data  # For effect sizes
    
    # placebo loop
    placebo_rmspe_results <- list()
    for (i in 1:ncol(dat$X0)) {
      placebo_X1 <- dat$X0[, i, drop = FALSE]
      placebo_Z1 <- dat$Z0[, i, drop = FALSE]
      placebo_Y1 <- dat$Y0plot[, i, drop = FALSE]
      
      placebo_X0 <- dat$X0[, -i, drop = FALSE]
      placebo_Z0 <- dat$Z0[, -i, drop = FALSE]
      placebo_Y0 <- dat$Y0plot[, -i, drop = FALSE]
      
      placebo_fit <- cv_pensynth(
        X1 = placebo_X1, X0 = placebo_X0,
        Z1 = placebo_Z1, Z0 = placebo_Z0,
        nlambda = 1, verbose = FALSE
      )
      
      placebo_dat <- list(
        Y1plot = placebo_Y1,
        Y0plot = placebo_Y0
      )
      
      rmspe_result <- calculate_rmspe(
        placebo_dat, placebo_fit, 
        outcome_name = dependent_var,
        return_full_data = FALSE
        )
      
      rmspe_result$unit <- colnames(dat$Y0plot)[i]
      placebo_rmspe_results[[i]] <- rmspe_result
    }
    
    # Combine placebo results
    placebo_rmspe_df <- do.call(rbind, placebo_rmspe_results)
    
    # Now calculate means for Abadie-Lhour method (after getting unit-specific values)
    post_strong_rmspe_t <- mean(rmspe$post_strong_rmspe)
    post_weak_rmspe_t <- mean(rmspe$post_weak_rmspe)
    pre_rmspe_t <- mean(rmspe$pre_rmspe)
    
    ratio_treated_strong <- post_strong_rmspe_t / pre_rmspe_t
    ratio_treated_weak <- post_weak_rmspe_t / pre_rmspe_t
    
    placebo_ratios_strong <- placebo_rmspe_df$post_strong_rmspe / placebo_rmspe_df$pre_rmspe
    placebo_ratios_weak <- placebo_rmspe_df$post_weak_rmspe / placebo_rmspe_df$pre_rmspe
    
    extreme_count_strong <- sum(placebo_ratios_strong >= ratio_treated_strong)
    extreme_count_weak <- sum(placebo_ratios_weak >= ratio_treated_weak)
    
    num_placebo <- length(control_units)
    
    p_value_strong <- (1 + extreme_count_strong) / (1 + num_placebo)
    p_value_weak <- (1 + extreme_count_weak) / (1 + num_placebo)
    
    # Calculate means of unit-specific effects for summary
    treatment_strong_effect <- mean(rmspe$treatment_strong_effect)
    treatment_weak_effect <- mean(rmspe$treatment_weak_effect)
    
    summary_df <- rbind(summary_df, data.frame(
      outcome = dependent_var,
      period = current_period_name,
      p_value_strong = p_value_strong,
      p_value_weak = p_value_weak,
      treatment_strong_effect = treatment_strong_effect,
      treatment_weak_effect = treatment_weak_effect
    ))
    
    plot_df <- data.frame(
      unit = c(rmspe$unit, colnames(dat$Y0plot)),
      rmspe_ratio = c(rmspe$ratio_strong, placebo_ratios_strong),
      is_treated = c(rep("Yes", nrow(rmspe)), rep("No", length(placebo_ratios_strong)))
    )
    
    # Add unit names for better readability (optional)
    plot_df <- plot_df %>%
      left_join(names_key %>% st_drop_geometry(), by = c("unit" = "adm1_id")) %>%
      mutate(
        color_group = case_when(
          ADM1_EN %in% northern_regions_en ~ "Treated",
          ADM0_EN == "Thailand" ~ "Thailand (Control)",
          ADM0_EN == "Myanmar" ~ "Myanmar (Control)",
          TRUE ~ "Other (Control)"
        ),
        unit_order = row_number()  # Create simple numeric ordering
      )
    
    plot_data_list[[paste0(dependent_var, "_", current_period_name)]] <- plot_df

    # Store weights matrix and metadata for later map creation
    weights_data_list[[paste0(dependent_var, "_", current_period_name)]] <- list(
      weights_matrix = weights,
      dependent_var = dependent_var,
      period = current_period_name
    )
    
    # top_weights_df <- get_top_weights(
    #   weights_matrix = weights,
    #   treated_unit_id = treated_unit_id,
    #   num_top = 5,
    #   names_key_df = names_key
    # )
  }
}

print(summary_df)

for (i in 1:length(plot_data_list)) {
  plot_df <- plot_data_list[[i]]
  
  p <- ggplot(plot_df, aes(x = reorder(unit_order, rmspe_ratio), y = rmspe_ratio, fill = color_group)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Treated" = "red", 
                                 "Thailand (Control)" = "lightblue", 
                                 "Myanmar (Control)" = "lightgreen",
                                 "Other (Control)" = "lightgray")) +
    labs(
      title = paste(plot_df$dependent_var[1], " - ", plot_df$period[1]),
      #x = "Units (ordered by RMSPE ratio)",
      fill = "Unit Type"
    ) +
    scale_y_continuous(limits = c(0, min(10, max(plot_df$rmspe_ratio, na.rm = TRUE)))) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),  # Remove unit names from y-axis
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  
  plot_list[[i]] <- p
}

# NEW: Create weight maps outside the loop using the stored weights data
weights_maps_list <- list()
for (weights_key in names(weights_data_list)) {
  weights_info <- weights_data_list[[weights_key]]
  weights_matrix <- weights_info$weights_matrix
  dependent_var <- weights_info$dependent_var
  current_period_name <- weights_info$period
  
  # Iterate over each treated unit for the current weights matrix
  for (treated_id in colnames(weights_matrix)) {
    # Generate Weight Map
    map_plot <- create_single_synth_weights_map(
      treated_unit_id = as.numeric(treated_id),
      weights_matrix = weights_matrix,
      current_period_name = current_period_name,
      names_key = names_key
    )
    weights_maps_list[[paste0("Treated_", treated_id, "_", dependent_var, "_", current_period_name)]] <- map_plot
  }
}

ggarrange(plotlist = plot_list[1:8], ncol = 4, nrow = 2)

weights_maps_list$`Treated_113_total_fires_Full Year`
