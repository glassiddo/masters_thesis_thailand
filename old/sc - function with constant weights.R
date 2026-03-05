run_synth_analysis_fixed_weights <- function(outcomes_list, datasets_to_run, treated_units, 
                                             control_units, n_start, n_mid, n_end, names_key) {
  all_results <- list()
  summary_df <- data.frame()
  
  # === Step 1: Estimate weights ONCE for "total_fires" and "Jan-May" ===
  reference_outcome <- "total_fires"
  reference_period <- "Jan-May"
  
  reference_outcome_obj <- outcomes_list[[which(sapply(outcomes_list, \(x) x$dependent == reference_outcome))]]
  reference_predictors <- reference_outcome_obj$predictors
  
  reference_data <- datasets_to_run[[which(sapply(datasets_to_run, \(x) unique(x$period) == reference_period))]]
  
  dat_ref <- create_multi_synth_dataprep(
    data = as.data.frame(reference_data),
    predictors = reference_predictors,
    special.predictors = synth_special_predictors,
    dependent = reference_outcome,
    unit.variable = "adm1_id",
    time.variable = "year_id",
    treatment.identifier = treated_units,
    controls.identifier = control_units,
    time.predictors.prior = n_begin:n_start,
    time.optimize.ssr = n_begin:n_start,
    time.plot = n_begin:n_end
  )
  
  fit_ref <- cv_pensynth(
    X1 = dat_ref$X1, X0 = dat_ref$X0,
    Z1 = dat_ref$Z1, Z0 = dat_ref$Z0,
    nlambda = lambda_treated, verbose = FALSE
  )
  
  shared_weights <- fit_ref$w_opt
  
  # === Step 2: Calculate placebo weights ONCE for each control unit ===
  placebo_weights_list <- lapply(seq_along(control_units), function(i) {
    placebo_dat <- list(
      Y1plot = dat_ref$Y0plot[, i, drop = FALSE],
      Y0plot = dat_ref$Y0plot[, -i, drop = FALSE],
      X1 = dat_ref$X0[, i, drop = FALSE],
      X0 = dat_ref$X0[, -i, drop = FALSE],
      Z1 = dat_ref$Z0[, i, drop = FALSE],
      Z0 = dat_ref$Z0[, -i, drop = FALSE]
    )
    
    placebo_fit <- cv_pensynth(
      X1 = placebo_dat$X1, X0 = placebo_dat$X0,
      Z1 = placebo_dat$Z1, Z0 = placebo_dat$Z0,
      nlambda = lambda_placebo, verbose = FALSE
    )
    
    list(
      weights = placebo_fit$w_opt,
      control_unit_id = control_units[i],
      remaining_controls = control_units[-i]
    )
  })
  
  # === Step 3: Loop over all outcomes and periods using pre-calculated weights ===
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
        time.predictors.prior = n_begin:n_start,
        time.optimize.ssr = n_begin:n_start,
        time.plot = n_begin:n_end
      )
      
      # Create weights matrix for plotting compatibility
      weights_matrix <- matrix(shared_weights, 
                               nrow = length(control_units), 
                               ncol = length(treated_units))
      colnames(weights_matrix) <- as.character(treated_units)
      rownames(weights_matrix) <- as.character(control_units)
      
      # Apply shared weights to calculate MSPE and treatment effects
      treated_output <- calculate_mspe(dat, fit = list(w_opt = shared_weights),
                                       outcome_name = dependent_var,
                                       return_full_data = TRUE)
      mspe <- treated_output$results
      
      # === Apply pre-calculated placebo weights ===
      placebo_mspe_results <- lapply(seq_along(placebo_weights_list), function(i) {
        placebo_info <- placebo_weights_list[[i]]
        
        # Reconstruct placebo data structure for current outcome/period
        placebo_dat <- list(
          Y1plot = dat$Y0plot[, i, drop = FALSE],
          Y0plot = dat$Y0plot[, -i, drop = FALSE],
          X1 = dat$X0[, i, drop = FALSE],
          X0 = dat$X0[, -i, drop = FALSE],
          Z1 = dat$Z0[, i, drop = FALSE],
          Z0 = dat$Z0[, -i, drop = FALSE]
        )
        
        # Use pre-calculated weights
        calculate_mspe(placebo_dat, fit = list(w_opt = placebo_info$weights), 
                       outcome_name = dependent_var,
                       return_full_data = FALSE) %>%
          mutate(unit = as.character(placebo_info$control_unit_id))
      })
      
      placebo_mspe_df <- bind_rows(placebo_mspe_results)
      
      # Calculate p-values
      post_strong_mspe_t <- mean(mspe$post_strong_mspe)
      post_weak_mspe_t <- mean(mspe$post_weak_mspe)
      pre_mspe_t <- mean(mspe$pre_mspe)
      
      ratio_treated_strong <- post_strong_mspe_t / pre_mspe_t
      ratio_treated_weak <- post_weak_mspe_t / pre_mspe_t
      
      placebo_ratios_strong <- placebo_mspe_df$post_strong_mspe / placebo_mspe_df$pre_mspe
      placebo_ratios_weak <- placebo_mspe_df$post_weak_mspe / placebo_mspe_df$pre_mspe
      
      extreme_count_strong <- sum(placebo_ratios_strong >= ratio_treated_strong)
      extreme_count_weak <- sum(placebo_ratios_weak >= ratio_treated_weak)
      
      p_value_strong <- (1 + extreme_count_strong) / (1 + length(control_units))
      p_value_weak <- (1 + extreme_count_weak) / (1 + length(control_units))
      
      # Store results
      all_results[[paste0(dependent_var, "_", current_period_name)]] <- list(
        Y1 = dat$Y1plot,
        Y0 = dat$Y0plot,
        weights = weights_matrix,
        mspe_data = bind_rows(mspe, placebo_mspe_df) %>%
          left_join(names_key %>% st_drop_geometry(), by = c("unit" = "adm1_id")) %>%
          mutate(is_treated = unit %in% treated_units),
        plot_data = treated_output$plot_data,
        period = current_period_name,
        dependent_var = dependent_var,
        p_value_strong = p_value_strong,
        p_value_weak = p_value_weak,
        treatment_effects = c(
          mean(mspe$treatment_strong_effect),
          mean(mspe$treatment_weak_effect))
      )
      
      summary_df <- rbind(summary_df, data.frame(
        outcome = dependent_var,
        period = current_period_name,
        p_value_strong = p_value_strong,
        p_value_weak = p_value_weak,
        treatment_strong_effect = mean(mspe$treatment_strong_effect),
        treatment_weak_effect = mean(mspe$treatment_weak_effect),
        treatment_strong_effect_perc = mean(mspe$treatment_strong_effect_perc),
        treatment_weak_effect_perc = mean(mspe$treatment_weak_effect_perc)
      ))
    }
  }
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_df,
    shared_weights = shared_weights,
    placebo_weights = placebo_weights_list
  ))
}