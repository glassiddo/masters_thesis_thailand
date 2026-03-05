lambda_treated <- 10
lambda_placebo <- 10 # important for p-values, but very slow; reduce to 1 if just plotting

run_synth_analysis <- function(outcomes_list, datasets_to_run, treated_units, 
                               control_units, n_start, n_end, names_key) {
  all_results <- list()
  summary_df <- data.frame()
  
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
      
      fit <- cv_pensynth(
        X1 = dat$X1, X0 = dat$X0,
        Z1 = dat$Z1, Z0 = dat$Z0,
        nlambda = lambda_treated, verbose = FALSE
      )
      
      weights <- fit$w_opt 
      colnames(weights) <- colnames(dat$Y1plot)
      rownames(weights) <- colnames(dat$Y0plot)    
      
      # Calculate mspe for treated units
      treated_output <- calculate_mspe(dat, fit, outcome_name = dependent_var,
                                        return_full_data = TRUE)
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
        
        calculate_mspe(placebo_dat, placebo_fit, outcome_name = dependent_var,
                        return_full_data = FALSE) %>%
          mutate(unit = colnames(dat$Y0plot)[i])
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
        weights = weights,
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
        treatment_weak_effect_perc = mean(mspe$treatment_weak_effect_perc),
        treatment_strong_effect_perc = mean(mspe$treatment_strong_effect_perc)
      ))
    }
  }
  
  return(list(
    detailed_results = all_results,
    summary_table = summary_df
  ))
}

# Execute analysis
northern_regions_ids <- names_key %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  pull(adm1_id) %>% 
  as.numeric()

analysis_results <- run_synth_analysis(
  outcomes_list = outcomes_list,
  datasets_to_run = datasets_to_run,
  treated_units = treated_units,
  control_units = control_units,
  n_start = n_start,
  n_end = n_end,
  names_key = names_key
)

results <- analysis_results$summary_table 
print(results)

results_fullyear <- results %>% filter(period == "Full Year")
