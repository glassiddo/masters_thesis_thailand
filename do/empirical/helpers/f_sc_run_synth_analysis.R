run_synth_analysis <- function(outcomes_list, datasets_to_run, treated_units, 
                               control_units, n_start, n_end, names_key,
                               synth_special_predictors, n_begin,
                               lambda_treated, lambda_placebo) {
  create_pensynth_dataprep_via_synth <- function(data,
                                                 predictors,
                                                 special.predictors,
                                                 dependent,
                                                 unit.variable,
                                                 time.variable,
                                                 treatment.identifier,
                                                 controls.identifier,
                                                 time.predictors.prior,
                                                 time.optimize.ssr,
                                                 time.plot) {
    data <- as.data.frame(data)
    
    # Synth::dataprep requires unit ids to be numeric
    data[[unit.variable]] <- suppressWarnings(as.numeric(as.character(data[[unit.variable]])))
    treatment.identifier <- suppressWarnings(as.numeric(as.character(treatment.identifier)))
    controls.identifier <- suppressWarnings(as.numeric(as.character(controls.identifier)))
      
    treated_dps <- purrr::map(treatment.identifier, function(t_id) {
      Synth::dataprep(
        foo = data,
        predictors = predictors,
        predictors.op = "mean",
        special.predictors = special.predictors,
        dependent = dependent,
        unit.variable = unit.variable,
        time.variable = time.variable,
        treatment.identifier = t_id,
        controls.identifier = controls.identifier,
        time.predictors.prior = time.predictors.prior,
        time.optimize.ssr = time.optimize.ssr,
        time.plot = time.plot
      )
    })
    
    first_dp <- treated_dps[[1]]
    
    # Donor-side matrices (common across treated units)
    X0 <- as.matrix(first_dp$X0)
    Z0 <- as.matrix(first_dp$Z0)
    Y0plot <- as.matrix(first_dp$Y0plot)
    
    # Treated-side matrices (stack treated columns)
    X1 <- do.call(cbind, purrr::map(treated_dps, ~ as.matrix(.x$X1)))
    Z1 <- do.call(cbind, purrr::map(treated_dps, ~ as.matrix(.x$Z1)))
    Y1plot <- do.call(cbind, purrr::map(treated_dps, ~ as.matrix(.x$Y1plot)))
    
    colnames(X1) <- as.character(treatment.identifier)
    colnames(Z1) <- as.character(treatment.identifier)
    colnames(Y1plot) <- as.character(treatment.identifier)
    colnames(X0) <- as.character(colnames(X0))
    colnames(Z0) <- as.character(colnames(Z0))
    colnames(Y0plot) <- as.character(colnames(Y0plot))
    
    list(
      X0 = X0, X1 = X1,
      Z0 = Z0, Z1 = Z1,
      Y0plot = Y0plot, Y1plot = Y1plot
    )
  }
  
  ensure_matrix_double <- function(x) {
    x <- as.matrix(x)
    storage.mode(x) <- "double"
    x
  }
  
  validate_pensynth_dat <- function(dat, context, delta_eps = 1e-12) {
    dat$X1 <- ensure_matrix_double(dat$X1)
    dat$X0 <- ensure_matrix_double(dat$X0)
    dat$Z1 <- ensure_matrix_double(dat$Z1)
    dat$Z0 <- ensure_matrix_double(dat$Z0)
    dat$Y1plot <- ensure_matrix_double(dat$Y1plot)
    dat$Y0plot <- ensure_matrix_double(dat$Y0plot)
    
    stopifnot(all(is.finite(dat$Z1)))
    stopifnot(all(is.finite(dat$Z0)))
    stopifnot(all(is.finite(dat$Y1plot)))
    stopifnot(all(is.finite(dat$Y0plot)))
    
    X <- cbind(dat$X1, dat$X0)
    stopifnot(all(is.finite(X)))
    
    sc <- apply(X, 1, sd)
    if (any(!is.finite(sc) | sc <= 0)) {
      bad <- rownames(X)[which(!is.finite(sc) | sc <= 0)]
      stop("Non-usable covariate(s) in ", context, ": ", paste(bad, collapse = ", "))
    }
    
    mu <- apply(X, 1, mean)
    X0s <- (dat$X0 - mu) / sc
    X1s <- (dat$X1 - mu) / sc
    
    for (k in seq_len(ncol(X1s))) {
      delta <- colSums((X0s - as.numeric(X1s[, k]))^2)
      if (any(!is.finite(delta) | delta <= delta_eps)) {
        bad_donors <- colnames(X0s)[which(!is.finite(delta) | delta <= delta_eps)]
        stop("Problematic donor(s) in ", context, ": ", paste(bad_donors, collapse = ", "))
      }
    }
    
    dat
  }
  
  all_results <- list()
  summary_df <- data.frame()
  
  for (outcome in outcomes_list) {
    dependent_var <- outcome$dependent
    predictors <- outcome$predictors
    
    for (period_data in datasets_to_run) {
      current_period_name <- unique(period_data$period)
      period_data <- as.data.frame(period_data)
      
      dat <- create_pensynth_dataprep_via_synth(
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
      
      dat <- validate_pensynth_dat(dat, context = paste0(dependent_var, " / ", current_period_name))
      
      fit <- cv_pensynth(
        X1 = dat$X1, X0 = dat$X0,
        Z1 = dat$Z1, Z0 = dat$Z0,
        nlambda = lambda_treated, verbose = FALSE
      )
      
      weights <- fit$w_opt
      if (is.null(dim(weights))) {
        weights <- matrix(weights, ncol = 1)
      }
      colnames(weights) <- colnames(dat$Y1plot)
      rownames(weights) <- colnames(dat$Y0plot)    
      
      # Calculate mspe for treated units
      treated_output <- calculate_mspe(dat, fit, outcome_name = dependent_var,
                                       return_full_data = TRUE)
      mspe <- treated_output$results
      
      # Run placebo tests
      active_control_units <- colnames(dat$Y0plot)
      
      placebo_mspe_results <- lapply(seq_along(active_control_units), function(i) {
        placebo_dat <- list(
          Y1plot = dat$Y0plot[, i, drop = FALSE],
          Y0plot = dat$Y0plot[, -i, drop = FALSE],
          X1 = dat$X0[, i, drop = FALSE],
          X0 = dat$X0[, -i, drop = FALSE],
          Z1 = dat$Z0[, i, drop = FALSE],
          Z0 = dat$Z0[, -i, drop = FALSE]
        )
        
        placebo_dat$X1 <- ensure_matrix_double(placebo_dat$X1)
        placebo_dat$X0 <- ensure_matrix_double(placebo_dat$X0)
        placebo_dat$Z1 <- ensure_matrix_double(placebo_dat$Z1)
        placebo_dat$Z0 <- ensure_matrix_double(placebo_dat$Z0)
        placebo_dat$Y1plot <- ensure_matrix_double(placebo_dat$Y1plot)
        placebo_dat$Y0plot <- ensure_matrix_double(placebo_dat$Y0plot)
        
        placebo_dat <- validate_pensynth_dat(
          list(
            X1 = placebo_dat$X1, X0 = placebo_dat$X0,
            Z1 = placebo_dat$Z1, Z0 = placebo_dat$Z0,
            Y1plot = placebo_dat$Y1plot, Y0plot = placebo_dat$Y0plot
          ),
          context = paste0("placebo: ", active_control_units[i], " / ", dependent_var, " / ", current_period_name)
        )
        
        placebo_fit <- cv_pensynth(
          X1 = placebo_dat$X1, X0 = placebo_dat$X0,
          Z1 = placebo_dat$Z1, Z0 = placebo_dat$Z0,
          nlambda = lambda_placebo, verbose = FALSE
        )
        
        calculate_mspe(placebo_dat, placebo_fit, outcome_name = dependent_var,
                       return_full_data = FALSE) %>%
          mutate(unit = active_control_units[i])
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
      
      p_value_strong <- (1 + extreme_count_strong) / (1 + length(active_control_units))
      p_value_weak <- (1 + extreme_count_weak) / (1 + length(active_control_units))
      
      # Store results
      all_results[[paste0(dependent_var, "_", current_period_name)]] <- list(
        Y1 = dat$Y1plot,
        Y0 = dat$Y0plot,
        weights = weights,
        mspe_data = bind_rows(mspe, placebo_mspe_df) %>%
          left_join(names_key, by = c("unit" = "adm1_id")) %>%
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
