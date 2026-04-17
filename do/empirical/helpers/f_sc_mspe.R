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
    indices <- 1:n_time
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
      diff_data = long_data |> 
        pivot_wider(names_from = type, values_from = value) |>
        mutate(difference = actual - synthetic)
    )
  } else {
    results
  }
}
