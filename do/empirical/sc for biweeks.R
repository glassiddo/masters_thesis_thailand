compute_synth_results <- function(dat, fit, outcome_name, ban_lookup, biweek_num) {

  Y1 <- dat$Y1plot
  Y0 <- dat$Y0plot
  W <- fit$w_opt
  unit_names <- colnames(Y1)
  n_time <- nrow(Y1)  
  
  synth_matrix <- Y0 %*% W
  colnames(synth_matrix) <- unit_names
  
  # individual unit results for pooling
  unit_results <- purrr::map_dfr(unit_names, function(unit) {
    actual <- Y1[, unit]
    synth <- synth_matrix[, unit]
    
    unit_ban_data <- ban_lookup %>%
      filter(adm1_id == unit) %>%
      arrange(year_id)
    
    pre_mspe <- #sqrt(
      mean((actual[1:n_start] - synth[1:n_start])^2)
    #)
    
    # Calculate for each year
    purrr::map_dfr(seq_len(n_time), function(year_idx) {
      if (year_idx > nrow(unit_ban_data) || is.na(unit_ban_data$ban_status[year_idx])) {
        return(NULL)
      }
      
      year_status <- unit_ban_data$ban_status[year_idx]
      actual_year <- year_idx + 2003
      
      post_mspe <- 
        #sqrt(
        mean((actual[year_idx] - synth[year_idx])^2)
      #)
      treatment_effect <- actual[year_idx] - synth[year_idx]
      
      tibble::tibble(
        unit = unit,
        year = actual_year,
        ban_status = year_status,
        pre_mspe = pre_mspe,
        post_mspe = post_mspe,
        ratio = post_mspe / pre_mspe,
        treatment_effect = treatment_effect,
        outcome = outcome_name,
        n_treated_units = 1,  # Each unit contributes 1 to the count
        biweek_num = biweek_num  
      )
    })
  })
  
  # Calculate placebo results for p-values
  placebo_results <- lapply(seq_along(control_units), function(i) {
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
      nlambda = 1, verbose = FALSE
    )
    
    # Calculate placebo results for each year
    placebo_Y1 <- placebo_dat$Y1plot
    placebo_Y0 <- placebo_dat$Y0plot
    placebo_W <- placebo_fit$w_opt
    placebo_synth <- placebo_Y0 %*% placebo_W
    
    placebo_pre_mspe <- 
      #sqrt(
        mean((placebo_Y1[1:n_start] - placebo_synth[1:n_start])^2)
    #)
    
    # Calculate year-wise placebo ratios
    placebo_year_ratios <- sapply(seq_len(n_time), function(year_idx) {
      placebo_post_mspe <- 
        #sqrt(
          mean((placebo_Y1[year_idx] - placebo_synth[year_idx])^2)
        #  )
      placebo_post_mspe / placebo_pre_mspe
    })
    
    tibble::tibble(
      placebo_unit = colnames(dat$Y0plot)[i],
      year_idx = seq_len(n_time),
      placebo_ratio = placebo_year_ratios
    )
  })
  
  placebo_df <- bind_rows(placebo_results)
  
  # Pool results by year and ban_status, then calculate p-values
  pooled_results <- unit_results %>%
    group_by(year, ban_status, outcome, biweek_num) %>%
    summarise(
      n_treated_units = sum(n_treated_units),
      pre_mspe = sum(pre_mspe * n_treated_units) / sum(n_treated_units),
      post_mspe = sum(post_mspe * n_treated_units) / sum(n_treated_units),
      treatment_effect = sum(treatment_effect * n_treated_units) / sum(n_treated_units),
      .groups = 'drop'
    ) %>%
    mutate(
      # Calculate ratio AFTER pooling MSPEs
      ratio = post_mspe / pre_mspe,
    ) %>%
    rowwise() %>%
    mutate(
      # Calculate p-value for each pooled result
      p_value = {
        year_idx <- year - 2003
        if (year_idx > 0 && year_idx <= max(placebo_df$year_idx)) {
          placebo_ratios_year <- placebo_df$placebo_ratio[placebo_df$year_idx == year_idx]
          extreme_count <- sum(placebo_ratios_year >= ratio, na.rm = TRUE)
          (1 + extreme_count) / (1 + length(control_units))
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  
  return(pooled_results)
}

bans <- fread("2. intermediate/bans_cleaned_1806.csv") %>% 
  select(-Ban_start, -Ban_end) 
#filter(year > 2016)

all_outputs <- list()

for (i in 1:9) {
  # 1. Create summary for this biweek
  biweek_summary <- create_data_summary(
    period_name = paste0("Biweek ", i),
    period_type = "half_month",
    period_values = i
  )
  
  # 2. Join with ban info
  bi_with_ban <- biweek_summary %>%
    mutate(biweek_num = i) %>%
    left_join(
      bans %>% select(-VeryLikely),
      by = c("ADM1_EN", "year")
    ) %>%
    mutate(
      biweek_relative_to_ban_start = case_when(
        is.na(halfmonth_ban_start) ~ NA,
        TRUE ~ biweek_num - halfmonth_ban_start
      ),
      biweek_relative_to_ban_end = case_when(
        is.na(halfmonth_ban_end) ~ NA,
        TRUE ~ biweek_num - halfmonth_ban_end
      ),
      ban_status = case_when(
        is.na(halfmonth_ban_start) ~ NA_character_,
        biweek_relative_to_ban_start == -1 ~ "just before",
        biweek_relative_to_ban_end == 1 ~ "just after",
        biweek_num >= halfmonth_ban_start & biweek_num <= halfmonth_ban_end ~ "during"
      )
    ) %>%
    select(-biweek_num, -halfmonth_ban_start, -halfmonth_ban_end)
  
  # 3. Create ban_lookup
  ban_lookup <- bi_with_ban %>%
    filter(ADM1_EN %in% northern_regions_en) %>% 
    select(adm1_id, year_id, ban_status) %>%
    distinct()
  
  # 4. Run synthetic control prep
  dat <- create_multi_synth_dataprep(
    data = bi_with_ban,
    predictors = "total_fires",
    special.predictors = synth_special_predictors,
    dependent = "total_fires",
    unit.variable = "adm1_id",
    time.variable = "year_id",
    treatment.identifier = treated_units,
    controls.identifier = control_units,
    time.predictors.prior = 1:n_start,
    time.optimize.ssr = 1:n_start,
    time.plot = 1:n_end
  )
  
  # 5. Fit penalized synth
  fit <- cv_pensynth(
    X1 = dat$X1, X0 = dat$X0,
    Z1 = dat$Z1, Z0 = dat$Z0,
    nlambda = 10, verbose = FALSE
  )
  
  # 6. Compute results
  output <- compute_synth_results(
    dat, fit,
    outcome_name = "total_fires", 
    ban_lookup = ban_lookup,
    biweek_num = i
  )
  
  # 7. Store
  all_outputs[[i]] <- output
}

# 8. Combine all results
output_all <- bind_rows(all_outputs)

weighted_output <- output_all %>% 
  mutate(
    period = ifelse(year > 2016, "strict", "weak")
  ) %>% 
  group_by(period, outcome, ban_status) %>% 
  summarise(
    weighted_treatment_effect = weighted.mean(treatment_effect, n_treated_units, na.rm = TRUE),
    weighted_p_value = weighted.mean(p_value, n_treated_units, na.rm = TRUE),
    total_units = sum(n_treated_units),
    .groups = "drop"
  ) 

print(weighted_output)
