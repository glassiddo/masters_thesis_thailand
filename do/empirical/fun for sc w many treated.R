#' based on pensynth package, allowing for multiple treated units
#' dont recommend using it outisde of this paper before validating that it is indeed correct
#'
#' Create Synth-style Data Matrices for Multiple Treated Units
#'
#' Replicates the logic and output structure of Synth::dataprep but allows
#' for multiple treated units in the X1, Z1, and Y1plot matrices.
#'
#' @param data Long-format data frame (e.g., your 'wsh' data).
#' @param predictors Character vector of time-varying predictor variable names.
#' @param special.predictors List of lists defining special predictors.
#'   Each inner list should be: list(variable_name, time_point, aggregation_function).
#'   Example: list("Total_pop", 1, "mean")
#' @param dependent Character string for the dependent variable name.
#' @param unit.variable Character string for the unit identifier column name.
#' @param time.variable Character string for the time identifier column name.
#' @param treatment.identifier Numeric or character vector of treated unit IDs.
#' @param controls.identifier Numeric or character vector of control unit IDs.
#' @param time.predictors.prior Numeric vector of time IDs for predictor averaging period.
#' @param time.optimize.ssr Numeric vector of time IDs for the outcome optimization period (Z matrices).
#' @param time.plot Numeric vector of time IDs for the full outcome plotting period (Y matrices).
#' @param unit.names.variable Optional: Character string for unit names column (not used in matrix creation but could be added).
#'
#' @return A list containing matrices: X0, X1, Z0, Z1, Y0plot, Y1plot.
#'
create_multi_synth_dataprep <- function(data,
                                        predictors,
                                        special.predictors,
                                        dependent,
                                        unit.variable,
                                        time.variable,
                                        treatment.identifier,
                                        controls.identifier,
                                        time.predictors.prior,
                                        time.optimize.ssr,
                                        time.plot,
                                        unit.names.variable = NULL) {
  
  # --- Argument Type Conversion ---
  # Ensure identifiers are character for consistent indexing later
  treatment.identifier <- as.character(treatment.identifier)
  controls.identifier <- as.character(controls.identifier)
  data[[unit.variable]] <- as.character(data[[unit.variable]])
  all.units <- c(treatment.identifier, controls.identifier)
  
  # --- 1. Prepare Special Predictors (taken at specific time points) ---
  special_preds_list <- list()
  special_pred_names <- c() # Store the generated names like "special.Var.Time"
  
  if (length(special.predictors) > 0) {
    for (i in seq_along(special.predictors)) {
      sp <- special.predictors[[i]]
      var_name <- sp[[1]]
      tp <- sp[[2]]
      # Aggregation function (sp[[3]]) is often "mean", which for a single time point
      # per unit just selects the value. We implement this selection directly.
      new_name <- paste0("special.", var_name, ".", tp)
      special_pred_names <- c(special_pred_names, new_name)
      
      sp_data <- data %>%
        filter(.data[[time.variable]] == tp,
               .data[[unit.variable]] %in% all.units) %>%
        select(all_of(c(unit.variable, var_name))) %>%
        rename(!!new_name := all_of(var_name)) %>%
        distinct(.data[[unit.variable]], .keep_all = TRUE) # Ensure one value per unit
      
      if (nrow(sp_data) == 0) {
        warning(paste("No data found for special predictor", var_name, "at time", tp))
      } else {
        special_preds_list[[new_name]] <- sp_data
      }
    }
    
    # Join all special predictors together
    if (length(special_preds_list) > 0) {
      special_preds_df <- Reduce(function(df1, df2) full_join(df1, df2, by = unit.variable),
                                 special_preds_list)
    } else {
      # Create an empty df with the unit variable if no special predictors worked
      special_preds_df <- data %>% distinct(.data[[unit.variable]]) %>% select(all_of(unit.variable))
    }
    
  } else {
    # Create an empty df with the unit variable if no special predictors specified
    special_preds_df <- data %>% distinct(.data[[unit.variable]]) %>% select(all_of(unit.variable))
    special_pred_names <- character(0) # Ensure it's an empty character vector
  }
  
  
  # --- 2. Prepare Time-Varying Predictors (averaged over prior period) ---
  time_varying_preds_avg <- data %>%
    filter(.data[[time.variable]] %in% time.predictors.prior,
           .data[[unit.variable]] %in% all.units) %>%
    select(all_of(c(unit.variable, predictors))) %>%
    group_by(.data[[unit.variable]]) %>%
    summarise(across(all_of(predictors), ~ mean(.x, na.rm = TRUE)), .groups = "drop") # Use ~ mean(.x) syntax
  
  # --- 3. Combine Predictors ---
  # Use left_join to keep all units from time_varying summary
  # (assuming all units have data in the prior period)
  all_predictors_df <- left_join(time_varying_preds_avg, special_preds_df, by = unit.variable)
  
  # Check for units completely missing special predictors and fill columns with NA if needed
  missing_special_cols <- setdiff(special_pred_names, names(all_predictors_df))
  if(length(missing_special_cols) > 0){
    for(col in missing_special_cols){
      all_predictors_df[[col]] <- NA_real_
    }
  }
  
  
  # --- 4. Create X0 and X1 (Predictor Matrices) ---
  # Ensure predictor order matches input + special predictors
  final_predictor_order <- c(predictors, special_pred_names)
  
  # Handle cases where some predictors might be missing entirely after processing
  final_predictor_order <- intersect(final_predictor_order, names(all_predictors_df))
  
  X_all <- all_predictors_df %>%
    filter(.data[[unit.variable]] %in% all.units) %>%
    # Reorder columns (predictors) then transpose
    select(all_of(unit.variable), all_of(final_predictor_order)) %>%
    pivot_longer(-all_of(unit.variable), names_to = "predictor", values_to = "value") %>%
    # Ensure factor levels match desired predictor order before pivoting
    mutate(predictor = factor(.data$predictor, levels = final_predictor_order)) %>%
    arrange(.data$predictor) %>%
    pivot_wider(names_from = all_of(unit.variable), values_from = "value") %>%
    tibble::column_to_rownames(var = "predictor") %>%
    # Ensure columns are in the order: treated, controls (or specific order if needed)
    select(all_of(treatment.identifier), all_of(controls.identifier)) %>%
    as.matrix()
  
  # Extract X0 and X1 based on column names
  X1 <- X_all[, treatment.identifier, drop = FALSE] # Will have multiple columns
  X0 <- X_all[, controls.identifier, drop = FALSE]
  
  
  # --- 5. Create Z0 and Z1 (Outcome Matrices - Optimization Period) ---
  Z_all <- data %>%
    filter(.data[[time.variable]] %in% time.optimize.ssr,
           .data[[unit.variable]] %in% all.units) %>%
    select(all_of(c(time.variable, unit.variable, dependent))) %>%
    # Ensure time periods are rows, units are columns
    pivot_wider(names_from = all_of(unit.variable), values_from = all_of(dependent)) %>%
    arrange(.data[[time.variable]]) %>% # Ensure time order
    # Keep time variable for potential rownames, then select unit columns
    select(all_of(time.variable), all_of(treatment.identifier), all_of(controls.identifier)) %>%
    tibble::column_to_rownames(var = time.variable) %>%
    as.matrix()
  
  Z1 <- Z_all[, treatment.identifier, drop = FALSE] # Multiple columns
  Z0 <- Z_all[, controls.identifier, drop = FALSE]
  
  
  # --- 6. Create Y0plot and Y1plot (Outcome Matrices - Plot Period) ---
  Y_all <- data %>%
    filter(.data[[time.variable]] %in% time.plot,
           .data[[unit.variable]] %in% all.units) %>%
    select(all_of(c(time.variable, unit.variable, dependent))) %>%
    pivot_wider(names_from = all_of(unit.variable), values_from = all_of(dependent)) %>%
    arrange(.data[[time.variable]]) %>% # Ensure time order
    select(all_of(time.variable), all_of(treatment.identifier), all_of(controls.identifier)) %>%
    tibble::column_to_rownames(var = time.variable) %>%
    as.matrix()
  
  Y1plot <- Y_all[, treatment.identifier, drop = FALSE] # Multiple columns
  Y0plot <- Y_all[, controls.identifier, drop = FALSE]
  
  
  # --- 7. Return List ---
  # Mimic the structure of Synth::dataprep output
  output <- list(
    X0 = X0, X1 = X1,
    Z0 = Z0, Z1 = Z1,
    Y0plot = Y0plot, Y1plot = Y1plot
    # Note: Synth::dataprep includes other elements like 'names.and.numbers',
    # 'tag', etc. which are not replicated here but could be added if needed.
  )
  
  return(output)
}


dump("create_multi_synth_dataprep", file = "create_multi_synth_dataprep.R")
