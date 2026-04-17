source("do/empirical/sc_01_prepare.R")
source("do/empirical/helpers/f_sc_run_synth_analysis.R")

# the higher the lambda, the slower it is
# they are important for p-values - use 10 as default 
# reduce to 1 if just plotting
lambda_treated <- 10
lambda_placebo <- 10 

# execute analysis
northern_regions_ids <- names_key |> 
  filter(ADM1_EN %in% northern_regions_en) |> 
  pull(adm1_id) |> 
  as.numeric()

analysis_results <- run_synth_analysis(
  outcomes_list = outcomes_list,
  datasets_to_run = datasets_to_run,
  treated_units = treated_units,
  control_units = control_units,
  n_start = n_start,
  n_end = n_end,
  names_key = names_key,
  synth_special_predictors = synth_special_predictors,
  n_begin = n_begin,
  lambda_treated = lambda_treated,
  lambda_placebo = lambda_placebo
)

results <- analysis_results$summary_table 
print(results)
