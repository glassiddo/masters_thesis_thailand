pm_threshold <- 37.5

pollition_no_na <- pollution_data_long %>%
  mutate(year = year(Date), month = month(Date)) %>%
  filter(!is.na(PM2.5), (month < 6))

pol14 <- pollition_no_na %>% 
  filter(year == 2015, province != "Songkhla") %>% 
  select(province) %>% 
  distinct() %>% pull()

adm_summary <- pollition_no_na %>% 
  filter(province %in% pol14, year >= 2015) %>% 
  group_by(province, year) %>%
  summarise(
    total_days = n(),
    days_above_threshold = sum(PM2.5 > pm_threshold, na.rm = TRUE),
    days_with_data = sum(!is.na(PM2.5)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_above_threshold = round((days_above_threshold / days_with_data) * 100, 1),
    adm1_id = as.numeric(as.factor(province))
  ) %>% 
  select(adm1_id, year, pct_above_threshold, province)

all_units <- adm_summary %>% 
  dplyr::pull(adm1_id) %>% 
  unique()

treated_units <- adm_summary %>% 
  filter(province %in% northern_regions_en) %>%
  dplyr::pull(adm1_id) %>% # Use pull() to get a vector
  unique()

control_units <- setdiff(all_units, treated_units)

###########
source("create_multi_synth_dataprep.R")

# Define parameters matching your original Synth::dataprep call
predictors_vec = c(
  "pct_above_threshold"
)
dependent_var = "pct_above_threshold"
unit_var = "adm1_id"
time_var = "year"
special_preds_list = list()
prior_time = 2015:2016
opt_time = 2015:2016
plot_time = 2015:2021 #974 

dat <- create_multi_synth_dataprep(
  data = adm_summary,
  predictors = predictors_vec,
  special.predictors = special_preds_list,
  dependent = dependent_var,
  unit.variable = unit_var,
  time.variable = time_var,
  treatment.identifier = treated_units,
  controls.identifier = control_units,
  time.predictors.prior = prior_time,
  time.optimize.ssr = opt_time,
  time.plot = plot_time
)

rm(special_preds_list, opt_time, prior_time, 
   plot_time, time_var, unit_var, dependent_var, predictors_vec)

### use it (after external function) ----
library(pensynth)
fit <- cv_pensynth(
  X1 = dat$X1, # Treated unit covariates
  X0 = dat$X0, # Donor unit covariates
  Z1 = dat$Z1, # Treated unit hold-out values (pre-intervention)
  Z0 = dat$Z0, # Donor unit hold-out values
  nlambda = 1,
  verbose = FALSE
)

synth_matrix <- dat$Y0plot %*% fit$w_opt
colnames(synth_matrix) <- colnames(dat$Y1plot)

# Combine treated and synthetic data in one operation
diff_data <- bind_rows(
  # Treated data with time column
  as_tibble(dat$Y1plot) %>%
    mutate(time = row_number()) %>%
    pivot_longer(cols = -time, names_to = "unit", values_to = "value") %>%
    mutate(type = "actual"),
  
  # Synthetic data with time column  
  as_tibble(synth_matrix) %>%
    mutate(time = row_number()) %>%
    pivot_longer(cols = -time, names_to = "unit", values_to = "value") %>%
    mutate(type = "synthetic", value = value) #round(value, 0))
) %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(difference = actual - synthetic)

ggplot(diff_data %>% 
         filter(unit == 2)
       , aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = synthetic, color = "Synthetic")) +
  scale_color_manual(values = c("Actual" = "#2E86AB", "Synthetic" = "#A23B72")) +
  #geom_line(aes(y = difference, color = "Difference")) +
  labs(
    title = "Average Actual vs Synthetic Values by Year",
    x = "Year Number",
    y = "Average Value",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
