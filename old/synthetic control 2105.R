rm(list=ls())
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest, # twfe + sun and abraham event study
  broom
)
setwd("C:/Users/iddo2/Documents/thesis data/data/")

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)


#######################
grid <- fread("grid_long_data.csv") %>% 
  filter(year > 2003)

climate <- fread("2. intermediate/adm1_climatic_date.csv") %>% 
  filter(year(date) == 2012) %>% 
  group_by(adm1_id) %>% 
  summarise(
    avg_temperature = mean(avg_temperature),
    avg_surface_temp = mean(avg_surface_temp),
    avg_precipitation = mean(avg_precipitation),
    avg_u_wind = mean(avg_u_wind),
    avg_v_wind = mean(avg_v_wind)
  ) %>% 
  ungroup()

grid <- grid %>% 
  left_join(climate, by = "adm1_id")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

all_units <- grid %>% 
  dplyr::pull(adm1_id) %>% 
  unique()

treated_units <- grid %>% 
  filter(ADM1_EN %in% northern_regions_en) %>%
  dplyr::pull(adm1_id) %>% # Use pull() to get a vector
  unique()

control_units <- setdiff(all_units, treated_units)

###########
source("create_multi_synth_dataprep.R")

month <- grid %>%
  mutate(time_period = month(date)) %>% 
  group_by(time_period, adm1_id, year) %>%
  summarise(
    crop_share = mean(crop_share),
    tree_share = mean(tree_share),
    urban_share = mean(urban_share),
    avg_elevation = mean(avg_elevation),
    adm1_area = mean(adm1_area),
    avg_temperature = mean(avg_temperature),
    avg_surface_temp = mean(avg_surface_temp),
    avg_precipitation = mean(avg_precipitation),
    avg_u_wind = mean(avg_u_wind),
    avg_v_wind = mean(avg_v_wind),
    total_fires = sum(total_fires)
  ) %>% 
  ungroup() %>% 
  filter(time_period == 5) %>%
  # Create a sequential time ID for weeks
  arrange(year) %>%
  mutate(
    time_id_flt = as.numeric(factor(year)),
    asinh = asinh(total_fires)
  ) 

# Reorder columns for better readability

n_start <- month %>% 
  filter(year == 2012) %>%
  select(time_id_flt) %>%
  distinct() %>% 
  min() 

n_mid <- month %>% 
  filter(year == 2016) %>%
  select(time_id_flt) %>%
  distinct() %>% 
  min() 

n_end <- month %>% 
  select(time_id_flt) %>% 
  distinct() %>% 
  max() 

# --- Example Usage ---

# Define parameters matching your original Synth::dataprep call
predictors_vec = c(
  "asinh"
)
dependent_var = "asinh"
unit_var = "adm1_id"
time_var = "time_id_flt"
special_preds_list = list(
  list("crop_share", 1, "mean"),
  list("urban_share", 1, "mean"),
  list("tree_share", 1, "mean"),
  list("avg_elevation", 1, "mean"),
  list("adm1_area", 1, "mean"),
  list("avg_temperature", 1, "mean"),
  list("avg_surface_temp", 1, "mean"),
  list("avg_precipitation", 1, "mean"),
  list("avg_u_wind", 1, "mean"),
  list("avg_v_wind", 1, "mean")
  #list("gdp_2005", 1, "mean")
)
prior_time = 1:n_mid
opt_time = 1:n_mid
plot_time = 1:n_end #974 

dat <- create_multi_synth_dataprep(
  data = month,
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
  nlambda = 50,
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

#### evaluation ----

h <-diff_data %>% 
  mutate(period = if_else(time < n_2013, "before", "after")) %>% 
  group_by(unit, period) %>% 
  summarise(mean_diff = mean(difference), .groups = "drop") %>% 
  pivot_wider(names_from = period, values_from = mean_diff) %>%
  mutate(
    difference = abs(after - before), 
    before_closer_to_zero = ifelse(abs(after) < abs(before), 0, 1)
  ) %>% 
  select(unit, before, after, before_closer_to_zero) %>%
  summarise(
    mean(before_closer_to_zero, na.rm = T)
  ) %>% 
  pull()

print(paste("Share of units where 'before' is closer to 0 is:", h))

rm(h)


summary <- diff_data %>%
  #mutate(    year = round(time / 1, 0) + 2003 ) %>%
  group_by(time) %>%
  summarise(
    actual = mean(actual, na.rm = TRUE),
    synthetic = mean(synthetic, na.rm = TRUE),
    difference = mean(difference, na.rm = TRUE),
    .groups = 'drop'
  ) 

ggplot(diff_data %>% 
       filter(unit == 96)
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


###### join -----
diff_data_for_join <- diff_data %>% 
  mutate(
    adm1_id = as.integer(unit),
    time_id_flt = time
  ) %>% 
  left_join(month %>% 
              select(time_id_flt, date) %>% 
              distinct(), 
            by = "time_id_flt", 
  ) %>%
  select(-unit, -time, -time_id_flt)

grid_adm1_with_synthetic <- grid %>% 
  #select(adm1_id, time_id, ADM1_EN, total_fires) %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  left_join(
    diff_data_for_join, by = c("adm1_id", "date")
  ) 

synthetic_days_to_ban <- grid_adm1_with_synthetic %>% 
  mutate(post_treatment = as.factor(ifelse(year > 2016, 1, 0))) %>% 
  select(days_from_ban_start, days_from_ban_end, difference, year, post_treatment, ADM1_EN) %>%  
  filter(days_from_ban_end >= -30, days_from_ban_end <= 30) %>%
  group_by(days_from_ban_end, post_treatment) %>% 
  summarise(avg_diff = mean(difference, na.rm = T)) %>% 
  ungroup() 

ggplot() + 
  geom_line(data = synthetic_days_to_ban, 
            aes(
              x = days_from_ban_end, y = avg_diff,
              group = as.factor(post_treatment), color = as.factor(post_treatment)           
            )
  ) + 
  labs(title = "negative means actual is lower than synthetic") 

### placebo test
test <- placebo_test(fit, dat$Y1, dat$Y0)

post_treatment_idx_weak <- (n_start + 1):n_mid
post_treatment_idx_strong <- (n_mid + 1):n_end

# Subset E1 (treated) and E1s (placebos) to post-treatment
e1_weak <- test$E1[post_treatment_idx_weak]
e0_weak <- test$E0[post_treatment_idx_weak,]

e1_strong <- test$E1[post_treatment_idx_strong]
e0_strong <- test$E0[post_treatment_idx_strong,]

mean_ATE1_weak <- mean(e1_weak)
mean_ATE0_weak <- colMeans(e0_weak)

mean_ATE1_strong <- mean(e1_strong)
mean_ATE0_strong <- colMeans(e0_strong)

# Compute p-value for placebo test (sharp null)
p_val_weak <- (1 + sum(abs(mean_ATE0_weak) >= abs(mean_ATE1_weak))) / (length(mean_ATE0_weak) + 1)
p_val_strong <- (1 + sum(abs(mean_ATE0_strong) >= abs(mean_ATE1_strong))) / (length(mean_ATE0_strong) + 1)
