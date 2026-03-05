rm(list=ls())
#dev.off()
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest, # twfe + sun and abraham event study
  Synth, pensynth
)
#install.packages("pensynth", repos = c("https://vankesteren.r-universe.dev", "https://cloud.r-project.org"))

setwd("C:/Users/iddo2/Documents/thesis data/data/")

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

# grid <- fread("temp_grid_1904.csv") %>% #%>% filter(ADM1_EN %in% northern_regions_en)
#   filter(year > 2011)

adm <- read_sf("thai admin units/admin3_simplified_en.shp") %>% 
  mutate(
    adm3_id = row_number()
  ) %>% 
  st_set_geometry(NULL) %>% 
  select(adm3_id, ADM1_EN, ADM2_EN, ADM3_EN)

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

pop <- fread("population/population_by_adm3.csv") %>% select(-V1) %>% 
  mutate(adm3_id = as.character(adm3_id))

pop_my <- fread("population/population_myanmar_cleaned.csv") %>% select(-V1) %>% 
  mutate(adm3_id = as.character(paste0(adm3_id_my, "-my"))) %>% 
  select(-adm3_id_my)

climate <- fread("wind etc/adm3_climatic_date.csv") %>% 
  select(-ADM0_EN, -ADM1_EN, -ADM2_EN, -ADM3_EN) %>% 
  rename(adm3_id = id) %>% 
  mutate(year = as.integer(year(date)), date = as.Date(date)) %>% 
  mutate(adm3_id = as.character(adm3_id))

climate_my <- fread("wind etc/adm3_climatic_date_my.csv") %>% 
  select(-V1, -ADM1_EN, -ADM2_EN, -ADM3_EN) %>% 
  rename(adm3_id = id) %>% 
  mutate(year = as.integer(year(date)), date = as.Date(date)) %>% 
  mutate(adm3_id = as.character(paste0(adm3_id, "-my")))

climate <- bind_rows(climate, climate_my)
pop <- bind_rows(pop, pop_my)

fwrite(pop, "population/pop_my_th_merged.csv")
fwrite(climate, "wind etc/climate_my_th_merged.csv")

pop <- fread("population/pop_my_th_merged.csv") 
climate <- fread("wind etc/climate_my_th_merged.csv") %>% 
  mutate(date = as.Date(date)) 

# ive deleted the original thai climate - can be recreated with the climate code

grid_mixed_updated <- grid_mixed %>% 
  as.tibble() %>% 
  left_join(pop %>% select(-ADM1_EN, -ADM2_EN, -ADM3_EN), by = "adm3_id") %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(climate, by = c("adm3_id", "year", "date")) %>%
  group_by(adm3_id) %>%
  mutate(time_id = row_number()) %>%
  ungroup() %>% 
  #drop_na() %>%
  as.data.frame()


adm <- read_sf("thai admin units/admin3_simplified_en.shp") %>% 
  mutate(
    adm3_id = row_number()
  ) %>% 
  select(adm3_id) %>%
  mutate(
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2]
  ) %>% 
  st_set_geometry(NULL) %>% 
  select(-centroid) %>% 
  mutate(adm3_id = as.character(adm3_id))

adm_my <- my3 %>% 
  mutate(adm3_id = as.character(paste0(adm3_id_my, "-my"))) %>% 
  select(adm3_id) %>%
  mutate(
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2]
  ) %>% 
  st_set_geometry(NULL) %>% 
  select(-centroid)

adm_merged <- bind_rows(adm, adm_my)

grid_mixed_updated <- grid_mixed_updated %>% 
  left_join(adm_merged, by = "adm3_id")

fwrite(grid_mixed_updated, "grid_mixed0805.csv")

rm(adm, adm_merged, adm_my, adm1, my3, my_adm1, 
   grid, grid_mixed, grid_my, modis_my, pop, climate)

#### synthetic ---------

# Now that we have a complete dataset with no missing values, we can run the synthetic control analysis
# Define control units - all admin units except Chiang Mai (treatment unit)
# all_admin_ids <- unique(wsh_filled$adm1_id)
# control_units <- setdiff(all_admin_ids, c(3, 4, 7, 8, 11, 13, 16, 20, 22))
grid_mixed_updated <- grid_mixed_updated %>% 
  filter(!is.na(avg_temperature), !is.na(Total_pop))

grid_mixed_updated <- grid_mixed_updated %>% 
  select(-ADM0_EN, -days_from_ban_start, -days_from_ban_end, Ban_start, Ban_end)

g <- lazy_dt(grid_mixed_updated) %>% 
  group_by(time_id, ADM2_EN, ADM1_EN) %>% 
  summarise(
    n_fires_modis =    sum(n_fires_modis),
    Total_pop = sum(Total_pop),
    Male_pop = sum(Male_pop),
    Female_pop = sum(Female_pop),
    HH_num = sum(HH_num),
    avg_temperature = mean(avg_temperature),
    avg_surface_temp  = mean(avg_v_wind),
    avg_precipitation = mean(avg_v_wind),
    avg_u_wind =mean(avg_v_wind),
    avg_v_wind = mean(avg_v_wind), 
    x = mean(x),
    y = mean(y)
  ) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  mutate(
    adm2_id = as.character(ADM2_EN)
  )

#all_adm3_ids <- unique(grid_mixed_updated$adm3_id)

########### using grid ----------
grid_adm1 <- grid_adm1 %>%
  group_by(ADM1_EN) %>%
  mutate(adm1_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(date) %>% 
  mutate(time_id = cur_group_id()) %>%
  ungroup() %>% 
  select(adm1_id, time_id, everything())

all_units <- grid_adm1 %>% 
  pull(adm1_id) %>% 
  unique()

# Define the multiple treated units
treated_units <- grid_adm1 %>% 
  filter(ADM1_EN %in% northern_regions_en) %>%
  pull(adm1_id) %>% # Use pull() to get a vector
  unique()

control_units <- setdiff(all_units, treated_units)

rm(all_adm3_ids, northern_adm3_ids, non_north, dates, all_units)


library(pensynth)
fit <- cv_pensynth(
  X1 = dataprep_multi_out$X1, # Treated unit covariates
  X0 = dataprep_multi_out$X0, # Donor unit covariates
  Z1 = dataprep_multi_out$Z1, # Treated unit hold-out values (pre-intervention)
  Z0 = dataprep_multi_out$Z0, # Donor unit hold-out values
  nlambda = 100,
  verbose = FALSE
)

synth_matrix <- dataprep_multi_out$Y0plot %*% fit$w_opt
colnames(synth_matrix) <- colnames(dataprep_multi_out$Y1plot)

# Combine treated and synthetic data in one operation
plot_data <- bind_rows(
  # Treated data with time column
  as_tibble(dataprep_multi_out$Y1plot) %>%
    mutate(time = row_number()) %>%
    pivot_longer(cols = -time, names_to = "unit", values_to = "value") %>%
    mutate(type = "actual"),
  
  # Synthetic data with time column  
  as_tibble(synth_matrix) %>%
    mutate(time = row_number()) %>%
    pivot_longer(cols = -time, names_to = "unit", values_to = "value") %>%
    mutate(type = "synthetic", value = round(value, 0))
)

ggplot(plot_data %>% filter(unit == "6"), aes(x = time, y = value, color = type, linetype = unit)) +
  geom_line() +
  geom_vline(xintercept = 1096, linetype = "dashed", color = "black") +
  labs(x = "Time", y = "Outcome", title = "Actual vs Synthetic Treated Outcomes") +
  theme_minimal()

diff_data <- plot_data %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(difference = actual - synthetic)

# Plot the differences
ggplot(diff_data, aes(x = time, y = difference, color = unit)) +
  geom_line() +
  geom_vline(xintercept = 1096, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Difference between Actual and Synthetic Control",
    x = "Time Period",
    y = "Difference (Actual - Synthetic)",
    color = "Unit"
  ) +
  theme_minimal()

h <- diff_data %>% 
  mutate(period = if_else(time < 1096, "before", "after")) %>% 
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


bans <- fread("1. raw/bans/ban dates.csv") %>%
  select(Region, `Ban start`, `Ban end`) %>%
  rename(ADM1_EN = Region,
         Ban_start = `Ban start`, 
         Ban_end = `Ban end`) %>%
  mutate(Ban_start = as.Date(Ban_start, format = "%Y-%m-%d"),
         Ban_end = as.Date(Ban_end, format = "%Y-%m-%d"),
         year = lubridate::year(Ban_start)) %>% 
  filter(!is.na(Ban_start)) %>% 
  filter(year < 2021, year > 2015) %>% 
  select(ADM1_EN, Ban_start, Ban_end, year) 
  
row_2016 <- bans[bans$year == 2016, ]
start_month_day <- format(row_2016$Ban_start, format="%m-%d")
end_month_day <- format(row_2016$Ban_end, format="%m-%d")

# Create new rows for 2012-2015
new_years <- 2015:2012
new_rows <- data.frame(
  Ban_start = as.Date(paste0(new_years, "-", start_month_day)),
  Ban_end = as.Date(paste0(new_years, "-", end_month_day)),
  year = new_years
)

# Combine with original data frame
bans_updated <- rbind(bans, new_rows)

dates_time <- grid %>% 
  select(date, time_id) %>% 
  rename(time = time_id) %>% 
  distinct()

adm1_no_cm <- northern_regions_en[northern_regions_en != "Chiang Mai"]
m <- g %>%
  filter(!ADM1_EN %in% adm1_no_cm) %>% 
  left_join(dates_time, by = "time_id") %>% 
  mutate(year = year(date)) %>% 
  left_join(bans_updated, by = "year") %>% 
  group_by(year) %>%
  mutate(
    days_from_ban_start = as.integer(date - Ban_start),
    days_from_ban_end = as.integer(date - Ban_end)
  ) %>%
  ungroup() %>% 
  mutate(
    is_ban = ifelse(days_from_ban_start >= 0 & days_from_ban_end < 0, 1, 0),
    is_ban_over = ifelse(days_from_ban_end >= 0, 1, 0),
    real_treat = ifelse(is_ban == 1 & year > 2015 #& type == "actual"
                        & ADM1_EN == "Chiang Mai"
                        , 1, 0),
    is_ever_treated = ifelse(ADM1_EN == "Chiang Mai", 1, 0)
  )

feols(
  n_fires_modis ~ real_treat | date + ADM1_EN
  ,
  data = m
)

h_bin_data <- m %>%
  #filter(year > 2015) %>% 
  mutate(
    days_bin = cut(days_from_ban_end, 
                   breaks = seq(-15, 20, by = 5),
                   labels = paste(seq(-15, 15, by = 5), "to", seq(-10, 20, by = 5) - 1),
                   include.lowest = TRUE)
  ) %>%
  group_by(year, days_bin, ADM1_EN, ADM2_EN, is_ever_treated) %>%
  summarise(
    n_fires_modis = sum(n_fires_modis), 
    #fires_rel = mean(fires_rel, na.rm = TRUE),
    .groups = "drop"
  ) 

# Create a list to store models for each bin
bin_models <- list()

unique_bins <- unique(h_bin_data$days_bin)
unique_bins <- unique_bins[!is.na(unique_bins)]

# Run separate models for each bin
for(bin in unique_bins) {
  bin_data <- h_bin_data %>% filter(days_bin == bin)
  
  bin_models[[as.character(bin)]] <- feols(
    n_fires_modis ~ is_ever_treated*as.factor(year),# | year,
    data = bin_data
  )
}

bin_models

