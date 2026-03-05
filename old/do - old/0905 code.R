rm(list=ls())
#dev.off()
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest # twfe + sun and abraham event study
)
setwd("C:/Users/iddo2/Documents/thesis data/data/")

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

### run "fake bans 1105" before 

g <-  
  dtplyr::lazy_dt(grid) %>% 
    mutate(
      ### take one admin level lower for myanmar as equivaent to thailand
      ADM1_EN = ifelse(ADM0_EN == "", ADM2_EN, ADM1_EN), # replace ADM1_EN of myanmar with adm2
      ADM2_EN = ifelse(ADM0_EN == "", ADM3_EN, ADM2_EN) # replace adm2_en of myanmar with adm3
      # OPPOSITE FOR LAOS
    ) %>% 
    group_by(ADM0_EN, ADM1_EN, ADM2_EN, time_id, year) %>% 
    summarise(
      Total_pop = sum(Total_pop),
      avg_temperature = mean(avg_temperature),
      is_ban = mean(is_ban),
      is_ban_over = mean(is_ban_over),
      days_from_ban_start = mean(days_from_ban_start),
      days_from_ban_end = mean(days_from_ban_end),
      avg_surface_temp = mean(avg_surface_temp),
      avg_precipitation = mean(avg_precipitation),
      avg_u_wind = mean(avg_u_wind),
      avg_v_wind = mean(avg_v_wind),
      x = mean(x),
      y = mean(y),
      n_fires_modis = sum(n_fires_modis),
      .groups = 'drop'
    ) %>% 
  mutate(
    is_treated = ifelse(ADM1_EN %in% northern_regions_en & 
                                   #time_id > 726
                                   year > 2016
                                 , 1, 0),
    type = case_when(
        ADM0_EN == "Thailand" & !ADM1_EN %in% northern_regions_en ~ "Thai NT",
        ADM0_EN == "Thailand" & ADM1_EN %in% northern_regions_en ~ "Treated",
        ADM0_EN == "Laos" ~ "Laos",
        TRUE ~ "Myanmar"
    )
  ) %>% 
  group_by(ADM1_EN, ADM2_EN) %>%
  mutate(ADM2_id = cur_group_id()) %>%
  ungroup() %>%
  # filter(
  #   !str_starts(ADM1_EN, "Yangon") # much more urban area
  # ) %>%
  as.data.frame()

rm(grid)

g_adm1 <-  
  dtplyr::lazy_dt(g) %>% 
  group_by(ADM0_EN, ADM1_EN, time_id, year) %>% 
  summarise(
    Total_pop = sum(Total_pop),
    avg_temperature = mean(avg_temperature),
    is_ban = mean(is_ban),
    is_ban_over = mean(is_ban_over),
    days_from_ban_start = mean(days_from_ban_start),
    days_from_ban_end = mean(days_from_ban_end),
    avg_surface_temp = mean(avg_surface_temp),
    avg_precipitation = mean(avg_precipitation),
    avg_u_wind = mean(avg_u_wind),
    avg_v_wind = mean(avg_v_wind),
    x = mean(x),
    y = mean(y),
    n_fires_modis = sum(n_fires_modis),
    .groups = 'drop'
  ) %>% 
  mutate(
    is_treated = ifelse(ADM1_EN %in% northern_regions_en & 
                          #time_id > 726
                          year > 2016
                        , 1, 0),
    type = case_when(
      ADM0_EN == "Thailand" & !ADM1_EN %in% northern_regions_en ~ "Thai NT",
      ADM0_EN == "Thailand" & ADM1_EN %in% northern_regions_en ~ "Treated",
      ADM0_EN == "Laos" ~ "Laos",
      TRUE ~ "Myanmar"
    )
  ) %>% 
  group_by(ADM0_EN, ADM1_EN) %>%
  mutate(ADM1_id = as.integer(cur_group_id())) %>%
  ungroup() %>% 
  as.data.frame()

##############
bin_start <- -15
bin_end <- 15
bin_space <- 3
breaks_seq <- seq(bin_start, bin_end, by = bin_space)
labels_seq <- paste(head(breaks_seq, -1), "to", tail(breaks_seq, -1) - 1)

h_bin_data <- g_adm1 %>%
  #filter(ADM0_EN == "Laos" | ADM1_EN %in% northern_regions_en) %>%
  mutate(
    days_bin = cut(days_from_ban_end, 
                   breaks = breaks_seq,
                   labels = labels_seq,
                   include.lowest = TRUE)
  ) %>% 
  filter(!is.na(days_bin)) 

# Create a list to store models for each bin
bin_models <- list()
unique_bins <- unique(h_bin_data$days_bin)
unique_bins <- unique_bins[!is.na(unique_bins)]

# Run separate models for each bin
for(bin in unique_bins) {
  bin_data <- h_bin_data %>% filter(days_bin == bin)
  
  bin_models[[as.character(bin)]] <- feols(
    n_fires_modis ~ is_treated + 
      Total_pop + avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind
    | 
      year +
      ADM1_EN,
    data = bin_data,
    cluster = ~ADM1_EN
  )
}

#bin_models
library(broom)
results <- lapply(names(bin_models), function(bin) {
  model_summary <- tidy(bin_models[[bin]])
  treated_row <- model_summary[model_summary$term == "is_treated", ]
  treated_row$bin <- bin  # keep track of the bin
  return(treated_row)
})

results_df <- do.call(rbind, results)
results_df <- results_df[, c("bin", "estimate", "std.error", "statistic", "p.value")]
results_df$stars <- symnum(results_df$p.value, corr = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", ""))
print(results_df, n = 100)

##############
feols(
  n_fires_modis ~ is_ban + 
    Total_pop + 
    avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind | 
    ADM1_EN + time_id, 
  data = g
)

g_grouped <- dtplyr::lazy_dt(g) %>%  
  filter(ADM0_EN != "Laos") %>% 
  mutate(
    is_ban_over = ifelse(ADM1_EN %in% northern_regions_en & days_from_ban_end >= 0, 1, 0),
    time_id_grouped = round(time_id / 3,0),
    is_ban_early = ifelse(is_ban == 1 | (days_from_ban_start > -7 & days_from_ban_end < 0), 1, 0)
  ) %>% 
  dplyr::group_by(time_id_grouped, ADM1_EN, is_ban) %>% 
  dplyr::summarise(
    #is_ban = round(is_ban, 0),
    n_fires_modis = sum(n_fires_modis)
    # avg_temperature = mean(avg_temperature),
    # Total_pop = mean(Total_pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    fires_binary = ifelse(n_fires_modis > 0, 1, 0),
    fires_log = log(n_fires_modis)
  ) %>% 
  as.data.frame() 

library(DIDmultiplegtDYN)

mod_dCDH24 = did_multiplegt_dyn(
  g_grouped, 
  outcome = 'n_fires_modis', 
  group = 'ADM1_EN', 
  time = 'time_id_grouped', 
  # controls = c(
  #   #"Total_pop",
  #   "avg_temperature", "avg_surface_temp",
  #   "avg_precipitation", "avg_u_wind", "avg_v_wind"
  #   ),
  treatment = 'is_ban', # original regression params
  effects   = 30,                  # no. of post-treatment periods
  placebo   = 10,                  # no. of pre-treatment periods
  #cluster   = 'ADM1_EN',           # variable to cluster SEs on
  normalized = TRUE # that are equal to a weighted average of the effects of the current treatment and of its ℓ − 1 first lags on the outcom
  #only_never_switchers = TRUE
)

g_for_plot <- g %>% 
  mutate(
    year_group = ifelse(year > 2016, "post", "pre"),
    type_group = ifelse(type == "Treated", "Treated", "NT")
  ) %>% 
  #filter(days_from_ban_start > -30, days_from_ban_start < 90) %>% 
  #filter(time_id > 182) %>% 
  filter(type != "Laos") %>% 
  #filter(!str_starts(ADM1_EN, "Yangon")) %>% 
  mutate(
    time_week = round(time_id / 182,0),
    days_from_ban_start_group = round(days_from_ban_start / 5, 0),
    binary_fires = ifelse(n_fires_modis>0, 1, 0)
  ) %>%
  group_by(time_week, type) %>% 
  dplyr::summarise(
    mean_fires = mean(n_fires_modis),
    fire_proportion = mean(binary_fires)
  ) %>% 
  ungroup()

ggplot(g_for_plot, aes(x = time_week, 
                       y = mean_fires, color = type, 
                       #linetype = year_group
                       )) +
  geom_line() +
  #geom_smooth(se = FALSE, method = "loess", span = 0.005) +
  labs(
    title = "Number of Fires Over Time by ADM2_EN",
    x = "Time",
    y = "Mean Fire in ADM1_EN"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


