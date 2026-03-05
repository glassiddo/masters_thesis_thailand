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

wsh <- grid_mixed %>% 
  mutate(month = month(date)) %>% 
  dplyr::group_by(ADM1_EN, month, year) %>% 
  #filter(ADM1_EN %in% sum_regions$ADM1_EN) %>% 
  #filter(year > 2016) %>% 
  summarise(
    n_fires_modis = sum(n_fires_modis)
    #n_fires_predicted = sum(predicted_fires, na.rm = T),
    # Ban_start = mean(Ban_start),
    # days_from_ban_start = mean(days_from_ban_start),
    # days_from_ban_end = mean(days_from_ban_end),
    # is_ban = mean(is_ban),
    # is_ban_over = mean(is_ban_over)
  )  %>% 
  ungroup()

rm(grid, sum_regions)
gc()

# Calculate the average ban start date for each northern region from their post-2014 data
# But we just want the month and day (not the year)
northern_avg_ban_dates <- grid_mixed %>%
  filter(ADM1_EN %in% northern_regions_en, 
         year > 2014,
         !is.na(Ban_start)) %>%
  group_by(ADM1_EN) %>%
  # Extract the month and day pattern from the actual ban dates
  summarise(
    avg_month = median(month(Ban_start), na.rm = TRUE),
    avg_day = median(day(Ban_start), na.rm = TRUE)
  ) %>%
  # Create a function to reconstruct the date for any year
  mutate(
    # This creates a function for each province that can generate a date for any year
    date_generator = purrr::map2(avg_month, avg_day, ~function(yr) {
      month_val <- floor(.x)
      day_val <- floor(.y)
      # Handle edge cases like February 29
      if(month_val == 2 && day_val > 28) day_val <- 28
      as.Date(paste0(yr, "-", month_val, "-", day_val))
    })
  )

generate_northern_ban_dates <- function(df, northern_avg_dates) {
  df %>%
    rowwise() %>%
    mutate(
      northern_ban_date = if(ADM1_EN %in% northern_regions_en) {
        # Find this province in our northern_avg_ban_dates
        province_info <- northern_avg_dates %>% filter(ADM1_EN == .data$ADM1_EN)
        if(nrow(province_info) > 0) {
          date_fn <- province_info$date_generator[[1]]
          date_fn(year)
        } else {
          NA_Date_
        }
      } else {
        NA_Date_
      }
    ) %>%
    ungroup()
}

h <- generate_northern_ban_dates(wsh, northern_avg_ban_dates)
wsh <- h %>% 
  mutate(
  Ban_start = ifelse(is.na(Ban_start) & year < 2015 & ADM1_EN %in% northern_regions_en, 
                     northern_ban_date, Ban_start)
  ) %>% 
  select(-northern_ban_date)
  
avg_ban_dates <- wsh %>%
  filter(!is.na(Ban_start)) %>%
  group_by(year) %>%
  summarise(avg_ban_date = as.Date(mean(as.numeric(Ban_start), na.rm = TRUE), 
                                   origin = "1970-01-01"))

# Find never treated provinces
never_treated_provinces <- wsh %>%
  group_by(ADM1_EN) %>%
  summarise(ever_treated = any(is_ban == 1, na.rm = TRUE)) %>%
  filter(!ever_treated) %>%
  pull(ADM1_EN)

# Apply the function to generate ban dates for never treated
wsh <- wsh %>%
  left_join(avg_ban_dates, by = "year") %>%
  # Apply our function to generate northern ban dates
  generate_northern_ban_dates(northern_avg_ban_dates) %>%
  mutate(
    date = as.Date(date),
    is_never_treated = ADM1_EN %in% never_treated_provinces,
    # Assign ban dates with the new logic
    ban_date = case_when(
      # Northern regions in pre-2015 years get their synthetic ban date
      ADM1_EN %in% northern_regions_en & year < 2015 ~ northern_ban_date,
      
      # Northern regions in 2015+ years use their actual ban dates when available
      ADM1_EN %in% northern_regions_en & year >= 2015 & !is.na(Ban_start) ~ as.Date(Ban_start),
      
      # Never treated provinces use the yearly average 
      is_never_treated ~ as.Date(avg_ban_date),
      
      # Actually treated provinces use their real ban dates
      is_ban == 1 ~ as.Date(Ban_start),
      
      # Default case
      TRUE ~ NA_Date_
    )
  ) %>%
  group_by(ADM1_EN, year) %>%
  fill(ban_date, .direction = "downup") %>%
  ungroup() %>%
  mutate(event_time = as.integer(date - ban_date)) 
  
wsh <- wsh %>% 
  mutate(
    is_treated = ifelse(ADM1_EN %in% northern_regions_en & year > 2016, 1, 0)
  ) %>% 
  filter(event_time > -15, event_time < 20)

model <- feols(
  n_fires_modis ~ i(event_time,is_treated, ref = "-10") | ADM1_EN + year,
  data = h
)

### biased! as the post 5 days include the anticipatory effect of -5
iplot(model, main = "Effect of Fire Ban Over Time", xlab = "Time relative to ban", ylab = "Change in fires")

# Create the annual aggregation for total fires
h_annual <- grid_ %>% 
  mutate(
    is_treated = ifelse(ADM1_EN %in% northern_regions_en & year > 2016, 1, 0)
  ) %>% 
  group_by(year, ADM1_EN, is_treated) %>% 
  summarise(n_total = sum(n_fires_modis), .groups = "drop")

# Run the model with total annual fires
feols(
  n_total ~ is_treated | year + ADM1_EN,
  data = h_annual
)

grid_mixed_fixed1 <- grid_mixed_fixed %>%
  group_by(ADM1_EN) %>%
  mutate(fires_std = scale(n_fires_modis))  # z-score normalization per region

##############
bin_start <- -15
bin_end <- 15
bin_space <- 3
breaks_seq <- seq(bin_start, bin_end, by = bin_space)
labels_seq <- paste(head(breaks_seq, -1), "to", tail(breaks_seq, -1) - 1)

h_bin_data <- g %>%
  mutate(
    days_bin = cut(days_from_ban_start, 
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
      #year + 
      ADM2_EN,
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

# Order and format for easy reading
results_df <- results_df[, c("bin", "estimate", "std.error", "statistic", "p.value")]
results_df$stars <- symnum(results_df$p.value, corr = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", ""))
print(results_df, n = 100)

model <- feols(
  n_fires_modis ~  #i(days_from_ban_start, ref = -10)  # Event study (relative to -10 days)
    i(days_from_ban_end, is_treated, ref = -10)  # Interaction for DiD.
  + Total_pop + avg_temperature + avg_surface_temp + avg_precipitation + avg_u_wind + avg_v_wind
  | ADM1_EN + year,                 # Province and year fixed effects
  data = h_bin_data,
  cluster = ~ADM1_EN               # Cluster robust SEs
)
iplot(model, main = "Flexible Bin-Based DiD", xlab = "Days Relative to Ban", ylab = "Effect on Fires")
