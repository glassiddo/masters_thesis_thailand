# Load necessary libraries
library(dplyr)
library(lubridate)

# Assume 'grid_mixed' is your data frame (data.table works too)
# Ensure date columns are correctly typed
grid_mixed <- grid_mixed %>%
  mutate(
    date = as.Date(date),
    Ban_start = as.Date(Ban_start),
    Ban_end = as.Date(Ban_end)
  )

# --- Step 1: Calculate mean ban dates per year (from non-NA values) ---
mean_ban_dates_by_year <- grid_mixed %>%
  filter(!is.na(Ban_start) & !is.na(Ban_end)) %>%
  group_by(year) %>%
  summarise(
    mean_Ban_start = as_date(floor(mean(as.numeric(Ban_start), na.rm = TRUE))),
    mean_Ban_end = as_date(floor(mean(as.numeric(Ban_end), na.rm = TRUE))),
    .groups = 'drop'
  ) %>%
  filter(!is.na(mean_Ban_start) & !is.na(mean_Ban_end)) # Only keep valid yearly means

new_years <- 2012:2014
# Display the calculated mean dates per year (optional)
made_up_years <- tibble::tibble(
  year = new_years,
  mean_Ban_start = as.Date(paste0(new_years, "-02-04")),  # Using 2015's day-month (02-04)
  mean_Ban_end = as.Date(paste0(new_years, "-04-07"))     # Using 2015's day-month (04-07)
)

# mean_start_doy <- mean(as.integer(format(mean_ban_dates_by_year$mean_start_date, "%j")))
# mean_end_doy <- mean(as.integer(format(mean_ban_dates_by_year$mean_end_date, "%j")))
# 
# new_rows_mean <- tibble::tibble(
#   year = 2012:2014,
#   mean_start_date = as.Date(mean_start_doy - 1, origin = paste0(2012:2014, "-01-01")),
#   mean_end_date = as.Date(mean_end_doy - 1, origin = paste0(2012:2014, "-01-01"))
# )

# Combine the data
extended_ban_dates <- rbind(made_up_years, mean_ban_dates_by_year) %>%
  arrange(year)

# --- Step 3: Join the yearly mean dates back to the main data ---
grid_mixed_updated <- grid_mixed %>%
  # Left join with extended_ban_dates_renamed based on year
  left_join(extended_ban_dates, by = "year")

update_date <- function(x, y) {
  indices <- is.na(x)
  x[indices] <- y[indices]
  return(x)
}

# Apply the function to preserve date class
grid_mixed_fixed <- grid_mixed_updated %>%
  mutate(
    Ban_start = update_date(Ban_start, mean_Ban_start),
    Ban_end = update_date(Ban_end, mean_Ban_end)
  ) %>%
  # Drop the temporary columns
  select(-mean_Ban_start, -mean_Ban_end) %>% 
  mutate(
    days_from_ban_start = as.integer(as.Date(date) - as.Date(Ban_start)),
    days_from_ban_end = as.integer(as.Date(date) - as.Date(Ban_end)),
    is_ban = ifelse(!is.na(Ban_start) & !is.na(Ban_end) & 
                      date >= Ban_start & date <= Ban_end, 1, 0),
    is_ban_over = ifelse(!is.na(Ban_end) & date > Ban_end, 1, 0)
  )


