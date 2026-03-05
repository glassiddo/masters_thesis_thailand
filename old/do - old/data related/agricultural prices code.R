rm(list=ls())
#dev.off()
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, tidyverse, ggplot2, 
  readxl, janitor, lubridate, zoo,
  geodata, spData, sf, terra, maps, sp, raster  # spatial analysis
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data/agricultural prices")

clean_crop_names <- function(crop_names) {
  crop_names %>%
    str_remove_all("\\(.*?\\)") %>%  # Remove anything inside parentheses
    str_remove_all("\\d+%") %>%  # Remove percentages (e.g., "5%")
    str_remove_all("-\\d+%") %>%  # Remove percentage ranges like "14-15%"
    str_remove_all("<.*|>.*") %>%  # Remove inequalities like "<" or ">100 Kg."
    str_trim()  # Trim extra spaces
}

prices <- read_excel("RG_NR_038.xlsx") %>% 
  mutate(
    row_number = row_number()
  ) %>% 
  select(-`Bank of Thailand`) %>% 
  filter(row_number > 4, row_number < 29) %>% # only relevant rows with data
  select(-row_number) %>%
  row_to_names(row_number = 1) %>% 
  rename(Crop = 1) %>% 
  mutate(
    Crop = clean_crop_names(Crop),
    across(-Crop, as.character),  # Ensure all non-Crop columns are character
    across(-Crop, ~ na_if(.x, "-")),  # Replace "-" with NA
    across(-Crop, as.numeric), # Convert values to numeric
    across(-Crop, ~ ifelse(Crop == "Major Rice" & is.na(.x), 
                                  .x[Crop == "Second Rice 14-"], 
                                  .x)), # replace 
    across(-Crop, ~ ifelse(. == 0, NA, .)),
    Crop = ifelse(Crop == "Major Rice", "Rice", Crop)
  ) %>% 
  filter(
    Crop %in% c("Rice", "Maize", "Soybean", "Glutinous", 
                "Sugarcane", "Cassava")
  )

normalized_prices <- prices %>%
  rowwise() %>%
  mutate(across(where(is.numeric), 
                ~ifelse(!is.na(.x) && !is.na(`JAN 2010`) && `JAN 2010` != 0,
                        (.x / `JAN 2010`) * 100, 
                        NA_real_))) %>%
  ungroup()

prices_long <- normalized_prices %>%
  pivot_longer(cols = -Crop, names_to = "Month", values_to = "Price") %>% # Reshape to long format
  mutate(
    First_of_month = parse_date_time(Month, orders = "my"), # Extract date
    Year = year(First_of_month), # Extract year
    Month = month(First_of_month),
    Quarter = paste0("Q", ceiling(Month / 3), " ", Year), # Create quarter labels
    Quarter = as.yearqtr(Quarter, format = "Q%q %Y")
  )
prices_long_q <- prices_long %>% 
  group_by(Crop, Quarter) %>%
  summarize(Price = mean(Price, na.rm = TRUE), .groups = "drop")# Compute quarterly mean
  #pivot_wider(names_from = Quarter, values_from = Quarterly_Price) # Reshape back to wide format if needed
   
ggplot(prices_long_q %>% filter(Crop == "Rice"), aes(x = Quarter, y = Price, color = Crop)) +
  geom_line() +
  geom_point() +
  labs(title = "Price Progression of Crops Over Time",
       x = "Date",
       y = "Price - normalized to 100 in January 2010") +
  theme_minimal()

