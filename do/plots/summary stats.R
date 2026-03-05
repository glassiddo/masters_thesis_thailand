rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table,
  tidyverse, stargazer, ggplot2, purrr, readxl, plotrix, classInt,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  fixest, ggfixest, # sun abraham even study
  stargazer,
  kableExtra,
  pensynth, Synth, ggpubr
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data/")

grid <- fread("grid_1606.csv")

climate_data <- fread("2. intermediate/adm1_climatic_date.csv") %>%
  filter(year < 2013) %>%
  rename(adm1_id = admin_id)

grid <- grid %>% 
  left_join(climate_data, by = c("adm1_id", "month", "year"))

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai",
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# Summary statistics by ADM0_EN (ignoring date)

adm0_summary <- grid %>%
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN),
    avg_precipitation = avg_precipitation * 1000,
    avg_temperature = avg_temperature - 273.15
  ) %>%
  group_by(ADM0_EN, ADM1_EN, year, adm1_area) %>%
  summarise(
    total_fires = sum(total_fires, na.rm = TRUE),
    day_fires = sum(day_fires, na.rm = TRUE),
    night_fires = sum(night_fires, na.rm = TRUE),
    crop50_fires = sum(crop50_fires, na.rm = TRUE),
    forest50_fires = sum(forest50_fires, na.rm = TRUE),
    sum_precipitation = sum(avg_precipitation, na.rm = TRUE),
    avg_temperature = mean(avg_temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_fires = total_fires / adm1_area,
    day_fires = day_fires / adm1_area,
    night_fires = night_fires / adm1_area,
    crop50_fires = crop50_fires / adm1_area,
    forest50_fires = forest50_fires / adm1_area
  ) %>%
  group_by(ADM0_EN, ADM1_EN) %>%
  summarise(
    region_mean_total_fires = mean(total_fires, na.rm = TRUE),
    region_mean_day_fires = mean(day_fires, na.rm = TRUE),
    region_mean_night_fires = mean(night_fires, na.rm = TRUE),
    region_mean_crop50_fires = mean(crop50_fires, na.rm = TRUE),
    region_mean_forest50_fires = mean(forest50_fires, na.rm = TRUE),
    region_mean_precip = mean(sum_precipitation, na.rm = TRUE),
    region_mean_temp = mean(avg_temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(ADM0_EN) %>%
  summarise(
    mean_total_fires = mean(region_mean_total_fires),
    sd_total_fires = sd(region_mean_total_fires),
    mean_day_fires = mean(region_mean_day_fires),
    sd_day_fires = sd(region_mean_day_fires),
    mean_night_fires = mean(region_mean_night_fires),
    sd_night_fires = sd(region_mean_night_fires),
    mean_crop50_fires = mean(region_mean_crop50_fires),
    sd_crop50_fires = sd(region_mean_crop50_fires),
    mean_forest50_fires = mean(region_mean_forest50_fires),
    sd_forest50_fires = sd(region_mean_forest50_fires),
    mean_precipitation = mean(region_mean_precip, na.rm = TRUE),
    sd_precipitation = sd(region_mean_precip, na.rm = TRUE),
    mean_temperature = mean(region_mean_temp, na.rm = TRUE),
    sd_temperature = sd(region_mean_temp, na.rm = TRUE),
    n_regions = n(),
    .groups = "drop"
  )  


adm0_summary_feb_apr <- grid %>%
  #filter(month %in% c(2,3,4)) %>%  # Filter for February-April
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN),
    avg_precipitation = avg_precipitation * 1000,
    avg_temperature = avg_temperature - 273.15
  ) %>%
  group_by(ADM0_EN, ADM1_EN, year, month, adm1_area) %>%
  summarise(
    total_fires = sum(total_fires, na.rm = TRUE),
    day_fires = sum(day_fires, na.rm = TRUE),
    night_fires = sum(night_fires, na.rm = TRUE),
    crop50_fires = sum(crop50_fires, na.rm = TRUE),
    forest50_fires = sum(forest50_fires, na.rm = TRUE),
    monthly_precipitation = sum(avg_precipitation, na.rm = TRUE),
    avg_temperature = mean(avg_temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ADM0_EN, ADM1_EN, year, adm1_area) %>%
  summarise(
    total_fires = mean(total_fires, na.rm = TRUE),
    day_fires = mean(day_fires, na.rm = TRUE),
    night_fires = mean(night_fires, na.rm = TRUE),
    crop50_fires = mean(crop50_fires, na.rm = TRUE),
    forest50_fires = mean(forest50_fires, na.rm = TRUE),
    sum_precipitation = mean(monthly_precipitation, na.rm = TRUE),  # Changed to mean
    avg_temperature = mean(avg_temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_fires = total_fires / adm1_area,
    day_fires = day_fires / adm1_area,
    night_fires = night_fires / adm1_area,
    crop50_fires = crop50_fires / adm1_area,
    forest50_fires = forest50_fires / adm1_area
  ) %>%
  group_by(ADM0_EN, ADM1_EN) %>%
  summarise(
    region_mean_total_fires = mean(total_fires, na.rm = TRUE),
    region_mean_day_fires = mean(day_fires, na.rm = TRUE),
    region_mean_night_fires = mean(night_fires, na.rm = TRUE),
    region_mean_crop50_fires = mean(crop50_fires, na.rm = TRUE),
    region_mean_forest50_fires = mean(forest50_fires, na.rm = TRUE),
    region_mean_precip = mean(sum_precipitation, na.rm = TRUE),
    region_mean_temp = mean(avg_temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(ADM0_EN) %>%
  summarise(
    dry_mean_total_fires = mean(region_mean_total_fires),
    dry_sd_total_fires = sd(region_mean_total_fires),
    dry_mean_precipitation = mean(region_mean_precip, na.rm = TRUE),
    dry_sd_precipitation = sd(region_mean_precip, na.rm = TRUE),
    dry_mean_temperature = mean(region_mean_temp, na.rm = TRUE),
    dry_sd_temperature = sd(region_mean_temp, na.rm = TRUE),
    .groups = "drop"
  )


# adm0_summary_time <- grid %>% 
#   mutate(
#     period = case_when(
#       year < 2013 ~ "pre",
#       year < 2017 ~ "weak",
#       TRUE ~ "strict"
#     ),
#     ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN)
#   ) %>%
#   group_by(ADM0_EN, period) %>% 
#   summarise(
#     # Fire variables
#     mean_total_fires = mean(total_fires, na.rm = TRUE),
#     sd_total_fires = sd(total_fires, na.rm = TRUE),
#     mean_day_fires = mean(day_fires, na.rm = TRUE),
#     mean_night_fires = mean(night_fires, na.rm = TRUE),
#     mean_aqua_fires = mean(aqua_fires, na.rm = TRUE),
#     mean_terra_fires = mean(terra_fires, na.rm = TRUE),
#   )

unit_vars <- grid %>% 
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN)
  ) %>% 
  select(
    adm1_id, ADM0_EN, adm1_area, crop_share, tree_share, urban_share, avg_elevation
  ) %>% 
  distinct() %>% 
  group_by(ADM0_EN) %>% 
  summarise(
    across(
      c(adm1_area, crop_share, tree_share, urban_share, avg_elevation),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))
    ),
    .groups = "drop"
  )

full_summary <- adm0_summary %>% left_join(unit_vars, by = "ADM0_EN")


#### plot -----

summary_units <- grid %>% 
  mutate(total_fires = total_fires / adm1_area) %>% 
  group_by(ADM1_EN, year) %>% 
  summarise(
    sum_y_fires = sum(total_fires)
  ) %>% 
  ungroup() %>% 
  group_by(ADM1_EN) %>% 
  summarise(
    mean_y_fires = mean(sum_y_fires)
  ) %>% 
  ungroup() 

all_units <- read_sf("2. intermediate/units/adm1_units_detailed.shp") %>% 
  left_join(summary_units, by = "ADM1_EN") %>% 
  rmapshaper::ms_simplify(keep = 0.01)

all_units_countries <- all_units %>% 
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN),
  ) %>% 
  st_make_valid() %>% 
  group_by(ADM0_EN) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_make_valid() %>% 
  mutate(border_type = "National Border")

northern_border <- all_units %>% 
  filter(ADM1_EN %in% northern_regions_en) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_make_valid()

ggplot() + 
  geom_sf(data = all_units, aes(fill = mean_y_fires)) + 
  geom_sf(data = all_units_countries, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = northern_border, fill = NA, color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(
    option = "F",
    direction = -1,
    name = "Average number of fires (MODIS) per year, normalised by each unit's size",
    limits = c(0, 30),
    breaks = seq(0, 30, length.out = 3), # Explicit breaks
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),  # Increased width
      ticks = TRUE,
      label = TRUE,
      frame.linewidth = 0.5,
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )



#### generic map ----
all_units <- all_units %>%
  mutate(region_group = case_when(
    ADM0_EN == "Laos" ~ "Laos",
    ADM0_EN == "Myanmar" ~ "Myanmar",
    ADM1_EN %in% northern_regions_en ~ "Northern Thailand",
    ADM0_EN == "Thailand" ~ "Rest of Thailand",
    TRUE ~ "Other"
  ))

# Create the map
ggplot(data = all_units) +
  geom_sf(aes(fill = region_group), color = "white", size = 0.5) +
  scale_fill_manual(values = c("Laos" = "#9e9ac8", "Myanmar" = "#fdae61", 
                               "Northern Thailand" = "#2ca25f", "Rest of Thailand" = "#a1d99b")) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    plot.title = element_blank()
  )

# Filter for only the northern provinces
northern_provinces <- read_sf("2. intermediate/units/adm1_units_detailed.shp") %>% 
  left_join(summary_units, by = "ADM1_EN") %>% 
  filter(ADM1_EN %in% northern_regions_en)

my_palette <- c(
  "#4477aa", # blue
  "#ee6677", # red
  "#228833", # green
  "#ccbb44", # yellow
  "#66ccee", # cyan
  "#aa3377", # purple
  "#bbbbbb", # gray
  "#ee8866", # orange
  "#aaaaaa"  # light gray
)
ggplot(data = northern_provinces) +
  geom_sf(aes(fill = ADM1_EN), color = "white", size = 0.5) +
  scale_fill_manual(values = my_palette) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.title = element_blank()
  )
