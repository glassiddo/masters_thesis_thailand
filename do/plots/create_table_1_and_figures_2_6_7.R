source("do/setup.R")

grid <- readRDS(here(out.dir, "grid.rds"))

climate_data <- readRDS(here(build.dir, "climate", "adm1_climatic_date.rds")) |> 
  filter(year < 2013) |>
  rename(adm1_id = admin_id)

grid <- grid |> 
  left_join(climate_data, join_by(adm1_id, month, year))

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai",
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# Summary statistics by ADM0_EN (ignoring date)

adm0_summary <- grid |>
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN),
    avg_precipitation = avg_precipitation * 1000,
    avg_temperature = avg_temperature - 273.15
  ) |>
  summarise(
    total_fires_modis = sum(total_fires_modis, na.rm = TRUE),
    day_fires_modis = sum(day_fires_modis, na.rm = TRUE),
    night_fires_modis = sum(night_fires_modis, na.rm = TRUE),
    crop50_fires_modis = sum(crop50_fires_modis, na.rm = TRUE),
    forest50_fires_modis = sum(forest50_fires_modis, na.rm = TRUE),
    sum_precipitation = sum(avg_precipitation, na.rm = TRUE),
    avg_temperature = mean(avg_temperature, na.rm = TRUE),
    .by = c(ADM0_EN, ADM1_EN, year, adm1_area)
  ) |>
  mutate(
    total_fires_modis = total_fires_modis / adm1_area,
    day_fires_modis = day_fires_modis / adm1_area,
    night_fires_modis = night_fires_modis / adm1_area,
    crop50_fires_modis = crop50_fires_modis / adm1_area,
    forest50_fires_modis = forest50_fires_modis / adm1_area
  ) |>
  summarise(
    region_mean_total_fires_modis = mean(total_fires_modis, na.rm = TRUE),
    region_mean_day_fires_modis = mean(day_fires_modis, na.rm = TRUE),
    region_mean_night_fires_modis = mean(night_fires_modis, na.rm = TRUE),
    region_mean_crop50_fires_modis = mean(crop50_fires_modis, na.rm = TRUE),
    region_mean_forest50_fires_modis = mean(forest50_fires_modis, na.rm = TRUE),
    region_mean_precip = mean(sum_precipitation, na.rm = TRUE),
    region_mean_temp = mean(avg_temperature, na.rm = TRUE),
    .by = c(ADM0_EN, ADM1_EN)
  ) |> 
  summarise(
    mean_total_fires_modis = mean(region_mean_total_fires_modis),
    sd_total_fires_modis = sd(region_mean_total_fires_modis),
    mean_day_fires_modis = mean(region_mean_day_fires_modis),
    sd_day_fires_modis = sd(region_mean_day_fires_modis),
    mean_night_fires_modis = mean(region_mean_night_fires_modis),
    sd_night_fires_modis = sd(region_mean_night_fires_modis),
    mean_crop50_fires_modis = mean(region_mean_crop50_fires_modis),
    sd_crop50_fires_modis = sd(region_mean_crop50_fires_modis),
    mean_forest50_fires_modis = mean(region_mean_forest50_fires_modis),
    sd_forest50_fires_modis = sd(region_mean_forest50_fires_modis),
    mean_precipitation = mean(region_mean_precip, na.rm = TRUE),
    sd_precipitation = sd(region_mean_precip, na.rm = TRUE),
    mean_temperature = mean(region_mean_temp, na.rm = TRUE),
    sd_temperature = sd(region_mean_temp, na.rm = TRUE),
    n_regions = n(),
    .by = ADM0_EN
  )  

unit_vars <- grid |> 
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN)
  ) |> 
  select(
    adm1_id, ADM0_EN, adm1_area, crop_share, tree_share, urban_share, avg_elevation
  ) |> 
  distinct() |> 
  summarise(
    across(
      c(adm1_area, crop_share, tree_share, urban_share, avg_elevation),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))
    ),
    .by = ADM0_EN
  )

full_summary <- adm0_summary |> 
  left_join(unit_vars, join_by(ADM0_EN))


#### plot -----

summary_units <- grid |> 
  mutate(total_fires_modis = total_fires_modis / adm1_area) |> 
  summarise(
    sum_y_fires_modis = sum(total_fires_modis),
    .by = c(ADM1_EN, year)
  ) |> 
  summarise(
    mean_y_fires_modis = mean(sum_y_fires_modis),
    .by = ADM1_EN
  ) 

all_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  left_join(summary_units, by = "ADM1_EN") |> 
  rmapshaper::ms_simplify(keep = 0.01)

all_units_countries <- all_units |> 
  mutate(
    ADM0_EN = ifelse(ADM1_EN %in% northern_regions_en, "North Thai", ADM0_EN),
  ) |> 
  st_make_valid() |> 
  summarise(
    geometry = st_union(geometry),
    .by = ADM0_EN
    ) |> 
  st_make_valid() |> 
  mutate(border_type = "National Border")

northern_border <- all_units |> 
  filter(ADM1_EN %in% northern_regions_en) |> 
  st_union() |> 
  st_as_sf() |> 
  st_make_valid()

ggplot() + 
  geom_sf(data = all_units, aes(fill = mean_y_fires_modis)) + 
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
all_units <- all_units |>
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
northern_provinces <- all_units |> 
  filter(region_group == "Northern Thailand")

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
