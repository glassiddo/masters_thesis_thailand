source("do/setup.R")

# shares of fires in each satellite overpass

modis_fires <- st_read(here(build.dir, "fires", "modis_with_region.gpkg"))

grid <- readRDS(here(out.dir, "grid.rds"))

# define units
all_units <- grid %>%
  pull(adm1_id) %>%
  unique()

treated_units <- grid %>%
  filter(ADM1_EN %in% northern_regions_en) %>%
  pull(adm1_id) %>%
  unique()

control_units <- setdiff(all_units, treated_units) 

rm(grid)

bounds <- modis_fires |> 
  st_drop_geometry() |>
  mutate(
    month = month(date),
    period = case_when(
      year < 2013 ~ "pre",
      year < 2017 ~ "weak",
      year > 2016 ~ "strict",
      TRUE ~ NA
    ),
    region = case_when(
      adm1_id %in% treated_units ~ "North Thailand", 
      adm0_id == 3 ~ "Thailand", 
      adm0_id == 2 ~ "Myanmar", 
      adm0_id == 1 ~ "Laos",
      TRUE ~ NA
    ),
    window = case_when(
      DAYNIGHT == "D" & SATELLITE == "Aqua"  ~ "aqua_day",
      DAYNIGHT == "D" & SATELLITE == "Terra" ~ "terra_day",
      DAYNIGHT == "N" & SATELLITE == "Aqua"  ~ "aqua_night",
      DAYNIGHT == "N" & SATELLITE == "Terra" ~ "terra_night",
      TRUE ~ NA_character_
    )
  ) |> 
  summarise(
    n = n(), 
    .by = c(region, period, window, month)
    ) |> 
  pivot_wider(names_from = window, values_from = n) |>
  filter(region == "North Thailand", period == "pre", month < 6) |>
  select(-period, -region) |>
  janitor::adorn_totals("row") |>
  mutate(month = ifelse(month == 15, "Total", month))
