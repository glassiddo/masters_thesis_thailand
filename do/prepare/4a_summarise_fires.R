source("do/setup.R")

read_fire_attrs <- function(gpkg_path, cols) {
  layer <- st_layers(gpkg_path)$name[[1]]
  query <- paste0(
    "SELECT ",
    paste(sprintf("\"%s\"", cols), collapse = ", "),
    " FROM \"",
    layer,
    "\""
  )
  st_read(gpkg_path, query = query, quiet = TRUE) |>
    as_tibble() |>
    select(any_of(cols)) |>
    mutate(date = as.Date(date))
}

count_true <- function(x) sum(x, na.rm = TRUE)

metrics_daynight <- list(
  day_fires = rlang::expr(count_true(DAYNIGHT == "D")),
  night_fires = rlang::expr(count_true(DAYNIGHT == "N"))
)

metrics_satellite <- list(
  terra_fires = rlang::expr(count_true(SATELLITE == "Terra")),
  aqua_fires = rlang::expr(count_true(SATELLITE == "Aqua"))
)

metrics_satellite_daynight <- list(
  terra_night_fires = rlang::expr(count_true(SATELLITE == "Terra" & DAYNIGHT == "N")),
  terra_day_fires = rlang::expr(count_true(SATELLITE == "Terra" & DAYNIGHT == "D")),
  aqua_night_fires = rlang::expr(count_true(SATELLITE == "Aqua" & DAYNIGHT == "N")),
  aqua_day_fires = rlang::expr(count_true(SATELLITE == "Aqua" & DAYNIGHT == "D"))
)

metrics_confidence <- list(
  high_confidence_fires50 = rlang::expr(count_true(CONFIDENCE > 50)),
  high_confidence_fires75 = rlang::expr(count_true(CONFIDENCE > 75))
)

metrics_frp <- list(
  q1_frp_fires = rlang::expr(count_true(FRP < 20.8)),
  q2_frp_fires = rlang::expr(count_true(FRP > 20.8 & FRP > 11.3))
)

metrics_road <- list(
  q1_roads_prov = rlang::expr(sum(road_q_province == 1, na.rm = TRUE)),
  q2_roads_prov = rlang::expr(sum(road_q_province == 2, na.rm = TRUE)),
  q1_roads_country = rlang::expr(sum(road_q_country == 1, na.rm = TRUE)),
  q2_roads_country = rlang::expr(sum(road_q_country == 2, na.rm = TRUE))
)

metrics_urban <- list(
  q1_urban_prov = rlang::expr(sum(urban_q_province == 1, na.rm = TRUE)),
  q2_urban_prov = rlang::expr(sum(urban_q_province == 2, na.rm = TRUE)),
  q1_urban_country = rlang::expr(sum(urban_q_country == 1, na.rm = TRUE)),
  q2_urban_country = rlang::expr(sum(urban_q_country == 2, na.rm = TRUE))
)

metrics_crop_prod <- list(
  q1_crop_prod = rlang::expr(sum(crop_production_q == 1, na.rm = TRUE)),
  q2_crop_prod = rlang::expr(sum(crop_production_q == 2, na.rm = TRUE)),
  q3_crop_prod = rlang::expr(sum(crop_production_q == 3, na.rm = TRUE)),
  q4_crop_prod = rlang::expr(sum(crop_production_q == 4, na.rm = TRUE))
)

metrics_dom_crop <- list(
  rice_dom = rlang::expr(sum(max_crop == "rice", na.rm = TRUE)),
  maize_dom = rlang::expr(sum(max_crop == "maiz", na.rm = TRUE)),
  cass_dom = rlang::expr(sum(max_crop == "cass", na.rm = TRUE))
)

metrics_landuse <- list(
  crop50_fires = rlang::expr(sum(crop50 == 1, na.rm = TRUE)),
  forest50_fires = rlang::expr(sum(forest50 == 1, na.rm = TRUE))
)

shared_metrics <- c(
  metrics_confidence,
  metrics_frp,
  metrics_road,
  metrics_urban,
  metrics_crop_prod,
  metrics_dom_crop
)

fire_cols <- c(
  "fire_id",
  "adm0_id",
  "adm1_id",
  "date",
  "DAYNIGHT",
  "SATELLITE",
  "CONFIDENCE",
  "FRP"
)

#### distances ----
quantiles_n <- 2 # theoretically could choose to actually use quantiles, rather than top/bottom 50%

add_distance_quantiles <- function(distances, fires, quantiles_n) {
  distances |>
    left_join(
      fires |> select(adm0_id, adm1_id, fire_id),
      join_by(fire_id)
    ) |>
    # compute quantiles within adm1 and adm0
    mutate(
      road_q_province = ntile(distance_to_road, quantiles_n),
      urban_q_province = ntile(distance_to_urban, quantiles_n),
      .by = c(adm0_id, adm1_id)
    ) |>
    mutate(
      road_q_country = ntile(distance_to_road, quantiles_n),
      urban_q_country = ntile(distance_to_urban, quantiles_n),
      .by = adm0_id
    ) |>
    select(-adm0_id, -adm1_id)
}

modis_fires <- read_fire_attrs(
  here(build.dir, "fires", "modis_with_region.gpkg"),
  fire_cols
)

modis_distances <- readRDS(here(build.dir, "fires", "modis_with_distances.rds")) |>
  add_distance_quantiles(modis_fires, quantiles_n)

#### crops and land use ----

modis_crops <- readRDS(here(build.dir, "fires", "modis_with_crops.rds"))
modis_landuse <- readRDS(here(build.dir, "fires", "modis_with_landuse.rds"))

##### summarise to days -----

#### full summary level 1-----

modis_summary <- list(modis_fires, modis_distances, modis_crops, modis_landuse) |>
  reduce(left_join, join_by(fire_id)) |> 
  summarise(
    total_fires = n(),
    # by day/night
    !!!metrics_daynight,
    # by satellite
    !!!metrics_satellite,
    # by satellite and day/night
    !!!metrics_satellite_daynight,
    # only high confidence fires
    !!!metrics_confidence,
    # by frp
    !!!metrics_frp,
    # by road quantiles
    !!!metrics_road,
    # by urban quantiles
    !!!metrics_urban,
    # by crop production
    !!!metrics_crop_prod,
    # by dominant crop
    !!!metrics_dom_crop,
    # # # by land use
    !!!metrics_landuse,
    .by = c(adm1_id, date)
  )

rm(modis_distances, modis_crops, modis_landuse)

#### without duplicates ----

modis_no_duplicate <- readRDS(here(build.dir, "fires", "modis_no_duplicate.rds")) |>
  mutate(date = as.Date(date)) |>
  rename(no_duplicates = modis_no_duplicates)

modis_no_repeated <- readRDS(here(build.dir, "fires", "modis_no_repeated.rds")) |>
  mutate(date = as.Date(date)) |>
  rename(no_repeated = modis_no_repeated)

modis_summary <- modis_summary |>
  left_join(modis_no_duplicate, join_by(adm1_id, date)) |>
  left_join(modis_no_repeated, join_by(adm1_id, date)) |>
  rename_with(~paste0(.x, "_modis"), -c(adm1_id, date))

#### write modis summarised ----

saveRDS(modis_summary, here(build.dir, "fires", "modis_summarised.rds"))

rm(modis_summary, modis_fires, modis_no_duplicate, modis_no_repeated)

#### viirs -----

viirs_fires <- read_fire_attrs(
  here(build.dir, "fires", "viirs_with_region.gpkg"),
  fire_cols
)

viirs_distances <- readRDS(here(build.dir, "fires", "viirs_with_distances.rds")) |>
  add_distance_quantiles(viirs_fires, quantiles_n)

viirs_crops <- readRDS(here(build.dir, "fires", "viirs_with_crops.rds"))

viirs_summary <- list(viirs_fires, viirs_distances, viirs_crops) |>
  reduce(left_join, join_by(fire_id)) |> 
  summarise(
    total_fires = n(),
    # by day/night
    !!!metrics_daynight,
    !!!shared_metrics,
    .by = c(adm1_id, date)
  )

rm(viirs_distance, viirs_crops)

viirs_no_duplicate <- readRDS(here(build.dir, "fires", "viirs_no_duplicate.rds")) |>
  mutate(date = as.Date(date)) |>
  rename(no_duplicates = viirs_no_duplicates)

viirs_no_repeated <- readRDS(here(build.dir, "fires", "viirs_no_repeated.rds")) |>
  mutate(date = as.Date(date)) |>
  rename(no_repeated = viirs_no_repeated)

viirs_summary <- viirs_summary |>
  left_join(viirs_no_duplicate, join_by(adm1_id, date)) |>
  left_join(viirs_no_repeated, join_by(adm1_id, date)) |>
  rename_with(~paste0(.x, "_viirs"), -c(adm1_id, date))

saveRDS(viirs_summary, here(build.dir, "fires", "viirs_summarised.rds"))

rm(viirs_summary, viirs_fires, viirs_no_duplicate, viirs_no_repeated)
