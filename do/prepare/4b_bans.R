source("do/setup.R")

# is rather late in the hierarchy because its independent of the fires
# and is very light, unlike the fires
# so better to keep it here for easier changes

bans <- fread(here(raw.dir, "bans", "ban dates v2.csv")) |>
  select(Region, `Ban start`, `Ban end`, `Certainty (quite random)`) |> 
  rename(
    ADM1_EN = Region,
    Ban_start = `Ban start`, 
    Ban_end = `Ban end`,
    VeryLikely = `Certainty (quite random)`
  ) |> 
  mutate(
    Ban_start = as.Date(Ban_start, format = "%Y-%m-%d"),
    Ban_end = as.Date(Ban_end, format = "%Y-%m-%d"),
    year = lubridate::year(Ban_start),
    VeryLikely = case_when(
      VeryLikely == "" ~ 1, 
      VeryLikely == "Low" ~ 0 # only low are marked, turn it into binary
    )
  ) |> 
  filter(!is.na(Ban_start)) |> 
  filter(year < 2021) |> 
  mutate(
    halfmonth_ban_start = (month(Ban_start) - 1) * 2 + if_else(day(Ban_start) <= ceiling(lubridate::days_in_month(Ban_start) / 2), 1, 2),
    halfmonth_ban_end = (month(Ban_end) - 1) * 2 + if_else(day(Ban_end) <= ceiling(lubridate::days_in_month(Ban_end) / 2), 1, 2)
  ) 
  
saveRDS(bans, here(build.dir, "bans_cleaned.rds"))
