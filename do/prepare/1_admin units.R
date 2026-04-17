source("do/setup.R")

adm1 <- ne_states(returnclass = "sf") |>
  filter(admin %in% c("Thailand", "Laos")) |> # these are used just to get provinces
  select(admin, name) |> 
  rename(
    ADM0_EN = admin,
    ADM1_EN = name
  ) |> 
  mutate(
    ADM1_EN = case_when(
      # simplify non english letters or clean names in laos
      ADM1_EN == "Oudômxai" ~ "Oudomxai",
      ADM1_EN == "Phôngsali" ~ "Phongsaly",
      ADM1_EN == "Louangphrabang" ~ "Louangphabang",
      ADM1_EN == "Louang Namtha" ~ "Louangnamtha",
      ADM1_EN == "Xaignabouri" ~ "Xaignabouly",
      TRUE ~ ADM1_EN
    )  
  ) 
  
myanmar <- 
  read_sf(
    here(raw.dir, "admin units", "myanmar", "mmr_polbnda_adm2_250k_mimu_20240215.shp")
    # from https://data.humdata.org/dataset/myanmar-administrative-lines-shapefile
  ) |> 
  st_make_valid() |> 
  # transform adm2_en into adm1_en as otherwise units are 
  # much larger than the ones in thailand and laos
  summarise(geometry = st_union(geometry), .by = ADM2_EN) |> 
  rename(
    ### take one admin level lower for myanmar as equivaent to thailand and laos
    ADM1_EN = ADM2_EN
  ) |> 
  mutate(
    ADM0_EN = "Myanmar"
  )

all_units <- bind_rows(adm1, myanmar) |> 
  mutate(
    adm1_id = row_number(),
    adm1_area = as.numeric(st_area(geometry)/100000000)
  ) |>
  mutate(
    adm0_id = cur_group_id(),
    .by = ADM0_EN
  ) 

write_sf(all_units, here(build.dir, "units", "adm1_units_detailed.shp"), append = F)
