source("do/setup.R")

##### load countries and create function to filter fires and match to admin units ----
all <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) 

admin_units <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) |> 
  select(adm0_id, adm1_id)

process_fires <- function(data_paths, units) {
  data_paths |> 
    map_df(~read_sf(.x)) |>
    rename(date = ACQ_DATE) |> 
    mutate(month = month(date), year = year(date)) |>
    select(-c("INSTRUMENT", "VERSION")) |>
    filter(
      TYPE == 0, # only presumed vegatation fires; exclude offshore, volcanos and static sources
      year < 2021,  # only interested in pre 2021 
      CONFIDENCE > 30 # ignore fires that are very uncertain
    ) |>
    st_join(units, join = st_within) |> # for each fire, check in which adm2 it is
    filter(!is.na(adm1_id)) # remove fires outside
}

### modis ----
# were downloaded as different per-country file from the nasa website
files <- list(
  modis = here(raw.dir, "fires", 
               c("thailand/modis/fire_archive_M-C61_620555.shp",
                 "myanmar/modis/fire_archive_M-C61_620556.shp",
                 "laos/modis/fire_archive_M-C61_627751.shp")),
  
  viirs = here(raw.dir, "fires", 
               c("thailand/viirs/fire_archive_SV-C2_629651.shp",
                 "myanmar/viirs/fire_archive_SV-C2_629653.shp",
                 "laos/viirs/fire_archive_SV-C2_629652.shp"))
)

# process and give unique identifier for each fire
# (must happen after merging otherwise ids will be repeated)
modis_fires <- process_fires(files$modis, admin_units) |> 
  mutate(fire_id = row_number()) 

st_write(modis_fires, here(build.dir, "modis_with_region_2206.gpkg"), delete_dsn = TRUE)

# only keep fire id and date for the file uploaded to gee
modis_for_gee <- modis_fires |> select(fire_id, date)

st_write(modis_for_gee, here(build.dir, "fires for gee", "modis_with_region_for_gee_1606.shp"), append = FALSE)

# test for duplicates
modis_fires |> 
  st_drop_geometry() |> 
  janitor::get_dupes(fire_id)

rm(modis_fires, modis_for_gee)
gc()

viirs_fires <- process_fires(files$viirs, admin_units) |> 
  mutate(fire_id = row_number())

st_write(viirs_fires, here(build.dir, "viirs_with_region_2906.gpkg"), delete_dsn = TRUE)

