source("code/setup.R")

##### load countries and create function to filter fires and match to admin units ----
myanmar <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) %>% 
  filter(ADM0_EN == "Myanmar") %>%
  select(adm0_id, adm1_id)

thailand <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) %>% 
  filter(ADM0_EN == "Thailand") %>% 
  select(adm0_id, adm1_id)

laos <- read_sf(here(build.dir, "units", "adm1_units_detailed.shp")) %>% 
  filter(ADM0_EN == "Laos") %>% 
  select(adm0_id, adm1_id)


process_fires <- function(data_path, units) {
  read_sf(data_path) %>%
    rename(date = ACQ_DATE) %>% 
    mutate(month = month(date), year = year(date)) %>%
    select(-c("INSTRUMENT", "VERSION")) %>%
    filter(
      TYPE == 0, # only presumed vegatation fires; exclude offshore, volcanos and static sources
      year < 2021,  # only interested in pre 2021 
      CONFIDENCE > 30 # ignore fires that are very uncertain
    ) %>%
    st_join(units, join = st_within) %>% # for each fire, check in which adm2 it is
    filter(!is.na(adm1_id)) # remove fires outside
}

### modis ----

modis_thai <- process_fires(
 here(raw.dir, "fires", "thailand", "modis", "fire_archive_M-C61_620555.shp"), thailand
  )

modis_myan <- process_fires(
  here(raw.dir, "fires", "myanmar", "modis", "fire_archive_M-C61_620556.shp"), myanmar
  )

modis_laos <- process_fires(
  here(raw.dir, "fires", "laos", "modis", "fire_archive_M-C61_627751.shp"), laos
)

modis_fires <- bind_rows(modis_thai, modis_myan, modis_laos) %>%
  mutate(
    fire_id = row_number() # give unique identifier for each fire
  )

st_write(modis_fires, here(out.dir, "modis_with_region_2206.gpkg"), append = FALSE)

# only keep fire id and date for the file uploaded to gee
modis_for_gee <- modis_fires %>% select(fire_id, date)

st_write(modis_for_gee, "fires for gee/modis_with_region_for_gee_1606.shp", append = FALSE)

# test for duplicates
modis_fires %>%
  st_drop_geometry() %>%
  janitor::get_dupes(fire_id)


### viirs -----

viirs_thai <- process_fires(
  here(raw.dir, "fires", "thailand", "viirs", "fire_archive_SV-C2_629651.shp"), thailand
) 

viirs_myan <- process_fires(
  here(raw.dir, "fires", "myanmar", "viirs", "fire_archive_SV-C2_629653.shp"), myanmar
) 

viirs_laos <- process_fires(
  here(raw.dir, "fires", "laos", "viirs", "fire_archive_SV-C2_629652.shp"), laos
) 

viirs_fires <- bind_rows(viirs_thai, viirs_myan, viirs_laos) %>% 
  mutate(
    fire_id = row_number() # give unique identifier for each fire
  ) 

st_write(viirs_fires, here(build.dir, "viirs_with_region_2906.gpkg"), append = FALSE)

