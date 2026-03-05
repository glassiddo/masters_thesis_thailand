## this code adds 2015 values of total population, male population, female population and 
## number of households for each tambon. there are 3 missing values (potentially fixeable but worth it?)
## data from here https://stat.bora.dopa.go.th/new_stat/webPage/statByYear.php
## theoretically can do values per year but assume variation is very small

rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, readxl,
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest # twfe + sun and abraham event study
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data")

all_units <- read_sf("2. intermediate/units/adm3_units.shp")

### thailand ----
pop <- fread("1. raw/population/pop tambon.csv")
admin_units_detailed_thai <- read_sf(
  "1. raw/admin units/thailand/tha_admbnda_adm3_rtsd_20220121.shp"
  )

duplicate_entry <- admin_units_detailed_thai %>%
  filter(ADM3_EN == "Mae Kha", ADM2_EN == "Fang", ADM1_EN == "Chiang Mai")
unioned_geometry <- st_union(duplicate_entry)

duplicate_entry <- duplicate_entry %>%
  slice(1) %>%  # Keep only one row
  mutate(geometry = unioned_geometry)

admin_units_detailed_thai <- admin_units_detailed_thai %>%
  filter(!(ADM3_EN == "Mae Kha" & ADM2_EN == "Fang" & ADM1_EN == "Chiang Mai"))

admin_units_detailed_thai <- bind_rows(admin_units_detailed_thai, duplicate_entry)

rm(duplicate_entry, unioned_geometry)

## translate the population data variables

colnames_pop <- colnames(pop)
translated_list <- polyglotr::google_translate(colnames_pop, target_lang = "en", source_language = "th")
translated_names <- as.character(translated_list)
colnames(pop) <- translated_names

### clean population data thai ----
pop_cln <- pop %>% 
  mutate(ADM1_TH = sub("^จังหวัด", "", `Province name`)) %>% 
  #filter(ADM1_TH %in% northern_regions) %>% 
  select(-`Registration Office name`, -`Village code`, -`Village name`,
         -`Province name`) %>% 
  rename(SUB_DISTRICT_CODE = `Sub -district code`, PROVINCE_CODE = `Provincial Code`) %>% 
  mutate(
    Year = 2015,  # it appears before as 5812, which refers to Year-Month, 2558 hence 58 is 2015. 12 is december
    ADM3_TH = trimws(sub("^ตำบล", "", `Sub -district name`)), # clean name (remove tambon)
    Total_pop = as.numeric(gsub(",","", `Total population`)), 
    Male_pop = as.numeric(gsub(",","", `Male population`)),
    Female_pop = as.numeric(gsub(",","", `Female population`)),
    HH_num = as.numeric(gsub(",","", `Number of houses (households)`)),
    SUB_DISTRICT_CODE = SUB_DISTRICT_CODE / 100 # remove the last two zeros to match
  ) %>% 
  select(-`Total population`, -`Male population`, -`Female population`, 
         -`Sub -district name`,, -`Number of houses (households)`) %>% 
  filter(ADM3_TH != "-") %>% # empty values / total for all province / districts
  mutate(pop_per_hh = Total_pop / HH_num) %>% 
  filter(pop_per_hh < 10 & pop_per_hh > 0.8) %>% # quite clear measurement errors
  group_by(
    SUB_DISTRICT_CODE, 
    ADM1_TH,
    ADM3_TH
    ) %>% 
  # for some subdistricts, there is more than a single registration office. ill take the means
  summarise(
    Total_pop = mean(Total_pop),
    Male_pop = mean(Male_pop),
    Female_pop = mean(Female_pop),
    HH_num = mean(HH_num)
  ) %>% 
  ungroup()
  
### incorporate administrative unit ----
#### required as there are some combinations of ADM1+ADM3 with different ADM2
#### so this takes data from here https://github.com/Cerberus/Thailand-Address/tree/master
#### and then I match the population data with the subdistrict code with the ADM2 

library(jsonlite)
d_json <- fromJSON("1. raw/admin units/thailand/districts.json", flatten = TRUE)
subd_json <- fromJSON("1. raw/admin units/thailand/subDistricts.json", flatten = TRUE) %>%
  left_join(d_json %>% select(DISTRICT_ID, DISTRICT_CODE, DISTRICT_NAME), by = "DISTRICT_ID") %>% 
  select(DISTRICT_NAME, SUB_DISTRICT_CODE) %>% 
  mutate(SUB_DISTRICT_CODE = as.integer(SUB_DISTRICT_CODE))

pop_district <- pop_cln %>% 
  ungroup() %>% 
  left_join(subd_json, by = "SUB_DISTRICT_CODE") %>% 
  select(-SUB_DISTRICT_CODE) %>% 
  filter(!is.na(DISTRICT_NAME)) %>% 
  rename(ADM2_TH = DISTRICT_NAME)

adm_units_with_pop <- admin_units_detailed_thai %>% 
  mutate(
    adm3_id_pop = row_number()
  ) %>% 
  select(adm3_id_pop, ADM1_TH, ADM2_TH, ADM3_TH, ADM1_EN, ADM2_EN, ADM3_EN) %>% 
  left_join(pop_district %>% 
              select(ADM1_TH, ADM2_TH, ADM3_TH, Total_pop, Male_pop, Female_pop, HH_num),
            by = c("ADM1_TH", "ADM2_TH", "ADM3_TH")
            ) 

ggplot(adm_units_with_pop) +
  geom_sf(aes(fill = Total_pop)) +
  scale_fill_viridis_c(option = "B")

adm_units_with_pop <- adm_units_with_pop %>% 
  st_set_geometry(NULL) %>% 
  select(-ADM1_TH, -ADM2_TH, -ADM3_TH)

adm_thai_pop <- all_units %>% 
  st_drop_geometry() %>% 
  filter(ADM0_EN == "Thailand") 

adm_units_with_pop <- adm_units_with_pop %>% 
  st_drop_geometry() %>% 
  left_join(adm_thai_pop %>% st_drop_geometry(), by = c("ADM1_EN", "ADM2_EN", "ADM3_EN")) %>% 
  ### causes three duplicates but not worth cleaning probably - at least for now...
  filter(!is.na(adm3_id)) %>% 
  select(-ADM0_EN, -ADM1_EN, -ADM2_EN, -ADM3_EN) %>% 
  group_by(
    adm3_id
  ) %>% 
  summarise(
    Total_pop = sum(Total_pop),
    Male_pop = sum(Male_pop),
    Female_pop = sum(Female_pop),
    HH_num = sum(HH_num)
  ) %>% 
  ungroup() 

# some units with NAs...

rm(adm_thai_pop, translated_list, d_json, pop, pop_cln, pop_district, subd_json)

##### myanmar -----
population_myanmar <- fread(
  "1. raw/population/householdpopulationbaseddatasetmimutownshipsabbreviated.csv"
) %>% 
  select(
    name_st, name_dt, name_ts, pop_t, pop_m, pop_f, hh_t
  ) %>% 
  rename(
    ADM1_EN = name_st,
    ADM2_EN = name_dt, 
    ADM3_EN = name_ts, 
    Total_pop = pop_t, 
    Male_pop = pop_m, 
    Female_pop = pop_f, 
    HH_num = hh_t
  ) %>% 
  # now, replace adm1 by adm2; and adm2 by adm3
  select(-ADM1_EN) %>% 
  rename(
    ADM1_EN = ADM2_EN,
    ADM2_EN = ADM3_EN,
  ) %>% 
  group_by(
    ADM1_EN, ADM2_EN
  ) %>% 
  summarise(
    Total_pop = sum(Total_pop),
    Male_pop = sum(Male_pop),
    Female_pop = sum(Female_pop),
    HH_num = sum(HH_num)
  ) %>% 
  ungroup() %>% 
  mutate( # clean some names to match with admin units in other df
    ADM2_EN = case_when(
      ADM2_EN == "Bawlakhe" ~ "Bawlake",
      TRUE ~ ADM2_EN
    )
  )

adm3_myanmar_pop <- all_units %>% 
  st_drop_geometry() %>% 
  filter(ADM0_EN == "Myanmar") %>% 
  left_join(population_myanmar, by = c("ADM1_EN", "ADM2_EN"))


##### laos -----
population_laos <- read_sf(
  "1. raw/admin units/laos/Laos_Population_Census_2015.shp"
  ) %>%
  st_drop_geometry() %>% 
  rename(
    Total_pop = unpdeaa01,
    Avg_HH_size = urhdeaa37,
    Sex_ratio = urpdeaa29,
    ADM1_EN = urcne,
    ADM2_EN = uscne,
    ADM3_EN = uucne
  ) %>% 
  mutate(
    ADM2_EN = gsub(" District", "", ADM2_EN),
    ADM3_EN = gsub("B\\. ", "", ADM3_EN)
  ) %>% 
  mutate(
    HH_num = Total_pop/Avg_HH_size,
    share_men = Sex_ratio/200, # sex ratio indicates number of men for 100 women
    Male_pop = Total_pop*share_men,
    Female_pop = Total_pop - Male_pop
  ) %>% 
  group_by(
    ADM1_EN, ADM2_EN, ADM3_EN
  ) %>% 
  summarise(
    Total_pop = sum(Total_pop),
    Male_pop = sum(Male_pop),
    Female_pop = sum(Female_pop),
    HH_num = sum(HH_num)
  ) %>% 
  ungroup() %>% 
  mutate( # clean names
    ADM2_EN = case_when(
      ADM2_EN == "Luangprabang " ~ "Luangprabang",
      ADM2_EN == "Pha oudom " ~ "Pha oudom",
      TRUE ~ ADM2_EN
    )
  )

adm3_laos_pop <- all_units %>% 
  st_drop_geometry() %>% 
  filter(ADM0_EN == "Laos") %>% 
  left_join(population_laos, by = c("ADM1_EN", "ADM2_EN", "ADM3_EN"))

### merge and save ----
adm3_pop <- bind_rows(adm3_laos_pop, adm3_myanmar_pop, adm_units_with_pop) %>% 
  select(adm3_id, Total_pop, Male_pop, Female_pop, HH_num)

#all_units_adm2 <- read_sf("2. intermediate/adm2_units.shp") %>% 
#  left_join(adm2_pop %>% select(Total_pop, adm2_id), by = "adm2_id")
#ggplot() + geom_sf(data = all_units_adm2, aes(fill=Total_pop))

fwrite(adm3_pop, "1. raw/population/population_by_adm3.csv")
### myanmar is wrong 
