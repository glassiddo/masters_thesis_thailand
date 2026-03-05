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
  lmtest, fixest, # twfe + sun and abraham event study
  jsonlite
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data")


### thailand --------
thai_province_names <- fromJSON("1. raw/population/provinces.json", flatten = TRUE) %>% 
  rename(ADM1_EN = provinceNameEn, ADM1_TH = provinceNameTh) %>% 
  select(-id, -ADM1_TH)

# from here - https://github.com/thailand-geography-data/thailand-geography-json/blob/main/src/provinces.json
pop_thai <- fread("1. raw/population/pop tambon.csv")
colnames_pop <- colnames(pop_thai)
translated_list <- polyglotr::google_translate(colnames_pop, target_lang = "en", source_language = "th")
translated_names <- as.character(translated_list)
colnames(pop_thai) <- translated_names

population_thailand <- pop_thai %>% 
  filter(
    `Sub -district name` == "-", `Registration Code` == 0
  ) %>% 
  mutate(
    Year = 2015,  # it appears before as 5812, which refers to Year-Month, 2558 hence 58 is 2015. 12 is december
    Total_pop = as.numeric(gsub(",","", `Total population`))
  ) %>% 
  rename(provinceCode = `Provincial Code`) %>% 
  filter(provinceCode != "0") %>% 
  left_join(thai_province_names, by = c("provinceCode")) %>% 
  select(ADM1_EN, Total_pop)


### myamar ----

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
  ) %>% 
  group_by(
    ADM1_EN
  ) %>% 
  summarise(
    Total_pop = sum(Total_pop),
    Male_pop = sum(Male_pop),
    Female_pop = sum(Female_pop),
    HH_num = sum(HH_num)
  ) %>% 
  ungroup() %>% 
  mutate(
    ADM1_EN = case_when(
      ADM1_EN == "Yinmabin" ~ "Yinmarbin",
      TRUE~ ADM1_EN
    )
  )


myanmar_with_pop <- myanmar %>% 
  st_drop_geometry() %>% 
  left_join(population_myanmar, by = "ADM1_EN")
