rm(list=ls())
#dev.off()
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest # twfe + sun and abraham event study
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

setwd("C:/Users/iddo2/Documents/thesis data/data/")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# bans <- fread("ban dates.csv") %>% 
#   select(Region, `Ban start`, `Ban end`) %>%
#   rename(name = Region,
#          Ban_start = `Ban start`, 
#          Ban_end = `Ban end`) %>%
#   mutate(Ban_start = as.Date(Ban_start, format = "%Y-%m-%d"),
#          Ban_end = as.Date(Ban_end, format = "%Y-%m-%d"),
#          year = lubridate::year(Ban_start)) %>% 
#   filter(!is.na(Ban_start))

adm1 <- ne_states(returnclass = "sf") %>%
  filter(admin == "Thailand") %>%
  select(admin, name)

northern_cols <- c(
  "57T", # Chiang Rai - Chiang Rai Provincial Health Office 
  "73T", # Chiang Rai - Mae Sai District Public Health Office, Mae Sai District 
  "35T", # Chiang Mai - City Hall, Mueang District 
  "36T", # Chiang Mai - Yupparaj Wittayalai School 
  "41T", # Nakhon Sawan - Nakhon Sawan Irrigation Project 
  "67T", # Nan - Nan Municipality Office 
  "75T", # Nan - Chaloem Phrakiat Hospital 
  "70T", # Phayao - Phayao Provincial Stadium, Mueang District 
  "86T", # Phitsanulok - Chomnan Chalermprakiat Park 
  "69T", # Phrae - Phrae Meteorological Station, Mueang District 
  "58T", # Mae Hong Son - Mae Hong Son Provincial Health Office 
  "37T", # Lampang - Lampang Meteorological Station 
  "38T", # Lampang - Ban Sop Pad Subdistrict Health Promotion Hospital 
  "39T", # Lampang - Ban Tha Si Subdistrict Health Promotion Hospital 
  "40T", # Lampang - Mae Moh Provincial Waterworks Authority 
  "68T", # Lamphun - Lamphun Meteorological Station 
  "76T", # Tak - Non-Formal Education Center 
  "92T", # Uttaradit - Maha Mongkol Park
  "96T", # Sukhothai - Le Thai Community Public Park 
  "95T", # Phichit - Central Stadium 
  "97T", # Phetchabun - Phetchabun Park 
  "98T", # Uthai Thani - His Majesty the King's 80th Birthday Anniversary Park Station 
  "94T" # Kamphaeng Phet - Sirikit Thai Cultural Conservation Park 
  )

station_info <- tribble(
  ~station_code, ~station_name, ~province, ~latitude, ~longitude,
  # North
  "57T", "Chiang Rai Provincial Office", "Chiang Rai", 19.909826088785138, 99.82339663055491,
  "73T", "Mae Sai District Office", "Chiang Rai", 20.42751713723659, 99.88342001336424,
  "35T", "City Hall, Mueang District", "Chiang Mai", 18.838182433685905, 98.97083319860305,
  "36T", "Yupparaj Wittayalai School", "Chiang Mai", 18.791031910827638, 98.9886862575506,
  "41T", "Nakhon Sawan Irrigation Project", "Nakhon Sawan", 15.686515602019323, 100.11019225938418,
  "67T", "Nan Municipality Office", "Nan", 18.78946914208024, 100.77645428448452,
  "75T", "Chaloem Phrakiat Hospital", "Nan", 19.575714974211827, 101.08209044405191,
  "70T", "Phayao Provincial Stadium", "Phayao", 19.20304139698306, 99.89189509984676,
  "86T", "Chomnan Chalermprakiat Park", "Phitsanulok", 16.82226003393469, 100.26048283796715,
  "69T", "Phrae Meteorological Station", "Phrae", 18.128961536191884, 100.16232437287171,
  "58T", "Mae Hong Son Provincial Office", "Mae Hong Son", 19.304249638217943, 97.9716674286854,
  "37T", "Lampang Meteorological Station", "Lampang", 18.278587162377995, 99.50658392863234,
  "38T", "Ban Sop Pad Subdistrict Health Promotion Hospital", "Lampang", 18.25048752626866, 99.76395808637034,
  "39T", "Ban Tha Si Subdistrict Health Promotion Hospital", "Lampang", 18.44318936022414, 99.74648749939982,
  "40T", "Mae Moh Provincial Waterworks Authority", "Lampang", 18.281620084357332, 99.65957175330885,
  "68T", "Lamphun Meteorological Station", "Lamphun", 18.566732804396434, 99.03897192627812,
  "76T", "Non-Formal Education Center", "Tak", 16.884807446652804, 99.13581823371172, # might be incorrect
  "92T", "Maha Mongkol Park", "Uttaradit", 17.628378975700233, 100.09917509990244,
  "96T", "Le Thai Community Public Park", "Sukhothai", 17.0359539748195, 99.87213734396919, # might be incorrect
  "95T", "Central Stadium", "Phichit", 16.44326830074468, 100.3240048710373,
  "97T", "Phetchabun Park", "Phetchabun", 16.419468269440276, 101.15704312859205,
  "98T", "His Majesty the King's 80th Birthday Anniversary Park Station", "Uthai Thani", 15.379224718567016, 100.06361430677462, # might be incorrect
  "94T", "Sirikit Thai Cultural Conservation Park", "Kamphaeng Phet", 16.475621166410903, 99.52437536678781,
  ## North East
  "46T", "Khon Kaen Regional Water Resources Office 4", "Kohn Kaen", 16.491458975915336, 102.83270119440998,
  "47T", "Nakhon Ratchasima Municipality Wastewater Pumping Station", "Nakhon Ratchasima", 15.00319604601279, 102.26557451972607,
  "88T", "Nakhon Phanom Meteorological Station", "Nakhon Phanom", 17.412467652992618, 104.77863872344615,
  "82T", "Nong Thin Public Park", "Nong Khai", 17.87714025366492, 102.72632326578716,
  "83T", "OTOP Center Ubon", "Ubon Ratchathani", 15.245003652156383, 104.84626633873856,
  "90T", "Sakon Nakhon Meteorological Department Radar Station", "Sakon Nakhon", 17.15659298104709, 104.13321422344006,
  "91T", "Nong Prajak Park", "Udon Thani", 17.419211609362087, 102.78083117927098,
  "101T", "Buriram Provincial Hall", "Buri Ram", 14.944415202546018, 103.10675788475338,
  "102T", "Mukdahan Province Stadium", "Mukdahan", 16.54375729341488, 104.72056128109618,
  "106T", "Nong Bueng Kan Public Park", "Bueng Kan", 18.360919018338006, 103.66185676949031,
  "107T", "Kaeng Don Klang Public Park", "Kalasin", 16.442743691454687, 103.51252050041337,
  "108T", "Muang Chaiyaphum Municipal Stadium", "Chaiyaphum", 15.81066875966016, 102.02358947489898,
  "109T", "Phaya Thaen Public Park", "Yasothon", 15.78355724698043, 104.15411730564749,
  "110T", "Amnat Charoen Municipality Office", "Amnat Charoen", 15.87610727074055, 104.62259608661813,
  "111T", "Surin Meteorological Station", "Surin", 14.90281691172538, 103.49971415968037,
  "112T", "Sisaket Meteorological Station", "Sisaket", 15.087065770947131, 104.32760209952032,
  "113T", "Bueng Planchai", "Roi Et", 16.05829096628694, 103.65115536943618,
  "114T", "Pramuan Suk Health Park", "Maha Sarakham", 16.214072505971384, 103.31100812131932,
  "115T", "Nong Bua Lamphu - Provincial Administrative Organization", "Lam Phu", 17.194975120528458, 102.4589608191739,
  # Central and Western
  "79T", "Kanchanaburi Meteorological Station", "Kanchanaburi", 14.022568451099184, 99.53596112337445,
  "26T", "Office of Environmental and Pollution Control No. 8", "Ratchaburi", 13.524119558676478, 99.81416145404603,
  "24T", "Na Phralan Police Station", "Saraburi", 14.68659661264431, 100.8713022635834,
  "25T", "Disaster Prevention and Mitigation Department of Saraburi Municipality", "Saraburi", 14.528308272988104, 100.91179705591009,
  ## others
  "61T", "Bodindecha (Sing Singhaseni) School", "Bangkok", 13.767966230633986, 100.61469099453475,
  "27T", "Samut Sakhon Wittayalai School", "Samut Sakhon", 13.54861002771207, 100.26579520375844,
  "44T", "Hatyai City Municipality Office", "Songkhla", 7.029300326036677, 100.4716390198189,
  "54T", "Din Daeng Community Housing", "Bangkok", 13.765710459180973, 100.55408631373822
  ) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs("EPSG:4326")

load_north_data <- function(year) {
  df <- readxl::read_excel(paste0("pollution/PM2.5/PM2.5(",year,").xlsx")) %>% 
    mutate(
      Date = as.Date(as.numeric(Date), origin = "1899-12-30")
      ) %>% 
    # select(
    #   Date, any_of(northern_cols)
    #   ) %>%
    mutate(
      across(-c(Date), ~as.numeric(.))
      )
}

pollution_data <- tibble()

for (y in 2011:2021) {
  yearly_pol_data <- load_north_data(y)
  pollution_data <- bind_rows(pollution_data, yearly_pol_data)
  rm(yearly_pol_data)
}

pollution_data_long <- pollution_data %>% 
  pivot_longer(
    cols = -Date,
    names_to = "station_code",
    values_to = "PM2.5"
  ) %>%
  left_join(station_info, by = "station_code") %>% 
  filter(!is.na(station_name))

pollution_by_month_year <- pollution_data_long %>% 
  st_drop_geometry() %>% 
  mutate(
    month = month(Date),
    year = year(Date),
    north = ifelse(province %in% northern_regions_en, "North", "Elsewhere")
  ) %>% 
  filter(
    !is.na(PM2.5), year < 2021, year > 2011, month < 6
  ) %>% 
  group_by(Date, north) %>% 
  summarise(
    mean_pm25 = mean(PM2.5, na.rm = T),
    n_stations = length(unique(station_code)),
    .groups = 'drop'
  ) %>% 
  mutate(year = year(Date))
  

ggplot(pollution_by_month_year, aes(x = Date, y = mean_pm25, color = north)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~year, scales = "free") +
  labs(title = "Daily PM2.5 Pollution by Region and Year",
       x = "Date", 
       y = "Mean PM2.5 (μg/m³)",
       color = "Region") +
  scale_x_date(date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


rm(pollution_data_long_clean)
##### older stuff ------
stations_buffered25km <- station_info %>% st_buffer(25000)
stations_buffered35km <- station_info %>% st_buffer(35000)

stations_buffered_intersections <- st_intersection(
  stations_buffered35km, 
  adm1
  ) %>% 
  st_set_geometry(NULL) %>% 
  rename(
    adm1_province = name,
    station_province = province
  ) %>% 
  mutate(
    overlap = ifelse(adm1_province != station_province, 1, 0)
  ) %>% 
  filter(
    overlap == 1, # only care about those with mismatch
    adm1_province %in% northern_regions_en # otherwise i dont have ban dates
  ) %>% 
  select(station_name, adm1_province)

pollution_data_long_fake <- pollution_data_long %>% 
  left_join(
    stations_buffered_intersections, by = "station_name"
  ) %>% 
  filter(
    !is.na(adm1_province),
    !is.na(PM2.5)
    ) 
  
ggplot() +
  geom_sf(data = adm1 %>%   
            mutate(region_group = ifelse(name %in% northern_regions_en, "Northern", "Other")),
          aes(fill = region_group), color = "black") +
  geom_sf(data = station_info, color = "red") +
  scale_fill_manual(values = c("Northern" = "lightgreen", "Other" = "gray90")) +
  theme_minimal()

# seems to be working fine

year_spec <- 2017
region_spec <- "Lamphun"

chiang_mai_ban <- bans %>%
  filter(name == region_spec) %>%
  select(Ban_start, Ban_end) %>% 
  filter(year(Ban_start) == year_spec)

ggplot(
  data = pollution_data_long %>%
    filter(
      #adm1_province == region_spec, 
      # if use this - it combines the ban dates of a province
      # with values from pollution stations just across the border
      # so it shouldn't be responsive for ban dates
      year(Date) == year_spec, month(Date) < 6
    ),
  # data = pollution_data_long %>%
  #   filter(
  #     # if use this - it combines the ban dates of a province
  #     # with its actual values
  #     # so its should be repsonsive for ban dates
  #     province == region_spec,
  #     year(Date) == year_spec, month(Date) < 6
  #     ),
    aes(x = Date, y = PM2.5)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  # geom_vline(aes(xintercept = as.numeric(chiang_mai_ban$Ban_start)), 
  #            color = "red", linetype = "dashed", size = 1) +
  # geom_vline(aes(xintercept = as.numeric(chiang_mai_ban$Ban_end)), 
  #            color = "red", linetype = "dashed", size = 1) + 
  labs(title = paste0(region_spec, " PM2.5 and ban dates in ", year_spec))

# Get date of first non-NA PM2.5 reading for each province
first_non_na_dates <- pollution_data_long %>%
  filter(!is.na(PM2.5)) %>%  # Remove NA PM2.5 values
  group_by(province) %>%
  arrange(Date) %>%  # Sort by date
  slice(1) %>%  # Take the first row for each province
  select(province, Date) %>%
  ungroup()

modis_nogeom <- modis_nogeom %>% rename(Date = ACQ_DATE, province = name)

ggplot() + 
  geom_line(data = pollution_data_long %>% 
              filter(province == "Lampang"),
            aes(x = Date, y = PM2.5)
            ) + 
  geom_line(data = modis_nogeom %>%
              filter(province == "Lampang"),
            aes(x = Date, y = n_fires_modis)
            )

pollution_for_join <- pollution_data_long %>% 
  group_by(Date, province) %>% 
  summarise(mean_PM2.5_prov = mean(PM2.5, na.rm = T)) %>% 
  ungroup()

write.csv(pollution_for_join, "pollution/pollution_by_date_north9.csv")
