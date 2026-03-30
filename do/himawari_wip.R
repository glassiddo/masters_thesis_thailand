source("do/setup.R")

area <- ne_countries(continent = "Asia", returnclass = "sf") %>%
  transmute(iso3 = sov_a3) %>% 
  filter(iso3 == "THA") # %in% c("MMR", "THA", "LAO"))

bb <- list(
  lon_min = 92,  # Myanmar west
  lon_max = 108, # Laos east
  lat_min = 5,   # Thailand south
  lat_max = 28   # Myanmar north
)

### example
read_himawari <- function(yr, month, day, hour){
  
  file_name <- paste0(
    "H08_", yr, month, day, "_", hour,
    "00_L3WLF010_FLDK.06001_06001.csv"
    )
  
  path <- here(raw.dir, "himawari", paste0(yr, month), day, file_name)  
  
  if (!file.exists(path)) {
    return(list(
      file_exists = FALSE,
      data = NULL
    ))
  }  
  
  dt <- suppressWarnings(fread(path))
  
  list(
    file_exists = TRUE,
    data = dt
  )
}


process_himawari <- function(dates_to_run, area, bb){
  
  # read all files + file_exists flags
  results <- lapply(
    seq_len(nrow(dates_to_run)),
    function(i) {
      with(dates_to_run[i,], read_himawari(yr, month, day, hour))
    }
  )
  
  # store availability
  dates_to_run$file_exists <- map_lgl(results, "file_exists")
  
  # bind only existing data
  data_raw <- rbindlist(
    map(results, "data"),
    use.names = TRUE, fill = TRUE
  )
  
  if (nrow(data_raw) == 0) return(list(result=NULL, availability=dates_to_run))
  
  # fast bounding-box filter
  data_raw <- data_raw[
    lon >= bb$lon_min &
      lon <= bb$lon_max &
      lat >= bb$lat_min &
      lat <= bb$lat_max
  ]
  
  pts <- st_as_sf(data_raw, coords = c("lon", "lat"), crs = 4326) %>% 
    st_join(area, join = st_intersects) %>% 
    filter(!is.na(iso3)) 

  list(result = pts, availability = dates_to_run)
}

months <- sprintf("%02d", 1:5)
days <- sprintf("%02d", 1:31)
hours <- sprintf("%02d", 0:23)
years <- c("2016")

dates_to_run <- CJ(
  yr    = years,
  month = months,
  day   = days,
  hour  = hours
)

out <- process_himawari(dates_to_run, area, bb)
hw <- out$result 
# availability <- out$availability
# 
# ggplot(availability, aes(
#   x = as.integer(hour),
#   y = as.integer(day),
#   fill = file_exists
#   )) +
#   geom_tile(color = "grey70") +
#   scale_fill_manual(values = c("white", "darkgreen")) +
#   scale_x_continuous(breaks = 0:23) +
#   labs(title = "Himawari",
#        x = "Hour",
#        y = "Day",
#        fill = "Exists") +
#   theme_minimal(base_size = 14)

# ggplot() + 
#   geom_sf(data = area, aes(fill = iso3)) + 
#   geom_sf(data = result, aes(color = hour), size = 1) +
#   scale_color_viridis_d()

########## COMPARE WITH MODIS

rm(dates_to_run, out, area, bb)

md <- read_sf(
  "C:/Users/iddo2/Dropbox/thesis data/data/1. raw/fires/thailand/modis/fire_archive_M-C61_620555.shp"
  ) %>% 
  mutate(
    year = year(ACQ_DATE),
    month = month(ACQ_DATE),
    day = day(ACQ_DATE)
  ) %>% 
  filter(year %in% years, month %in% c(1:5))

# m1 <- modis %>% filter(month == 4)
# h1 <- result %>% mutate(hour = as.numeric(hour)) 
# 
# ggplot(data = h1, aes(x = factor(hour), fill = factor(month))) +
#   geom_histogram(stat = "count", position = "fill") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(
#     x = "Hour of Day",
#     y = "Proportion",
#     fill = "Month",
#     title = "Proportional Histogram of Hour by Month"
#   ) +
#   theme_minimal()

# h1 <- h1 %>% filter(month == 4)
# 
# ggplot() + 
#   geom_sf(data = area %>% filter(iso3 == "THA"), aes(fill = iso3)) + 
#   geom_sf(data = m1, size = 1, color = "white") +
#   geom_sf(data = h1, size = 1, color = "black") +
#   scale_color_viridis_d()
# 
# ggplot() + 
#   #geom_sf(data = area %>% filter(iso3 == "THA")) + 
#   geom_sf(data = h1, aes(color = day), size = 1) +
#   scale_color_viridis_c()

units <- read_sf(
  "C:/Users/iddo2/Dropbox/thesis data/data/2. intermediate/units/adm1_units_detailed.shp"
) %>% 
  #filter(ADM0_EN == "Thailand") %>% 
  mutate(
    region = case_when(
      ADM1_EN %in% c(
        "Chiang Mai", "Tak", "Nan", "Chiang Rai", "Mae Hong Son", 
        "Lamphun", "Lampang", "Phrae", "Phayao"
        ) ~ "North",
      ADM1_EN %in% c(
        "Amnat Charoen", "Bueng Kan", "Buri Ram", "Chaiyaphum", "Kalasin", 
        "Khon Kaen", "Loei", "Maha Sarakham", "Mukdahan", "Nakhon Phanom", 
        "Nakhon Ratchasima", "Nong Bua Lam Phu", "Nong Khai", "Roi Et", 
        "Sakon Nakhon", "Si Sa Ket", "Surin", "Ubon Ratchathani", 
        "Udon Thani", "Yasothon", 
        "Uttaradit"
      ) ~ "North East",
      ADM1_EN %in% c(
        "Kanchanaburi", "Phetchaburi", "Prachuap Khiri Khan", "Ratchaburi"
      ) ~ "West",
      ADM1_EN %in% c(
        "Ang Thong", "Chai Nat", "Kamphaeng Phet", "Krung Thep Maha Nakhon", 
        "Lop Buri", "Nakhon Nayok", "Nakhon Pathom", "Nakhon Sawan", 
        "Nonthaburi", "Pathum Thani", "Phetchabun", "Phichit", 
        "Phitsanulok", "Phra Nakhon Si Ayutthaya", "Samut Prakan", 
        "Samut Sakhon", "Samut Songkhram", "Saraburi", "Sing Buri", 
        "Sukhothai", "Suphan Buri", "Uthai Thani"
      ) ~ "Centre",
      ADM1_EN %in% c(
        "Chachoengsao", "Chanthaburi", "Chon Buri", "Prachin Buri", 
        "Rayong", "Sa Kaeo", "Trat"
      ) ~ "East",
      ADM1_EN %in% c(
        "Chumphon", "Krabi", "Nakhon Si Thammarat", "Narathiwat", 
        "Pattani", "Phangnga", "Phatthalung", "Phuket", "Ranong", 
        "Satun", "Songkhla", "Surat Thani", "Trang", "Yala"
      ) ~ "South",
      ADM1_EN == "Bangkok Metropolis" ~ "Bangkok",
      TRUE ~ NA_character_
    ),
    area = as.numeric(st_area(geometry))/1000000
  )

hw_ve <- vect(hw)
md_ve <- vect(md)
units_vec <- vect(units)

res_hw <- terra::extract(units_vec, hw_ve)
res_md <- terra::extract(units_vec, md_ve)

setDT(res_hw)
setDT(res_md)
res_hw <- res_hw[, .SD[1], by = id.y]
res_md <- res_md[, .SD[1], by = id.y]

hw <- hw %>% 
  mutate(id = row_number()) %>%
  st_drop_geometry() %>%
  left_join(res_hw %>% rename(id = id.y), by = "id")

md <- md %>% 
  mutate(id = row_number()) %>% 
  st_drop_geometry() %>% 
  left_join(res_md %>% rename(id = id.y), by = "id")

rm(res_hw, res_md, md_ve, hw_ve, units_vec)

# filter low likelihood fires
hw <- hw %>% 
  filter(`ave(confidence)` > 2.5) # above certain level of likelihood
md <- md %>% 
  filter(CONFIDENCE > 30) # not exactly comparable though

########## ignore repeated fires -----
# I treat fires as separate so far 
# but e.g., use 3km buffer and 6 (?) hours apart
# not properly capturing ignition but rather
# "first detection of a fire cluster within the buffer that was not present 
# recently (within the time frame) nearby in the same buffer"
# 


###############
u <- units %>% 
  st_drop_geometry() %>% 
  group_by(region) %>% 
  reframe(total_area = sum(area))

h1_nogeom <- hw %>% st_drop_geometry() %>% 
  filter(region != "Southern") %>% 
  left_join(u, by = "region")

# ggplot(data = hw, aes(x = factor(hour), fill = factor(region))) +
#   geom_histogram(stat = "count", position = "fill") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(
#     x = "Hour of Day",
#     y = "Proportion",
#     fill = "Month",
#     title = "Proportional Histogram of Hour by Month"
#   ) +
#   theme_minimal()

ggplot(hw %>% filter(region == "North"), 
       aes(x = hour, color = factor(`#year`))) +
  geom_line(stat = "count", linewidth = 1) +
  facet_wrap(~ `#year`, scales = "free_y") +
  labs(
    x = "Hour of Day",
    y = "Number of Fires",
    color = "Month",
    title = "Hourly Fire Distribution by Region and Month"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### 2016 already looks very different in the north to other regions

hour_groups <- c(5, 11, 17, 23) # six hours in each group, e.g., 0-5 

hw_grouped <- hw %>% 
  mutate(
    hour_g = case_when(
      hour %in% c(0:5) ~ 5,
      hour %in% c(6:11) ~ 11,
      hour %in% c(12:17) ~ 17,
      hour %in% c(18:23) ~ 23
    ),
    month = ifelse(str_length(month) == 1, paste0("0",month), month),
    day = ifelse(str_length(day) == 1, paste0("0",day), day)
  ) %>% 
  mutate(
    year = `#year`,
    time = paste0(hour_g, "-", day, "-", month, "-", year)
    ) %>% 
  group_by(adm1_id, time) %>% 
  reframe(
    n_h = n()
  )

dat <- units %>% 
  #select(-region) %>% 
  st_drop_geometry() %>% 
  crossing(hour_g = hour_groups) %>% 
  crossing(day = days) %>% 
  crossing(month = months) %>% 
  crossing(year = years) %>% 
  mutate(
    time = paste0(hour_g, "-", day, "-", month, "-", year)
  ) %>% 
  left_join(hw_grouped, by = c("time", "adm1_id")) %>%
  mutate(
    n_h = replace_na(n_h, 0),
    region = ifelse(is.na(region), ADM0_EN, region)
    )

options(scipen=999)
d <-  dat %>% 
  filter(
    !region %in% c("Bangkok", "South"),
    year == 2018
    ) %>%
  group_by(region, hour_g, month) %>%
  reframe(
    fires = sum(n_h)
  ) %>% 
  group_by(region, month) %>% 
  mutate(
    share = fires / sum(fires)
  ) %>%
  ungroup() 

ggplot(d, aes(x = region, y = share, fill = factor(hour_g))) +
  geom_col(alpha = 0.9) +  # width in days
  facet_wrap(~ month) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )
