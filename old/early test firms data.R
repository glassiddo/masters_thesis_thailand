setwd("C:/Users/iddo2/Documents/thesis data")
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table, 
  lubridate, ggplot2, readxl,  fixest, DIDmultiplegtDYN,
  lattice, CFtime,
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  ncdf4 # Read, write, and create netCDF files
)
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

### modis data from gee -----
modis19 <- fread("fireCounts_northern_thailand_timeseries19.csv")
modis20 <- fread("fireCounts_northern_thailand_timeseries20.csv")

north_thai_regions <- unique(modis19$ADM1_NAME)

modis19 <- modis19 %>% 
  select(ADM1_NAME, inBan, fire_count, date)
  
modis20 <- modis20 %>% 
  select(ADM1_NAME, inBan, fire_count, date) %>% 
  mutate(
    inBan = ifelse(ADM1_NAME == "Chiang Rai", 0, inBan)
  )

thai_ban19 <- modis19 %>%
  group_by(ADM1_NAME) %>%
  summarise(
    ban_start = min(date[inBan == 1], na.rm = TRUE),  # First date of inBan == 1
    ban_end = max(date[inBan == 1], na.rm = TRUE)     # Last date of inBan == 1
  )

thai_ban20 <- modis20 %>%
  group_by(ADM1_NAME) %>%
  summarise(
    ban_start = min(date[inBan == 1], na.rm = TRUE),  # First date of inBan == 1
    ban_end = max(date[inBan == 1], na.rm = TRUE)     # Last date of inBan == 1
  )

thai_ban <- thai_ban19 %>% 
  rename(
    ban_start19 = ban_start,
    ban_end19 = ban_end
  ) %>% 
  left_join(thai_ban20, by = "ADM1_NAME") %>% 
  rename(
    ban_start20 = ban_start,
    ban_end20 = ban_end
  )

modis_ban_relative19 <- modis19 %>%
  left_join(thai_ban19, by = "ADM1_NAME") %>%
  mutate(
    date = as.Date(date),
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  ) 

modis_ban_relative20 <- modis20 %>%
  left_join(thai_ban20, by = "ADM1_NAME") %>%
  mutate(
    date = as.Date(date),
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  ) 

modis_ban_relative <- bind_rows(modis_ban_relative19, modis_ban_relative20)
date_threshold <- 15

ban_relative_start <- modis_ban_relative %>% 
  filter(
    days_group_pre > -date_threshold,
    days_group_pre < date_threshold
  )

ban_relative_end <- modis_ban_relative %>% 
  filter(
    days_group_post > -date_threshold,
    days_group_post < date_threshold
  )

ggplot(ban_relative_start, aes(x = days_group_pre, y = fire_count, color = ADM1_NAME)) +
  geom_line() +  
  labs(
    title = "Fire Counts Over Time by Region",
    x = "Date compared to start",
    y = "Fire Count"
  ) +
  theme_minimal()

ggplot(ban_relative_end, aes(x = days_group_post, y = fire_count, color = ADM1_NAME)) +
  geom_line() +  
  labs(
    title = "Fire Counts Over Time by Region",
    x = "Date compared to end",
    y = "Fire Count"
  ) +
  theme_minimal()

### global fire data from global fire emissions db ----

##### these is used once to create the db for each year, commented out after saving the filtered file ----
# # Function to convert a NetCDF file to a data frame
# nc_to_df_filtered <- function(nc_file_path) {
#   # Open the NetCDF file
#   nc_data <- nc_open(nc_file_path)
# 
#   # Get the variable names
#   var_names <- names(nc_data$var)
# 
#   # Get time, lat, and lon
#   time <- ncvar_get(nc_data, "time")
#   lat <- ncvar_get(nc_data, "lat")
#   lon <- ncvar_get(nc_data, "lon")
# 
#   # Convert time to a readable format
#   time_converted <- as.POSIXct(time * 3600, origin = "1800-01-01", tz = "UTC")
# 
#   # Define the bounding box for Northern Thailand
#   lon_min <- 95.86442908004929
#   lon_max <- 107.83952673629929
#   lat_min <- 10.729710197709874
#   lat_max <- 21.109110591704187
# 
#   # Filter lat/lon within the bounding box
#   lat_filter <- lat >= lat_min & lat <= lat_max
#   lon_filter <- lon >= lon_min & lon <= lon_max
# 
#   # Apply the filter to lat/lon
#   lat_filtered <- lat[lat_filter]
#   lon_filtered <- lon[lon_filter]
# 
#   # Create the base data frame with filtered lat, lon, and time
#   df <- expand.grid(lon = lon_filtered, lat = lat_filtered, time = time_converted)
# 
#   # Convert to data.table for faster processing
#   df <- as.data.table(df)
# 
#   # Loop over each variable, filter, and add it to the data frame
#   for (var in var_names) {
#     var_data <- ncvar_get(nc_data, var)
# 
#     # Apply the same filter to the variable data
#     var_data_filtered <- var_data[lon_filter, lat_filter, ]
# 
#     df[[var]] <- as.vector(var_data_filtered)
#   }
# 
#   # Close the NetCDF file
#   nc_close(nc_data)
# 
#   return(df)
# }
# df_gfed5_201912 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201812.nc")
# df_gfed5_202001 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201901.nc")
# df_gfed5_202002 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201902.nc")
# df_gfed5_202003 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201903.nc")
# df_gfed5_202004 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201904.nc")
# df_gfed5_202005 <- nc_to_df_filtered("global fire emissions/GFED5_Beta_daily_201905.nc")
# 
# # deleted these files as they're really heavy, downloadable from
# # https://www.globalfiredata.org/data.html
# df_gfed <- rbind(df_gfed5_201912, df_gfed5_202001, df_gfed5_202002,
#                  df_gfed5_202003, df_gfed5_202004, df_gfed5_202005)
# rm(df_gfed5_201912, df_gfed5_202001, df_gfed5_202002, df_gfed5_202003,
#    df_gfed5_202004, df_gfed5_202005)
# 
# write.csv(df_gfed, "global fire emissions/GFED5_Beta_Daily_FILTERED_OWN_19")

##### read the new file and analyse it ----
df_gfed19 <- fread("global fire emissions/GFED5_Beta_Daily_FILTERED_OWN_19")
df_gfed20 <- fread("global fire emissions/GFED5_Beta_Daily_FILTERED_OWN_20")

fao_g1 <- st_read("fao gaul level 1/g2015_2014_1.shp")
sf_use_s2(FALSE)

process_gfed_data <- function(df_gfed, fao_shapefile_path) {
  df_gfed <- df_gfed[, c("time", "lon", "lat", "PM2p5", "NOx", "N2O", "CO2")]
  df_gfed$time <- as.Date(df_gfed$time)
  
  df_gfed_sf <- st_as_sf(df_gfed, coords = c("lon", "lat"), crs = 4326)  # Assuming WGS84 CRS
  
  df_gfed_sf <- st_join(df_gfed_sf, fao_g1[c("ADM0_NAME", "ADM1_NAME")]) %>%
    na.omit()
  
  df_gfed_summary <- df_gfed_sf %>% 
    st_set_geometry(NULL) %>% 
    group_by(time, ADM0_NAME, ADM1_NAME) %>% 
    rename(date = time) %>% 
    summarise(
      PM2p5 = sum(PM2p5),
      NOx = sum(NOx), 
      N2O = sum(N2O),
      CO2 = sum(CO2),
      .groups = 'drop'
    )
  
  return(df_gfed_summary)
}

df_gfed19 <- process_gfed_data(df_gfed19)
df_gfed20 <- process_gfed_data(df_gfed20)

df_gfed_ban19 <- df_gfed19 %>%
  left_join(thai_ban19, by = "ADM1_NAME") %>% 
  mutate(
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  ) %>% 
  filter(ADM1_NAME %in% north_thai_regions)

df_gfed_ban20 <- df_gfed20 %>%
    left_join(thai_ban20, by = "ADM1_NAME") %>% 
  mutate(
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  ) %>% 
    filter(ADM1_NAME %in% north_thai_regions)

df_gfed_ban <- bind_rows(df_gfed_ban19, df_gfed_ban20)
df_gfed_ban_start <- df_gfed_ban %>% 
  filter(
    days_group_pre > -date_threshold,
    days_group_pre < date_threshold
  )

df_gfed_ban_end <- df_gfed_ban %>% 
  filter(
    days_group_post > -date_threshold,
    days_group_post < date_threshold
  )

ggplot(df_gfed_ban_start, aes(x = days_group_pre, y = PM2p5, color = ADM1_NAME)) +
  geom_line() +  
  labs(
    title = "PM2p5 Sum Over Time by Region",
    x = "Date compared to start",
    y = "Fire Count"
  ) +
  theme_minimal()

ggplot(df_gfed_ban_end, aes(x = days_group_post, y = PM2p5, color = ADM1_NAME)) +
  geom_line() +  
  labs(
    title = "PM2p5 Sum Over Time by Region",
    x = "Date compared to end",
    y = "Fire Count"
  ) +
  theme_minimal()


### aqua and terra values -----
aquaterra19 <- fread("FireCounts_Northern_Thailand_Terra_Aqua19.csv")
aquaterra20 <- fread("FireCounts_Northern_Thailand_Terra_Aqua20.csv")

for (df in list(aquaterra19, aquaterra20)){
  df <- df %>% 
    select(ADM1_NAME, inBan, terra_fire_count, aqua_fire_count, date) %>% 
    mutate(noon_premium = aqua_fire_count - terra_fire_count)
}

aquaterra19 <- aquaterra19 %>%
  left_join(thai_ban19, by = "ADM1_NAME") %>%
  mutate(
    date = as.Date(date),
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  )

aquaterra20 <- aquaterra20 %>%
  left_join(thai_ban20, by = "ADM1_NAME") %>%
  mutate(
    date = as.Date(date),
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1)
  )

aquaterra <- bind_rows(aquaterra19, aquaterra20) %>% 
  mutate(noon_premium = aqua_fire_count - terra_fire_count)

date_threshold <- 50

aquaterra_start <- aquaterra %>% 
  filter(
    days_group_pre > -date_threshold,
    days_group_pre < date_threshold
  ) %>% 
  group_by(days_group_pre) %>%
  summarise(terra_fire_count = sum(terra_fire_count, na.rm = TRUE),
            aqua_fire_count = sum(aqua_fire_count, na.rm = TRUE)) %>% 
  mutate(noon_premium = aqua_fire_count - terra_fire_count)

aquaterra_end <- aquaterra %>% 
  filter(
    days_group_post > -date_threshold,
    days_group_post < date_threshold
  ) %>% 
  group_by(days_group_post) %>%
  summarise(terra_fire_count = sum(terra_fire_count, na.rm = TRUE),
            aqua_fire_count = sum(aqua_fire_count, na.rm = TRUE)) %>% 
  mutate(noon_premium = aqua_fire_count - terra_fire_count)

ggplot(aquaterra_start, aes(x = days_group_pre)) +
  geom_line(aes(y = terra_fire_count, color = "Terra (10:30)")) +
  geom_line(aes(y = aqua_fire_count, color = "Aqua (13:30)")) +
  labs(
    title = "Total Fire Counts Over Time (Terra and Aqua)",
    x = "Date compared to start",
    y = "Fire Count",
    color = "Fire Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Terra (10:30)" = "blue", "Aqua (13:30)" = "red"))

ggplot(aquaterra_end, aes(x = days_group_post)) +
  geom_line(aes(y = terra_fire_count, color = "Terra (10:30)")) +
  geom_line(aes(y = aqua_fire_count, color = "Aqua (13:30)")) +
  labs(
    title = "Total Fire Counts Over Time (Terra and Aqua)",
    x = "Date compared to start",
    y = "Fire Count",
    color = "Fire Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Terra (10:30)" = "blue", "Aqua (13:30)" = "red"))

ggplot(aquaterra_start, aes(x = days_group_pre, y = noon_premium)) +
  geom_line() +
  labs(
    title = "Noon premium (Terra and Aqua)",
    x = "Date compared to start",
    y = "Noon premium",
    color = "Fire Source"
  ) +
  theme_minimal()

ggplot(aquaterra_end, aes(x = days_group_post, y = noon_premium)) +
  geom_line() +
  labs(
    title = "Noon premium (Terra and Aqua)",
    x = "Date compared to start",
    y = "Noon premium",
    color = "Fire Source"
  ) +
  theme_minimal()

aquaterra_summary <- aquaterra %>%
  # Create 5-day period groupings based on days_from_start
  mutate(days_group = floor(days_from_start / 10) * 10) %>%
  # Group by region (ADM1_NAME) and the 10-day period
  group_by(ADM1_NAME, days_group) %>%
  # Summarize the average noon_premium for each group
  summarise(avg_noon_premium = mean(noon_premium, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter() %>% 
  filter(
    days_group < 90, days_group > -60
  ) 
  
aquaterra_summary_nation <- aquaterra %>% 
  mutate(days_group = floor(days_from_start / 5) * 5) %>%
  # Group by the 5-day period
  group_by(days_group) %>%
  # Summarize the average noon_premium for each group
  summarise(avg_noon_premium = mean(noon_premium, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(
    days_group < 60, days_group > -30
  )

ggplot(aquaterra_summary, aes(x = days_group, y = avg_noon_premium, color = ADM1_NAME)) +
  geom_line() +
  labs(
    title = "Noon premium", 
    subtitle = "how much extra burning in noon compared to morning)",
    x = "Date compared to start of ban",
    y = "Fire Count",
    color = "Fire Source"
  ) +
  theme_minimal()

ggplot(aquaterra_summary_nation, aes(x = days_group, y = avg_noon_premium)) +
  geom_line() +
  labs(
    title = "Noon premium", 
    subtitle = "how much extra burning in noon compared to morning)",
    x = "Date compared to start of ban",
    y = "Fire Count",
    color = "Fire Source"
  ) +
  theme_minimal()

#### random estimations -----
modis <- bind_rows(modis19, modis20)
modis_join <- modis %>% select(-inBan) %>% mutate(date = as.Date(date))
gfed_join <- df_gfed_ban %>% select(date, ADM1_NAME, 'NOx', 'N2O', 'CO2', 'PM2p5') 
full_data <- aquaterra %>% 
  left_join(modis_join, by = c("ADM1_NAME", "date")) %>% 
  left_join(gfed_join, by = c("ADM1_NAME", "date"))
print(coeftest(feols(
  #noon_premium 
  fire_count
  #PM2p5
             #terra_fire_count
             #aqua_fire_count
             ~ inBan | ADM1_NAME + date, 
          data = full_data, 
          vcov = cluster ~ ADM1_NAME)
))

mod_dCDH24 <- summary(
  did_multiplegt_dyn(
    df = full_data, 
    outcome = "fire_count", 
    #continuous = 1, # better with bootstrapped SEs
    group = "ADM1_NAME", time = "date", treatment = "inBan", 
    effects = 10, # no. of post-treatment periods
    placebo = 10, # no. of pre-treatment periods
    #controls = c("lpop", "precipitation"), 
    cluster = "ADM1_NAME",
    #by = "bla"
  )
)

### south east asia data ----
asean <- read_csv("FireCounts_SEA_Terra_Aqua_simple.csv")
fao_g1 <- st_read("fao gaul level 1/g2015_2014_1.shp")
fao <- fao_g1 %>% 
  select(ADM0_NAME, ADM1_NAME) %>% 
  st_set_geometry(NULL)
asean <- asean %>% 
  left_join(thai_ban, by = "ADM1_NAME") %>% 
  left_join(fao, by = "ADM1_NAME")

asean <- asean %>% 
  mutate(
    date = as.Date(date),
    ban_start = as.Date(ban_start),
    ban_end = as.Date(ban_end),
    days_from_start = as.numeric(date - ban_start),
    days_from_end = as.numeric(date - ban_end),
    days_group_pre = floor(days_from_start / 1),
    days_group_post = floor(days_from_end / 1),
    isBan = ifelse(date >= ban_start & date <= ban_end, 1, 0)
  )
rm(fao, fao_g1, thai_ban)

asean_summary <- asean %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(1, 2, 3, 4, 5)) %>% 
  group_by(isBan, ADM0_NAME) %>% 
  summarise(
    aqua_noon_avg = mean(aqua_fire_count),
    terra_morning_avg = mean(terra_fire_count)
  ) %>% 
  mutate(noon_multiplier = aqua_noon_avg / terra_morning_avg)

#### aqua terra orbit times ----
# https://oceandata.sci.gsfc.nasa.gov/overpass_pred/
# calculated predicted orbit times for january to may (inc) 2025
input_file <- "satpasses_1730296441.txt"
mixed_aquaterra <- read.table(input_file, header = FALSE, sep = "", fill = TRUE, stringsAsFactors = FALSE) %>% 
  select(V1, V2, V3, V4)
aqua_start <- which(mixed_aquaterra$V1 == "MODIS-Aqua")
terra_start <- which(mixed_aquaterra$V1 == "MODIS-Terra")

# Extract Aqua and Terra data into separate data frames
aqua_data <- mixed_aquaterra[(aqua_start + 2):(terra_start - 1), ] %>% 
  mutate(Date = as.Date(paste(V1, V2, V3), format = "%d %b %Y")) %>%
  select(Date, Aqua = V4)  # Rename V4 to Hour and keep only necessary columns
terra_data <- mixed_aquaterra[(terra_start + 2):nrow(mixed_aquaterra), ] %>% filter(V3 == "2025") %>% 
  mutate(Date = as.Date(paste(V1, V2, V3), format = "%d %b %Y")) %>%
  select(Date, Terra = V4)  # Rename V4 to Hour and keep only necessary columns
mixed_aquaterra <- merge(aqua_data, terra_data, by = "Date", all = T) %>%
  na.omit() %>% 
  mutate(
    Aqua = ymd_hms(paste(Date, Aqua)) + hours(7),
    Terra = ymd_hms(paste(Date, Terra)) + hours(7),
    Diff = abs(difftime(
      ymd_hm(paste(Date, format(Aqua, "%H:%M"))),
      ymd_hm(paste(Date, format(Terra, "%H:%M"))),
      units = "hours"))
  )
rm(aqua_data, terra_data, aqua_start, terra_start, input_file)

mixed_aquaterra_summary <- mixed_aquaterra %>% 
  summarise(
    Avg_Aqua_hour = mean(hour(Aqua)),
    Avg_Terra_hour = mean(hour(Terra)),
    Avg_Diff_minutes = mean(Diff)
  )

mixed_aquaterra <- mixed_aquaterra %>% 
  mutate(
    Aqua = format(Aqua, "%H:%M"),
    Terra = format(Terra, "%H:%M")
  )


### 02/11/24 data -------
firms <- fread("0211 data/firms_thailand.csv")
aquaterra <- fread("0211 data/aqua_terra_thailand.csv")
bans <- fread("0211 data/Northern Thailand Crop Burning Ban.csv")
precipitation <- fread("0211 data/Daily_Precipitation_Northern_Thailand_Mean.csv")
fao_g1 <- st_read("fao gaul level 1/g2015_2014_1.shp")

fao_g1 <- fao_g1 %>% 
  select(ADM0_NAME, ADM1_NAME) %>% 
  filter(ADM0_NAME %in% c("Thailand", "Lao People's Democratic Republic",
                          "Myanmar"))

sf::sf_use_s2(FALSE)

precipitation[, unique_id := .GRP, by = .(lon, lat)]
precipitation <- precipitation[, .SD[which.max(mean)], by = .(date, unique_id)]

precipitation_index <- precipitation %>% 
  mutate(date = as.Date(date),
         year = year(date), 
         month = month(date), 
         day = day(date)) %>% 
  filter(year == "2020", month == "6", day == "3") %>% # choose a random day
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

precipitation_index <- st_intersection(fao_g1, precipitation_index)
relevant_regions <- unique(precipitation_index$ADM1_NAME)

fao_g1 <- fao_g1 %>% 
  filter(ADM1_NAME %in% relevant_regions) %>%  
  st_transform(crs = st_crs(precipitation_index))

ggplot() +
  geom_sf(data = fao_g1, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = precipitation_index, aes(color = mean), size = 4, alpha = 0.7) +
  scale_color_viridis_c() +
  theme_minimal()

precipitation_index <- precipitation_index %>% 
  select(unique_id, ADM0_NAME, ADM1_NAME) %>% 
  st_set_geometry(NULL)

precipitation <- precipitation %>% left_join(precipitation_index, by = "unique_id") %>% 
  select(mean, date, unique_id, ADM0_NAME, ADM1_NAME) %>% 
  rename(precipitation = mean)

precipitation_by_region <- precipitation %>% 
  group_by(ADM1_NAME, date) %>% 
  summarise(precipitation = mean(precipitation)) %>% 
  rename(Region = ADM1_NAME)

fires <- aquaterra %>% left_join(firms, by = c("date", "ADM1_NAME")) %>%
  rename(Region = ADM1_NAME)

bans <- bans %>% 
  select(Region, `Ban start`, `Ban end`)

bans[, `Ban start` := as.Date(`Ban start`, format = "%Y-%m-%d")]
bans[, `Ban end` := as.Date(`Ban end`, format = "%Y-%m-%d")]

fires[, is_ban := 0]  # Initialize is_ban
fires[bans, on = .(Region = Region, date >= `Ban start`, date <= `Ban end`), 
      is_ban := 1]

fires <- fires %>% left_join(precipitation_by_region, by = c("date", "Region"))

print(coeftest(feols(
  fire_count
  #aqua_fire_count
  #terra_fire_count
  ~ is_ban + precipitation 
  #+ is_ban*precipitation
  | Region + date, 
  data = fires, 
  vcov = cluster ~ Region)
))


expanded_fires <- fires[bans, on = .(Region =Region), allow.cartesian = TRUE]

# Step 2: Calculate days to the Ban start and Ban end for each matching row
expanded_fires[, `:=`(
  days_to_ban_start = as.integer(date - `Ban start`),
  days_to_ban_end = as.integer(date - `Ban end`)
)]

# Step 3: Find the closest Ban start and Ban end for each firm date
# We take the minimum absolute days difference for each date and region combination
fires_closest_bans <- expanded_fires[
  , .(
    days_to_closest_ban_start = days_to_ban_start[which.min(abs(days_to_ban_start))],
    days_to_closest_ban_end = days_to_ban_end[which.min(abs(days_to_ban_end))]
  ),
  by = .(Region, date)
]

# Step 4: Merge the closest ban information back to the original firms table
fires <- merge(fires, fires_closest_bans, by = c("Region", "date"), all.x = TRUE)
fires <- fires %>% mutate(year = year(date)) 

fires_filtered_start <- fires %>% 
  group_by(year) %>% 
  filter(days_to_closest_ban_start > -30, days_to_closest_ban_start < 30) %>% 
  mutate(ban_enforced = ifelse(year > 2016, "After 16", "15-16"))

print(coeftest(feols(
  fire_count
    ~ is_ban | Region + date, 
  data = fires_filtered_start, 
  vcov = cluster ~ Region)
))

fires_filtered_start <- fires_filtered_start %>% 
  group_by(days_to_closest_ban_start, ban_enforced) %>% 
  summarise(
    fire_count = mean(fire_count, na.rm = T),
    aqua_fire_count = mean(aqua_fire_count, na.rm = T),
    terra_fire_count = mean(terra_fire_count, na.rm = T),
    precipitation = mean(precipitation, na.rm = T)
  )

ggplot(fires_filtered_start, 
       aes(x = days_to_closest_ban_start, y = fire_count, color = ban_enforced)
       ) +
  geom_line() +  
  labs(
    title = "Fire Counts Over Time",
    x = "Date compared to start",
    y = "Fire Count"
  ) +
  theme_minimal()

ggplot(fires_filtered_start, aes(x = days_to_closest_ban_start)) +
  geom_line(aes(y = aqua_fire_count, color = "Aqua Fire Count")) +
  geom_line(aes(y = terra_fire_count, color = "Terra Fire Count")) +
  labs(
    title = "Aqua and Terra Fire Counts Over Time",
    x = "Days Relative to Ban Start",
    y = "Fire Count"
  ) +
  scale_color_manual(values = c("Aqua Fire Count" = "blue", "Terra Fire Count" = "red")) +
  theme_minimal()

fires_filtered_end <- fires %>% 
  group_by(year) %>% 
  filter(days_to_closest_ban_end > -30, days_to_closest_ban_end < 30) %>% 
  mutate(ban_enforced = ifelse(year > 2016, "After 16", "15-16"))
  
print(coeftest(feols(
  fire_count
  #aqua_fire_count
  #terra_fire_count
  ~ is_ban | Region + date, 
  data = fires_filtered_end, 
  vcov = cluster ~ Region)
))

fires_filtered_end <- fires_filtered_end %>% 
  group_by(days_to_closest_ban_end, ban_enforced) %>% 
  summarise(
    fire_count = mean(fire_count, na.rm = T),
    aqua_fire_count = mean(aqua_fire_count, na.rm = T),
    terra_fire_count = mean(terra_fire_count, na.rm = T),
    precipitation = mean(precipitation, na.rm = T)
  )

ggplot(fires_filtered_end, 
       aes(x = days_to_closest_ban_end, y = fire_count, color = ban_enforced)
       ) +
  geom_line() +  
  labs(
    title = "Fire Counts Over Time",
    x = "Date compared to end",
    y = "Fire Count"
  ) +
  theme_minimal()

ggplot(fires_filtered_end, aes(x = days_to_closest_ban_end)) +
  geom_line(aes(y = aqua_fire_count, color = "Aqua Fire Count")) +
  geom_line(aes(y = terra_fire_count, color = "Terra Fire Count")) +
  labs(
    title = "Aqua and Terra Fire Counts Over Time",
    x = "Days Relative to Ban End",
    y = "Fire Count"
  ) +
  scale_color_manual(values = c("Aqua Fire Count" = "blue", "Terra Fire Count" = "red")) +
  theme_minimal()

fires_filtered_region <- fires %>% 
  filter(days_to_closest_ban_start > -30, days_to_closest_ban_start < 30) %>% 
  group_by(days_to_closest_ban_start, Region) %>% 
  summarise(
    fire_count = mean(fire_count, na.rm = T),
    aqua_fire_count = mean(aqua_fire_count, na.rm = T),
    terra_fire_count = mean(terra_fire_count, na.rm = T)
  ) %>% 
  mutate(
    first_treat = 0
  )

sa20 = feols(
  fire_count 
  #terra_fire_count
  #aqua_fire_count
  ~ sunab(first_treat, days_to_closest_ban_start) | Region, 
  data = fires_filtered_region, vcov = ~Region
)

sa20 |>
  iplot(
    main     = "fixest::sunab",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1
  )

