ggplot() + 
  geom_sf(data = admin_units) + 
  geom_sf(data = modis %>% sample_n(2000), aes(color = avg_elevation)) 

modis_nogeom <- modis %>% 
  st_set_geometry(NULL) %>% 
  group_by(adm3_id, ACQ_DATE) %>% 
  summarise(
    n_fires_modis = n()
  )

provinces <- unique(adm1$name) # latitude > 15, latitude < 26, longitude < 103

grid <- expand.grid(
  date = dates,
  adm3_id = unique(admin_units$adm3_id),
  stringsAsFactors = FALSE
  ) %>%
  mutate(year = year(date)) %>% 
  full_join(admin_units %>% st_set_geometry(NULL), by = "adm3_id") %>% 
  filter(
   #ADM1_EN %in% northern_regions_en # only those with bans
   ADM1_EN %in% provinces # all above 12N
  ) %>%
  left_join(modis_nogeom %>% rename(date = ACQ_DATE), by = c("adm3_id", "date")) %>%
  mutate(
    n_fires_modis = replace_na(n_fires_modis, 0)
  ) %>%
  mutate(
    across(
      where(is.numeric), #& !c(id),
      ~replace_na(., 0)
    )
  ) %>% 
  left_join(bans %>% rename(ADM1_EN = name), 
            by = c("ADM1_EN", "year")) %>% 
  mutate(
    days_from_ban_start = as.numeric(date - Ban_start),    
    days_from_ban_end = as.numeric(date - Ban_end),
    is_ban = case_when(
      is.na(Ban_start) ~ NA,
      (!is.na(Ban_start) & !is.na(Ban_end) &
         year == lubridate::year(Ban_start) &
         date >= Ban_start & date <= Ban_end
      ) ~ 1, 
      TRUE ~ 0
    ),
    is_ban_over = ifelse(is_ban == 0 & days_from_ban_end > 0, 
                         1, 0) # for event study of ban being over
  )

#write_csv(grid, "temp_grid_1904.csv")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

grid <- fread("temp_grid_1904.csv") #%>% filter(ADM1_EN %in% northern_regions_en)

wsh <- grid %>% 
  dplyr::group_by(ADM1_EN, date, year) %>% 
  #filter(year > 2016) %>% 
  summarise(
    n_fires_modis = sum(n_fires_modis),
    #n_fires_predicted = sum(predicted_fires, na.rm = T),
    Ban_start = mean(Ban_start),
    days_from_ban_start = mean(days_from_ban_start),
    days_from_ban_end = mean(days_from_ban_end),
    is_ban = mean(is_ban),
    is_ban_over = mean(is_ban_over)
    )  
  mutate(
    #is_ban = replace_na(is_ban, 0)
    #diff = n_fires_modis - n_fires_predicted
  ) 

ggplot() + 
  geom_point(data = wsh %>% filter(n_fires_modis < 150), aes(x = n_fires_modis, y = n_fires_predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual Fires (MODIS)", y = "Predicted Fires")


summary(DIDmultiplegtDYN::did_multiplegt_dyn(
  df = wsh,
  outcome = "diff",
  group = "ADM1_EN",
  time = "date",
  treatment = "is_ban_over",
  effects = 15,
  placebo = 20,
  cluster = "ADM1_EN",
  normalized = FALSE,
  same_switchers = FALSE,
  effects_equal = FALSE
))


library(PanelMatch)

dem.panel <- PanelData(panel.data = grid,
                       unit.id = "adm3_id",
                       time.id = "date_int",
                       treatment = "is_ban",
                       outcome = "n_fires_modis")
DisplayTreatment(panel.data = dem.panel, legend.position = "none",
                 xlab = "date", ylab = "Country Code")

PM.results <- PanelMatch(panel.data = dem.panel, lag = 10, 
                         refinement.method = "ps.match", 
                         match.missing = TRUE, 
                         covs.formula = ~ I(lag(is_ban, 1:10)),
                         size.match = 5, qoi = "att",
                         lead = 0:10, 
                         forbid.treatment.reversal = FALSE)
PM.results
