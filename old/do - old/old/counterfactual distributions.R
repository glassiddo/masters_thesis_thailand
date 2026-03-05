g <- expand.grid(
  date = dates,
  id = unique(adm1$id),
  stringsAsFactors = FALSE
  ) %>%
  mutate(year = year(date), day = day(date), month = month(date)) %>% 
  full_join(adm1 %>% st_set_geometry(NULL), by = "id") %>% 
  left_join(modis_nogeom %>% rename(date = ACQ_DATE), by = c("admin", "name", "date"))%>% 
  filter(name %in% northern_regions_en) %>% 
  mutate(n_fires_modis = replace_na(n_fires_modis, 0))

g_preban <- g %>% 
  filter(year < 2014) %>% 
  group_by(name, day, month) %>% 
  summarise(fires_pre_ban = mean(n_fires_modis), .groups = "drop")

g_postban <- g %>% 
  filter(year >= 2014) %>% 
  left_join(g_preban, by = c("name", "day", "month")) %>% 
  left_join(bans, by = c("year", "name"))
  
g_postban <- g_postban %>%
  group_by(name, year) %>%
  mutate(
    total_observed = sum(n_fires_modis, na.rm = TRUE),
    total_counterfactual = sum(fires_pre_ban, na.rm = TRUE),
    scaling_factor = ifelse(total_counterfactual == 0, 1, total_observed / total_counterfactual),
    scaled_counterfactual = fires_pre_ban * scaling_factor
  ) %>%
  ungroup()

ggplot(g_postban %>% filter(year == 2016)) + 
  geom_line(aes(x = date, y = n_fires_modis, color = "Actual Fires"), alpha = 0.5) +  # Original line (optional)
  geom_line(aes(x = date, y = scaled_counterfactual, color = "Scaled Pre-Ban Fires"), alpha = 0.5) +  # Original line (optional)
  geom_smooth(aes(x = date, y = n_fires_modis, color = "Actual Fires"), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 1) +  # Smoothed line for Actual Fires
  geom_smooth(aes(x = date, y = scaled_counterfactual, color = "Scaled Pre-Ban Fires"), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 1) +  # Smoothed line for Scaled Pre-Ban Fires
  facet_wrap(~name, scales = "free_y") +
  geom_vline(aes(xintercept = as.numeric(Ban_start)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(Ban_end)), color = "red", linetype = "dashed") +
  scale_color_manual(values = c("Actual Fires" = "black", "Scaled Pre-Ban Fires" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better visibility
