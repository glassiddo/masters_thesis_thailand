grid <- fread("grid_1606.csv")

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai",
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

### first plot - fires over time -----
g1 <- grid %>% 
  mutate(
    region_type = case_when(
      ADM1_EN %in% northern_regions_en ~ "Northern Thailand",
      ADM0_EN == "Thailand" ~ "Rest of Thailand",
      TRUE ~ ADM0_EN
    )
  ) %>% 
  group_by(year, month, region_type) %>% 
  summarise(
    n_fires = mean(total_fires),
    .groups = "drop"
  ) 
  

ggplot(
  data = g1 %>% filter(region_type == "Northern Thailand"), 
  aes(x = month, y = factor(year), fill = n_fires)
  ) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "F",
    direction = -1,
    name = "Average number of fires (MODIS)",
    trans = "log1p",
    breaks = c(0, 5, 10, 50, 100),  # Manual breaks from second version
    labels = c("0", "5", "10", "50", "100")  # Clean labels
  ) +
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,
    position = "bottom"  
  ) +
  scale_y_discrete(
    breaks = function(x) x[seq(1, length(x), by = 2)]  # Every 2nd year
  ) +
  #facet_wrap(~region_type) +#, scales = "free") +
  labs(
    #title = "Fires in Southeast Asia peak during the haze season (Feb-April)"
    #caption = "*Northern Thailand is composed of the nine provinces that imposed a burning ban from 2013"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
      #element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_blank(),
      #element_text(size = 10, hjust = 0),
    axis.text.x = element_text(color = "black", size = 15, face = "bold"),  
    axis.text.y = element_text(color = "black", size = 15, face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face= "bold"),
    legend.key.width = unit(3, "cm"),  # Wider legend from second version
    strip.text = element_text(size = 14, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")  # Extra spacing around plot
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "bottom",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black"
    )
  )


######## fires gap -----
modis_fires <- st_read("modis_with_region_2206.gpkg")

treated_units <- grid %>%
  filter(ADM1_EN %in% northern_regions_en) %>%
  pull(adm1_id) %>%
  unique()

# Convert ACQ_TIME to Thailand local time (UTC+7)

f_with_shares <- modis_fires %>% 
  st_drop_geometry() %>% 
  mutate(
    period = case_when(
      year < 2013 ~ "2004-12",
      year < 2017 ~ "2013-16",
      year >= 2017 ~ "2017-20",
      TRUE ~ NA
    ),
    region = case_when(
      adm1_id %in% treated_units ~ "North Thailand", 
      adm0_id == 3 ~ "Thailand", 
      adm0_id == 2 ~ "Myanmar", 
      adm0_id == 1 ~ "Laos",
      TRUE ~ NA
    ),
    ACQ_DATETIME_UTC = ymd_hm(paste0(year(date), "-01-01 ", 
                                     str_sub(ACQ_TIME, 1, 2), ":", 
                                     str_sub(ACQ_TIME, 3, 4))),
    ACQ_DATETIME_TH = ACQ_DATETIME_UTC + hours(7),
    ACQ_TIME_TH = format(ACQ_DATETIME_TH, "%H%M"),
    TIME_DECIMAL = as.numeric(substr(ACQ_TIME_TH, 1, 2)) + 
      as.numeric(substr(ACQ_TIME_TH, 3, 4)) / 60
  ) %>% 
  filter(
    #year %in% 2017:2020, 
    region == "North Thailand"
  ) %>% 
  mutate(
    time_midpoint = case_when(
      SATELLITE == "Terra" & TIME_DECIMAL >= 21.5 & TIME_DECIMAL < 23.5 ~ 22.5,
      SATELLITE == "Terra" & TIME_DECIMAL >= 9.5  & TIME_DECIMAL < 11.5 ~ 10.5,
      SATELLITE == "Aqua" & TIME_DECIMAL >= 0.5  & TIME_DECIMAL < 2.5  ~ 1.5,
      SATELLITE == "Aqua" & TIME_DECIMAL >= 12.5 & TIME_DECIMAL < 14.5 ~ 13.5,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(time_midpoint)) %>% 
  group_by(time_midpoint, SATELLITE, period) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Create a combined factor for satellite and period
  mutate(
    sat_period = paste(SATELLITE, period, sep = "_"),
    # Order the periods as desired
    period = factor(period, levels = c("2004-12", "2013-16", "2017-20"))
  ) %>% 
  # group_by(period) %>%
  # summarise(overall_n = sum(n)) %>% 
  # ungroup() %>% 
  group_by(period) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() 

top_bars <- f_with_shares %>%
  mutate(label = case_when(
    SATELLITE == "Terra" & time_midpoint == 10.5 ~ "Terra\n(10:30)",  # \n adds a line break
    SATELLITE == "Terra" & time_midpoint == 22.5 ~ "Terra\n(22:30)",
    SATELLITE == "Aqua" & time_midpoint == 1.5  ~ "Aqua\n(01:30)",
    SATELLITE == "Aqua" ~ "Aqua\n(13:30)"
  )) %>% 
  select(label, time_midpoint, share) %>%  # Include share for y-position
  group_by(label, time_midpoint) %>%
  summarize(y_pos = max(share))  # Get top position for each label

color_palette <- c(
  "2004-12"    = "#120D3A",  # Deep purple (near-black)
  "2013-16"   = "#CB1B4F",  # Vibrant pink-red (mid-rocket)
  "2017-20" = "#FCA50A"   # Dark gold (instead of light yellow)
)

ggplot(f_with_shares) +
  annotate("rect", xmin = 9.5, xmax = 11.5, ymin = 0, ymax = Inf, fill = "lightblue") +
  annotate("rect", xmin = 21.5, xmax = 23.5, ymin = 0, ymax = Inf, fill = "lightblue") +
  annotate("rect", xmin = 12.5, xmax = 14.5, ymin = 0, ymax = Inf, fill = "pink") +
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = Inf, fill = "pink") +
  # evasion zones
  annotate("rect", xmin = 2.5, xmax = 9.5, ymin = 0, ymax = Inf, fill = "grey") +
  annotate("rect", xmin = 14.5, xmax = 21.5, ymin = 0, ymax = Inf, fill = "grey") +
  # evasion text
  annotate("text", x = 6, y = max(f_with_shares$share) * 0.55, 
           label = "Window for\ndetection evasion", size = 5, fontface = "bold") +
  annotate("text", x = 18, y = max(f_with_shares$share) * 0.55, 
           label = "Window for\ndetection evasion", size = 5, fontface = "bold") +
  # Bar chart with pattern and color
  geom_col(
    aes(x = time_midpoint, y = share, fill = period),
    position = position_dodge(width = 1.5), 
    width = 1.5
  ) +
  geom_text(data = top_bars,
            aes(x = time_midpoint, y = 0.9, label = label),
            #vjust = 0.5,  # Less extreme vjust
            fontface = "bold",
            size = 5) +  # Aesthetic tweaks
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(breaks = seq(0, 24, 3), labels = function(x) sprintf("%02d:00", x)) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    labels = function(x) ifelse(x == 0, "", scales::comma(x))
  ) +
  labs(
    title = "Share of fires detected by the Terra satellite increase during the ban"
  ) +
  theme(
    plot.title = element_blank(),
      #element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),  
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title = element_blank(),
    #panel.grid = element_blank(),
    #panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    #legend.title.align = 0.5,
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.key.width = unit(2, "cm")  # Wider legend from second version
  ) +
  guides(
    fill = guide_legend(order = 1),  # no title.hjust or title.position
    pattern = guide_legend(order = 2)
  )
