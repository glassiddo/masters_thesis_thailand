hansen <- read_sf("with land use from hansen 2703/THAI_modis_landuse_from_hansen.shp") 
hansen_nogeom <- hansen %>% st_set_geometry(NULL) 
rm(hansen)

modis <- modis %>% 
  #filter(name %in% northern_regions_en) %>% 
  left_join(hansen_nogeom, by = "fire_id")

rm(hansen_nogeom)

modis_nogeom <- modis %>% 
  st_set_geometry(NULL) %>% 
  left_join(hansen_nogeom, by = "fire_id") %>% 
  mutate(
    treecrops_share = crop_share + tree_share,
    other_share = 100 - treecrops_share,
    main_landuse = case_when(
      crop_share > 0.9 ~ "crop",
      tree_share > 0.9 ~ "tree",
      other_share > 0.9 ~ "other",
      TRUE ~ NA
    )
  ) %>% 
  group_by(ACQ_DATE, admin, name, main_landuse, DAYNIGHT) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = c(main_landuse, DAYNIGHT),
    values_from = count,
    names_sep = "_",
    values_fill = 0
  )

modis_north <- modis %>% 
  filter(name %in% c("Chiang Mai", "Lamphun")) %>% 
  st_set_geometry(NULL) %>% 
  select(fire_id, name)

hansen_north <- hansen %>% 
  left_join(modis_north, by = "fire_id") %>% 
  filter(!is.na(name)) %>% 
  mutate(
    treecrops_share = crop_share + tree_share,
    other_share = 100 - treecrops_share,
    main_landuse = case_when(
      crop_share > 0.5 ~ "crop",
      tree_share > 0.5 ~ "tree",
      other_share > 0.5 ~ "other",
      TRUE ~ NA
    )
  )
  

ggplot() + 
  geom_sf(data = hansen_north, aes(color = main_landuse)) 

