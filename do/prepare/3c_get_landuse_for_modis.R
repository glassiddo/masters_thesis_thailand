source("code/setup.R")
# match fires to land use

# Firms_SEA_LandUse_Share.csv is downloaded from gee
# need to get the code
modis_landuse <- fread(here(build.dir, "fires", "Firms_SEA_LandUse_Share.csv")) %>%
  mutate(
    combined = tree_share + crop_share,
    crop50 = ifelse(crop_share > 50, 1, 0),
    forest50 = ifelse(tree_share > 50, 1, 0),
  )

fwrite(modis_landuse, here(build.dir, "modis_with_landuse.csv"))
