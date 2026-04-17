source("do/setup.R")
# match fires to land use

# Firms_SEA_LandUse_Share.csv is downloaded from gee
# need to get the code from GEE as python version 

# need to do the same for viirs
modis_landuse <- fread(here(build.dir, "fires", "Firms_SEA_LandUse_Share.csv")) |> 
  mutate(
    combined = tree_share + crop_share,
    crop50 = ifelse(crop_share > 50, 1, 0),
    forest50 = ifelse(tree_share > 50, 1, 0),
  )

saveRDS(modis_landuse, here(build.dir, "fires", "modis_with_landuse.rds"))
