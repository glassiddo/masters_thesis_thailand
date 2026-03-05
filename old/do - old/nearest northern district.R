northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

# Read the shapefile and calculate centroids
all_units_adm2 <- read_sf("2. intermediate/units/adm2_units.shp")

# Function to find closest northern region based on centroids
find_closest_northern_regions_centroid <- function(all_units_adm2) {
  
  # Calculate centroids for all regions
  all_centroids <- st_centroid(all_units_adm2)
  
  # Split into northern and non-northern regions
  northern_centroids <- all_centroids %>% 
    filter(ADM1_EN %in% northern_regions_en)
  
  non_northern_centroids <- all_centroids %>% 
    filter(!ADM1_EN %in% northern_regions_en)
  
  # Create a results dataframe
  results <- data.frame(
    adm2_id = integer(),
    closest_northern_region = integer(),
    distance_km = numeric(),
    stringsAsFactors = FALSE
  )
  
  # For each non-northern area, find the closest northern area
  for (i in 1:nrow(non_northern_centroids)) {
    current_centroid <- non_northern_centroids[i, ]
    
    # Calculate distances to all northern centroids
    distances <- st_distance(current_centroid, northern_centroids)
    
    # Find the index of the minimum distance
    min_idx <- which.min(distances)
    
    # Get the corresponding northern region ID
    closest_northern_id <- northern_centroids$adm2_id[min_idx]
    
    # Add to results dataframe
    results <- rbind(results, data.frame(
      adm2_id = current_centroid$adm2_id,
      closest_northern_region = closest_northern_id,
      distance_km = as.numeric(distances[min_idx]) / 1000, # Convert to km
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

# Execute the function
closest_northern_regions <- find_closest_northern_regions_centroid(all_units_adm2)

# View the first few results
head(closest_northern_regions)

# Rename columns if you want adm2_id and adm2_id_nearest format
names(closest_northern_regions) <- c("adm2_id", "adm2_id_nearest", "distance_km")

all_units_adm2 <- all_units_adm2 %>% 
  left_join(closest_northern_regions, by = "adm2_id")
