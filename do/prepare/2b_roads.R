source("do/setup.R")

# read osm country road data
countries <- list(
  thai = "hotosm_tha_roads_lines_shp.shp",
  mya  = "hotosm_mmr_roads_lines_shp.shp",
  lao  = "hotosm_lao_roads_lines_shp.shp"
)

process_roads <- function(roads_file) {
  
  path <- here(raw.dir, "roads", roads_file)
  
  read_sf(path) |> 
    filter(
      highway %in% c(
        # filter to only actual "highways". 
      # info here https://wiki.openstreetmap.org/wiki/WikiProject_Thailand
      "motorway", # Expressway or Controlled-access
      "trunk", # Uncontrolled-access, All 1-2-digit national highways orroad section that is more than 90% dual-carriageway + > 100 km long
      "primary", # All 3-digit national highways, or more than 90% dual-carriageway + >25 km long
      "secondary", #  4-digit national highways,
      "tetiary", # rural roads
      "unclassified" # lowest rank of a public road usable by motor cars
      )
    ) |> 
    filter_out(
      surface %in% c("gravel", "fine_gravel", "unpaved") # exclude those
    ) |>
    mutate(len = as.numeric(st_length(geometry))) 
}

roads_all <- map_dfr(countries, process_roads)

roads_all_asphalt <- roads_all |> 
  filter(
    grepl("asphalt", surface)
  )

st_write(
  roads_all |> select(geometry), 
  here(build.dir, "roads", "main_roads.shp"), 
  append = FALSE
)

st_write(
  roads_all_asphalt |> select(geometry), 
  here(build.dir, "roads", "main_asphalt_roads.shp"), 
  append = FALSE
)
