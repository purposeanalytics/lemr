# Add spatial elements (boundaries, neighbourhoods, etc)

library(dplyr)
library(sf)
library(janitor)
library(lemur)
library(rmapshaper)
devtools::load_all()

# Read / clean data and geos ----

proximity_measures_toronto <- readRDS(here::here("data-raw", "aggregate_data", "proximity_measures", "clean", "proximity_measures_toronto.rds")) %>%
  mutate(dbuid = as.character(dbuid))

dissemination_block_geo <- st_read(here::here("data-raw", "aggregate_data", "proximity_measures", "raw", "ldb_000b16a_e", "ldb_000b16a_e.shp"))

dissemination_block_geo_toronto <- dissemination_block_geo %>%
  clean_names() %>%
  semi_join(proximity_measures_toronto %>%
    distinct(dbuid), by = "dbuid") %>%
  select(dbuid, ctuid) %>%
  st_transform(crs = 4326)

ct_to_neighbourhood <- st_read(here::here("data-raw", "shared", "Census Geographies to TO Neighbourhoods.gpkg")) %>%
  as_tibble() %>%
  clean_names() %>%
  select(ctuid, neighbourhood = area_name) %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

# Combine ----

proximity_measures <- proximity_measures_toronto %>%
  left_join(dissemination_block_geo_toronto, by = "dbuid") %>%
  left_join(ct_to_neighbourhood, by = "ctuid") %>%
  relocate(geometry, .after = last_col()) %>%
  st_sf()

saveRDS(proximity_measures, here::here("data-raw", "aggregate_data", "proximity_measures", "final", "proximity_measures.rds"))

# The geometry makes the file huge - 6+ MB versus 400kb. Don't save for now until we decide what to actually do with this data, especially since we might just map the amenity density.

# Don't save in the package

# Amenity density only -----

amenity_density <- proximity_measures_toronto %>%
  as_tibble() %>%
  distinct(dbuid, population, amenity_dense) %>%
  left_join(dissemination_block_geo_toronto, by = "dbuid") %>%
  select(dbuid, population, amenity_dense, geometry) %>%
  st_sf()

# Simplify geography
amenity_density <- amenity_density %>%
  ms_simplify(keep = 0.1, keep_shapes = TRUE)

# Add colour and opacity

# Colours
amenity_density_colours <- tibble::tribble(
  ~amenity_dense, ~colour,
  "Low", low_colour,
  "Medium", accent_colour,
  "High", high_colour,
  "Unknown", "white"
)

amenity_density <- amenity_density %>%
  dplyr::left_join(amenity_density_colours, by = "amenity_dense")

# Alphas / opacity
n <- 20

amenity_density_alpha <- amenity_density %>%
  dplyr::as_tibble() %>%
  dplyr::distinct(.data$dbuid, .data$population) %>%
  dplyr::mutate(
    log_population = log(.data$population + 1),
    log_population_group = cut(.data$log_population, breaks = n)
  )

log_population_groups <- amenity_density_alpha %>%
  dplyr::distinct(.data$log_population_group)

alphas <- seq(0.1, 0.8, length.out = nrow(log_population_groups))

log_population_groups <- log_population_groups %>%
  dplyr::mutate(alpha = alphas)

amenity_density_alpha <- amenity_density_alpha %>%
  dplyr::left_join(log_population_groups, by = "log_population_group") %>%
  dplyr::select(.data$dbuid, .data$alpha)

amenity_density <- amenity_density %>%
  dplyr::left_join(amenity_density_alpha, by = "dbuid")

# Save final file, not in package but as geojson for a mapbox layer

st_write(amenity_density, here::here("data-raw", "aggregate_data", "proximity_measures", "final", "amenity_density.geojson"))
