# Add spatial elements (boundaries, neighbourhoods, etc)

library(dplyr)
library(sf)
library(janitor)
library(lemur)

# Read / clean data and geos ----

proximity_measures_toronto <- readRDS(here::here("data-raw", "proximity_measures", "clean", "proximity_measures_toronto.rds")) %>%
  mutate(dbuid = as.character(dbuid))

dissemination_block_geo <- st_read(here::here("data-raw", "proximity_measures", "raw", "ldb_000b16a_e", "ldb_000b16a_e.shp"))

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

saveRDS(proximity_measures, here::here("data-raw", "proximity_measures", "final", "proximity_measures.rds"))

# The geometry makes the file huge - 6+ MB versus 400kb. Remove for now until we decide what to actually do with this data, especially since we might just map the amenity density.

proximity_measures <- proximity_measures %>%
  as_tibble() %>%
  select(-geometry)

# Save -----

usethis::use_data(proximity_measures, overwrite = TRUE)

# Amenity density only -----

amenity_density <- proximity_measures_toronto %>%
  distinct(dbuid, amenity_dense) %>%
  left_join(dissemination_block_geo_toronto, by = "dbuid") %>%
  select(dbuid, amenity_dense, geometry) %>%
  st_sf()

usethis::use_data(amenity_density, overwrite = TRUE)
