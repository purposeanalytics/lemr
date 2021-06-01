# Final cleaning of apartment building registry, convert to spatial

library(sf)
library(janitor)
library(dplyr)
devtools::load_all()

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "geocode_clean"), suffix = "-apartment_building_registry_geocoded_clean.rds", fileext = "rds")

# Move address fields to start, clean up column names
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  clean_names() %>%
  select(id, starts_with("bing"), everything()) %>%
  select(-bing_status_code, -bing_method, -bing_confidence, -site_address, -address_geocode_error, -address_for_geocoding)

# Convert to SF
apartment_building_registry_sf <- apartment_building_registry_geocoded %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326, remove = FALSE)

# Save as data set in package
apartment_building_registry <- apartment_building_registry_sf

usethis::use_data(apartment_building_registry, overwrite = TRUE)
