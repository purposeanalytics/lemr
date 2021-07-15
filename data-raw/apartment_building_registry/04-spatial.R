# Final cleaning of apartment building registry, convert to spatial

library(sf)
library(janitor)
library(stringr)
library(dplyr)
devtools::load_all()

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "geocode_clean"), suffix = "-apartment_building_registry_geocoded_clean.rds", fileext = "rds")

# Move address fields to start, clean up column names
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  clean_names() %>%
  select(id, site_address, starts_with("bing"), everything()) %>%
  select(-bing_status_code, -bing_method, -bing_confidence, -address_geocode_error, -address_for_geocoding)

# Clean up original address
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(
    site_address = str_squish(site_address),
    site_address = str_to_title(site_address)
  )

# Convert to SF
apartment_building_registry_sf <- apartment_building_registry_geocoded %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326, remove = FALSE)

# Get neighbourhood for each building
apartment_with_neighbourhood <- apartment_building_registry_sf %>%
  st_intersection(neighbourhoods) %>%
  as_tibble() %>%
  select(id, neighbourhood)

apartment_building_registry <- apartment_building_registry_sf %>%
  left_join(apartment_with_neighbourhood, by = "id") %>%
  select(id, site_address, starts_with("bing"), neighbourhood, everything())

# Save as data set in package
usethis::use_data(apartment_building_registry, overwrite = TRUE)
