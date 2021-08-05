# Final cleaning of apartment building registry, convert to spatial

library(sf)
library(janitor)
library(stringr)
library(dplyr)
devtools::load_all()

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartments", "apartment_building_registry", "geocode_clean"), suffix = "-apartment_building_registry_geocoded_clean.rds", fileext = "rds")

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

# Keep relevant columns

apartment_building_registry <- apartment_building_registry %>%
  select(id, rsn, site_address, bing_address, neighbourhood, confirmed_units, confirmed_storeys, year_built, geometry)

# Check values
apartment_building_registry %>%
  filter(is.na(year_built))

# Can't really find data on year built, so will have to go with "Unknown"

apartment_building_registry %>%
  filter(confirmed_units < 10 | is.na(confirmed_units))

apartment_building_registry %>%
  filter(confirmed_storeys < 3 | is.na(confirmed_storeys))

# Flag as NA

apartment_building_registry <- apartment_building_registry %>%
  mutate(across(
    c(confirmed_units, confirmed_storeys),
    function(x) {
      case_when(
        cur_data()[["rsn"]] %in% c(4820405, 4904686) ~ NA_real_,
        TRUE ~ x
      )
    }
  ))

# Save as data set in package
usethis::use_data(apartment_building_registry, overwrite = TRUE)
