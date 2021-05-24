# Geocode apartment building registry data
# For two purposes:
# 1. to get lat/lon points
# 2. to get standardized address to use as a key with other datasets

library(dplyr)
library(progress)
library(purrr)
library(tidyr)
devtools::load_all() # Load package itself to get geocode_address and read_latest_file

# Read in latest extract
apartment_building_registry <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "extract"), suffix = "-apartment_building_registry.csv")

# Test function for geocoding a single address
geocode_address("235 Bloor St E")

# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(apartment_building_registry))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

apartment_building_registry_geocoded <- apartment_building_registry %>%
  mutate(address_geocode = map(SITE_ADDRESS, function(x) {
    pb$tick()
    safely_geocode_address(x)
  }))

# Separate results from errors
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  unnest(cols = c(address_geocode)) %>%
  select(-address_geocode)

# Save results ----
saveRDS(apartment_building_registry_geocoded, here::here("data-raw", "apartment_building_registry", "geocode_raw", glue::glue("{Sys.Date()}-apartment_building_registry_geocoded.rds")))
