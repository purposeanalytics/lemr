# Geocode apartment building registry data
# For two purposes:
# 1. to get lat/lon points
# 2. to get standardized address to use as a key with other datasets

library(dplyr)
library(progress)
library(purrr)
library(tidyr)
library(fs)
library(usethis)
devtools::load_all() # Load package itself to get geocode_address and read_latest_file

# Read in latest extract
apartment_building_registry <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "extract"), suffix = "-apartment_building_registry.csv")

# If geocoded files exist, read in the latest one, and only geocode new addresses
geocoded_files <- dir_ls(here::here("data-raw", "apartment_building_registry", "geocode_raw"))

if (length(geocoded_files) > 0) {
  apartment_building_registry_already_geocoded <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "geocode_raw"), suffix = "-apartment_building_registry_geocoded.rds", fileext = "rds")

  apartment_building_registry_not_geocoded <- apartment_building_registry %>%
    anti_join(apartment_building_registry_already_geocoded, by = "_id")

  if (nrow(apartment_building_registry_not_geocoded) == 0) {
    ui_done("All addresses done - no geocoding required! No need to run through the rest of the script or others for this data set.")
  } else {
    ui_todo("Some addresses need to be geocoded! Continue through the script and next steps.")
    apartment_building_registry_for_geocoding <- apartment_building_registry_not_geocoded
  }
} else { # If there is no existing dataset, the whole thing needs to be geocoded
  ui_todo("All addresses need to be geocoded! Continue through the script and next steps.")
  apartment_building_registry_for_geocoding <- apartment_building_registry
}

# Test function for geocoding a single address
geocode_address("235 Bloor St E Toronto, ON M4W")

# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(apartment_building_registry_for_geocoding))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

apartment_building_registry_geocoded <- apartment_building_registry_for_geocoding %>%
  mutate(
    # Combine address, city (Toronto, ON), and FSA for better results when geocoding
    address_for_geocoding = glue::glue("{SITE_ADDRESS} Toronto, ON {ifelse(is.na(PCODE), '', PCODE)}"),
    address_geocode = map(address_for_geocoding, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

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

# Sometimes the call is "successful" but nothing actually comes through
# For ones that are missing, requery - they mostly come up again!

geocode_missing <- apartment_building_registry_geocoded %>%
  select(`_id`, address_for_geocoding, PCODE, starts_with("bing")) %>%
  filter(is.na(bing_latitude) | is.na(bing_longitude) | is.na(bing_postal_code))

geocode_missing_filled <- geocode_missing %>%
  select(`_id`, address_for_geocoding) %>%
  mutate(address_geocode = map(address_for_geocoding, function(x) {
    safely_geocode_address(x)
  })) %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-address_geocode_error)

geocode_missing_filled <- geocode_missing_filled %>%
  filter(!is.na(bing_postal_code))

# Update the missing ones with these
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  rows_update(geocode_missing_filled, by = c("_id", "address_for_geocoding"))

# If there were some already geocoded, just append these news ones to that!
if (length(geocoded_files) > 0) {
  apartment_building_registry_geocoded <- apartment_building_registry_already_geocoded %>%
    bind_rows(apartment_building_registry_geocoded)
}

# Save results ----
saveRDS(apartment_building_registry_geocoded, here::here("data-raw", "apartment_building_registry", "geocode_raw", glue::glue("{Sys.Date()}-apartment_building_registry_geocoded.rds")))
