# Geocode eviction hearings addresses

library(dplyr)
library(stringr)
library(purrr)
library(progress)
library(tidyr)
devtools::load_all()

evictions <- readRDS(here::here("data-raw", "apartments", "evictions", "extract", "evictions.rds"))

# Clean up addresses - separate postal code, add Ontario, convert to title case
evictions <- evictions %>%
  mutate(
    postal_code = str_extract(address, "M[0-9][A-Z][0-9][A-Z][0-9]"),
    address = ifelse(!is.na(postal_code), str_remove(address, postal_code), address),
    address = str_trim(address),
    address = str_to_title(address),
    address = paste(address, "Ontario"),
    address_for_geocoding = paste(address, postal_code)
  )

# Geocode ----
# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(evictions))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

evictions_geocoded <- evictions %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

# Separate results from errors
evictions_geocoded <- evictions_geocoded %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
evictions_geocoded <- evictions_geocoded %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

# Get errors - either 404 or low confidence, and re-geocode
evictions_geocoded_redone <- evictions_geocoded %>%
  filter(bing_status_code == 404 | bing_confidence == "Low") %>%
  select(-starts_with("bing"), -address_geocode_error) %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      safely_geocode_address(x)
    }),
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

evictions_geocoded_redone_correct <- evictions_geocoded_redone %>%
  filter(!(bing_status_code == 404 | bing_confidence == "Low"))

evictions_geocoded_redone_incorrect <- evictions_geocoded_redone %>%
  filter(bing_status_code == 404 | bing_confidence == "Low") %>%
  mutate(corrected = FALSE)

# Manual corrections

# Some just have the address wrong and that can be fixed and then re-geocoded
address_corrections <- tribble(
  ~address, ~address_for_geocoding,
  "1172 Pleasant View Drive Toronto Ontario", "172 Pleasant View Drive Toronto Ontario M2J3R5",
  "185 1/2 Beverley Street Toronto Ontario", "185 Beverley Street Toronto Ontario M5T1Y9",
  "558 Waterton Road Toronto Ontario", "58 Waterton Road Toronto Ontario M9P2R1",
  "106 Lakeshore Drive Toronto Ontario", "106 Lake Shore Dr Etobicoke, ON M8V 2A2",
  "30 Tuxedo Court Scarborough Ontario", "30 Tuxedo Court Scarborough Ontario",
  "5162 Young Street Toronto Ontario", "5162 Yonge Street Toronto Ontario M2N0E9"
)

address_corrections <- address_corrections %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      safely_geocode_address(x)
    }),
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode")) %>%
  mutate(corrected = TRUE)

evictions_geocoded_redone_incorrect <- evictions_geocoded_redone_incorrect %>%
  rows_update(address_corrections, by = "address")

# Some need manual lat / long

lat_long_corrections <- tribble(
  ~address, ~bing_latitude, ~bing_longitude,
  "2549 Lakeshore Boulevard W Toronto Ontario", 43.609325237047486, -79.48855866084631,
  "82 Ypres Road York Ontario", 43.68799794615186, -79.47251746769774,
  "10 Vena Way Toronto Ontario", 43.750306712965084, -79.54161503593076,
  "15 Eaton Park Lane Scarborough Ontario", 43.7943945582108, -79.31059617323517,
  "2559 Lakeshore Boulevard W Toronto Ontario", 43.60905422030891, -79.489613181866,
  "6 Vena Way Toronto Ontario", 43.75022337410752, -79.54159714567894,
  "2 Vena Way Toronto Ontario", 43.749107626057466, -79.54124877886046,
  "Hertford Hertford Avenue W Toronto Ontario", 43.68872295218799, -79.47692736064509,
  "106 Lakeshore Drive Toronto Ontario", 43.595109238893485, -79.50397790103436
) %>%
  mutate(corrected = TRUE)

evictions_geocoded_redone_incorrect <- evictions_geocoded_redone_incorrect %>%
  rows_update(lat_long_corrections, by = "address")

# Update all

evictions_geocoded <- evictions_geocoded %>%
  rows_update(evictions_geocoded_redone_correct, by = "address") %>%
  rows_update(evictions_geocoded_redone_incorrect %>%
    select(-corrected), by = "address")

saveRDS(evictions_geocoded, here::here("data-raw", "apartments", "evictions", "geocode", "evictions.rds"))
