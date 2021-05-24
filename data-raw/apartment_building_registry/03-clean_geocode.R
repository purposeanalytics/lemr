# Clean results from geocoding

library(dplyr)
library(progress)
library(purrr)
library(tidyr)
library(stringr)
devtools::load_all() # Load package itself to get geocode_address and read_latest_file

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "geocode_raw"), suffix = "-apartment_building_registry_geocoded.rds", fileext = "rds")

# Check if any records were duplicated
nrow(apartment_building_registry) == nrow(apartment_building_registry_geocoded)

# Check any that had errors
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(address_geocode_error = map_lgl(address_geocode_error,  ~ !is.null(.x)))

apartment_building_registry_geocoded %>%
  filter(address_geocode_error) %>%
  nrow() == 0

# Check that we have all fields for every record
apartment_building_registry_geocoded %>%
  select(starts_with("bing")) %>%
  visdat::vis_miss()

issues_missing <- apartment_building_registry_geocoded %>%
  select(SITE_ADDRESS, starts_with("bing")) %>%
  filter(if_any(starts_with("bing"), ~ is.na(.x))) %>%
  mutate(issue = "missing")

# Any with "Low" confidence from the API
issues_low_confidence <- apartment_building_registry_geocoded %>%
  filter(bing_confidence == "Low") %>%
  select(SITE_ADDRESS, starts_with("bing")) %>%
  mutate(issue = "low confidence")

# Any where PCODE (first three digits of postal code, from the extract) doesn't match the postal code from the API
issues_pcode_mismatch <- apartment_building_registry_geocoded %>%
  filter(!str_starts(bing_postal_code, PCODE)) %>%
  select(SITE_ADDRESS, starts_with("bing")) %>%
  mutate(issue = "pcode mismatch")

# Combine issues to handle
geocode_issues <- issues_missing %>%
  bind_rows(issues_low_confidence) %>%
  bind_rows(issues_pcode_mismatch) %>%
  group_by(SITE_ADDRESS) %>%
  mutate(issue = stringr::str_c(issue, collapse = ", ")) %>%
  ungroup() %>%
  distinct()

geocode_issues %>%
  count(issue)

# Not done here onwards ----


# corrections from manual inspection of missing geocoded entries
# From google
corrections <- tribble(
  ~SITE_ADDRESS, ~manual_address, ~manual_latitude, ~manual_longitude,
  "2877 A  ELLESMERE RD", "2877 Ellesmere Rd", 43.780633, -79.20349,
  "2  KINGSTON RD", "2 Kingston Rd", 43.774061, -79.183909,
  "10  VENA WAY", "10 Vena Way", 43.75049883092916, -79.5416203305828,
  "6  VENA WAY", "6 Vena Way", 43.75040688113031, -79.54156401524023,
  "2  VENA WAY", "2 Vena Way", 43.7493214854571, -79.54115877106196,
  "360  BLOOR ST W", "360 Bloor St W", 43.666789, -79.405123,
  "21  MAYFAIR AVE", "21 Mayfair Ave", 43.703712, -79.422213,
  "127  ISABELLA ST", "127 Isabella St", 43.669023, -79.377737,
  "74  HUBBARD BLVD", "74 Hubbard Blvd", 43.669107, -79.291128
)


# TODO: look into dplyr::update

# replace with corrections
corrected_apt_registry <- geocode_apt_registry %>%
  left_join(corrections, by = "SITE_ADDRESS") %>%
  mutate(
    bing_address = coalesce(manual_address, bing_address),
    bing_latitude = if_else(!is.na(manual_latitude), manual_latitude, bing_latitude),
    bing_longitude = if_else(!is.na(manual_longitude), manual_longitude, bing_longitude)
  ) %>%
  select(-manual_address, -manual_latitude, -manual_longitude)

# convert to sf
apt_registry_sf <- corrected_apt_registry %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)
