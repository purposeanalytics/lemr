# Clean results from geocoding

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(usethis)
devtools::load_all() # Load package itself to get read_latest_file

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartments", "apartment_building_registry", "geocode_raw"), suffix = "-apartment_building_registry_geocoded.rds", fileext = "rds")

apartment_building_registry <- read_latest_file(directory = here::here("data-raw", "apartments", "apartment_building_registry", "extract"), suffix = "-apartment_building_registry.csv", fileext = "csv")

# Check if any records were duplicated
no_duplicated_records <- nrow(apartment_building_registry) == nrow(apartment_building_registry_geocoded)

if (no_duplicated_records) {
  ui_done("No records duplicated!")
} else {
  ui_todo("Uh oh, the datasets are not 1 to 1! There was some duplication in the geocoding.")
}

# Check any that had errors
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(address_geocode_error = map_lgl(address_geocode_error, ~ !is.null(.x)))

no_geocoding_errors <- apartment_building_registry_geocoded %>%
  filter(address_geocode_error) %>%
  nrow() == 0

if (no_geocoding_errors) {
  ui_done("No errors in running the geocoding function in the first step!")
} else {
  ui_todo("Uh oh, there were some geocoding errors! Take a look at these records:")
  apartment_building_registry_geocoded %>%
    filter(address_geocode_error)
}

# Check that we have all fields for every record
apartment_building_registry_geocoded %>%
  select(starts_with("bing")) %>%
  visdat::vis_miss()

# Records with missing postal code / lat/long
issues_missing <- apartment_building_registry_geocoded %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  filter(is.na(bing_latitude) | is.na(bing_longitude) | is.na(bing_postal_code)) %>%
  mutate(issue = "missing")

# Any with "Low" confidence from the API
issues_low_confidence <- apartment_building_registry_geocoded %>%
  filter(bing_confidence == "Low") %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  mutate(issue = "low confidence")

# Any where PCODE (first three digits of postal code, from the extract) doesn't match the postal code from the API
# Some records are just missing PCODE, so don't count that as an error
issues_pcode_mismatch <- apartment_building_registry_geocoded %>%
  filter(!str_starts(bing_postal_code, PCODE) & !is.na(PCODE)) %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  mutate(issue = "pcode mismatch")

# Combine issues to handle
geocode_issues <- issues_missing %>%
  bind_rows(issues_low_confidence) %>%
  bind_rows(issues_pcode_mismatch) %>%
  group_by(`_id`, SITE_ADDRESS) %>%
  mutate(issue = stringr::str_c(issue, collapse = ", ")) %>%
  ungroup() %>%
  distinct()

geocode_issues %>%
  count(issue)

# Fixing issues ----
corrections <- tribble(
  ~`_id`, ~SITE_ADDRESS, ~bing_address, ~bing_latitude, ~bing_longitude, ~bing_postal_code,
  72396, "10  VENA WAY", "10 Vena Way", 43.75049883092916, -79.5416203305828, "M9M 0G3",
  71088, "6  VENA WAY", "6 Vena Way", 43.75040688113031, -79.54156401524023, "M9M 2X3",
  69628, "2  VENA WAY", "2 Vena Way", 43.74910835423819, -79.54121241541944, "M9M 0G2",
  70884, "245 A  HOWLAND AVE", "245 A Howland Ave", 43.67287105917314, -79.41118264425805, "M5R 3B7"
)

# Check we got them all
geocode_issues %>%
  anti_join(corrections, by = "SITE_ADDRESS")
# 15  HARDING AVE  is actually right - it's on the border of M9N but postal code is in fact M6M 0A4
# If there are any others - then we should look into and correct those!

# Replace with corrections ----

corrections_data <- apartment_building_registry_geocoded %>%
  select(`_id`, SITE_ADDRESS) %>%
  inner_join(corrections, by = c("_id", "SITE_ADDRESS"))

apartment_building_registry_geocode_with_corrections <- apartment_building_registry_geocoded %>%
  rows_update(corrections_data, by = c("_id", "SITE_ADDRESS"))

# Write data
saveRDS(apartment_building_registry_geocode_with_corrections, here::here("data-raw", "apartments", "apartment_building_registry", "geocode_clean", glue::glue("{Sys.Date()}-apartment_building_registry_geocoded_clean.rds")))
