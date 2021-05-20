library(opendatatoronto)
library(dplyr)
library(janitor)
library(progress)
library(purrr)
dplyr::load_all() # Load package itself to get geocode_address

# Get Apartment Building Registration Resource ----
# Extracted May 19, 2021
apartment_building_registry <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-registration/") %>%
  filter(name == "Apartment Building Registration Data") %>%
  get_resource()

# Save resource with date extracted
write_csv(apartment_building_registry, here::here("data-raw", "apartment_building_registry", glue::glue("{Sys.Date()}-apartment_building_registry.csv")))

# Geocode addressess ---
# For two purposes:
# 1. to get lat/lon points
# 2. to get standardized address to use as a key with other datasets

# Test function for geocoding a single address
geocode_address("235 Bloor St E")

# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(apartment_building_registry))
apartment_building_registry_geocoded <- apartment_building_registry %>%
  mutate(address_geocode = map(SITE_ADDRESS, function(x) {
    pb$tick()
    geocode_address(x, quiet = TRUE)
  }))

# Unnest results
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  unnest(cols = address_geocode)

# Save results ----
saveRDS(apartment_building_registry_geocoded, here::here("data-raw", "apartment_building_registry", glue::glue("{Sys.Date()}-apartment_building_registry_geocoded.rds")))

# Check if any addresses were duplicated
nrow(apartment_building_registry) == nrow(apartment_building_registry_geocoded)

apartment_building_registry_geocoded %>%
  get_dupes(SITE_ADDRESS) %>%
  select(SITE_ADDRESS, starts_with("bing")) %>%
  distinct()

# TODO - ask Daniel

# Not done here onwards ----


# corrections from manual inspection of missing geocoded entries
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

# replace with corrections
corrected_apt_registry <- geocode_apt_registry %>%
  left_join(corrections, by = "SITE_ADDRESS") %>%
  mutate(
    bing_address = if_else(!is.na(manual_address), manual_address, bing_address),
    bing_latitude = if_else(!is.na(manual_latitude), manual_latitude, bing_latitude),
    bing_longitude = if_else(!is.na(manual_longitude), manual_longitude, bing_longitude)
  ) %>%
  select(-manual_address, -manual_latitude, -manual_longitude)

# convert to sf
apt_registry_sf <- corrected_apt_registry %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)
