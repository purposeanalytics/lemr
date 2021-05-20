library(opendatatoronto)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)
library(sf)
library(purrr)
library(uset)

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

# Bing API documentation: https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/find-a-location-by-address
# Bing license requirements: http://mapsforenterprise.binginternal.com/en-us/maps/product
# limit: 5 calls per second, 50K per day for non-profit uses

# Set components of URL of format: base + address_string + ?key + token
base <- "http://dev.virtualearth.net/REST/v1/Locations/CA/"
token <- Sys.getenv("BING_TOKEN")

# Function for geocoding a single address
geocode_address <- function(address, quiet = FALSE) {
  clean_address <- address %>%
    str_squish() %>% # Remove excess whitespace
    str_replace_all("[^a-zA-Z0-9]", "%20") # Replace any spaces with %20, required for URLs
  call <- glue::glue("{base}{clean_address}Toronto%20ON?key={token}") # Full call URL

  # Get geocoding
  geocode_result <- GET(call)

  # Status code
  status_code <- geocode_result[["status_code"]]

  # Display progress if quiet = FALSE
  if (!quiet) {
    cat(ui_info("Fetching {address} - Status: {status_code}"))
  }

  # If successful (status code 200), extract address

  if (status_code == 200) {
    geocode_json_tidied <- content(geocode_result, "text") %>%
      fromJSON(flatten = TRUE) %>%
      pluck("resourceSets") %>%
      # Extract the element named "resourceSets"
      pull(resources) %>%
      # Column named "resources"
      pluck(1) # First element

    # Address
    address <- geocode_json_tidied[["address.addressLine"]]

    # Municipality
    municipality <- geocode_json_tidied[["address.adminDistrict2"]]

    # Postal code
    postal_code <- geocode_json_tidied[["address.postalCode"]]

    # Information on geocoding
    geocode_points_tidied <- geocode_json_tidied %>%
      pull(geocodePoints) %>%
      pluck(1) %>%
      slice(1)

    # Method of determining geocoding
    method <- geocode_points_tidied[["calculationMethod"]]

    # Confidence of geocoding
    confidence <- geocode_json_tidied[["confidence"]]

    # Latitude and longitude
    latitude_longitude <- geocode_points_tidied %>%
      pull(coordinates) %>% # Pull coordinates
      pluck(1) # In a list, so first element

    latitude <- latitude_longitude[[1]]
    longitude <- latitude_longitude[[2]]
  }

  res <- list(
    status_code = status_code,
    address = address,
    municipality = municipality,
    postal_code = postal_code,
    method = method,
    confidence = confidence,
    latitude = latitude,
    longitude = longitude
  )

  # Replace any NULLs with NAs, then turn into a tibble
  res <- purrr::map(res, function(x) {
    if (is.null(x)) {
      NA
    } else {
      x
    }
  }) %>%
    as_tibble()

  names(res) <- glue::glue("bing_{names(res)}")

  res
}

# Iterate through addresses
apartment_building_registry_geocoded <- apartment_building_registry %>%
  mutate(address_geocode = map(SITE_ADDRESS, function(x) {
    Sys.sleep(0.25) # Wait so that we abide by license  (5 calls per second)
    geocode_address(x)
  }))

# Unnest results
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  unnest(cols = address_geocode)

# Save
saveRDS(apartment_building_registry_geocoded, here::here("data-raw", "apartment_building_registry", glue::glue("{Sys.Date()}-apartment_building_registry_geocoded.rds")))

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
