library(dplyr)
library(opendatatoronto)
library(httr)
library(jsonlite)
library(sf)

# get all resources for this package
extract_apt_registry <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-registration/")  %>%
  filter(name == "Apartment Building Registration Data") %>%
  get_resource()

# geocode addresses for two reasons
# 1. to get lat/lon points
# 2. to get standardized address to use as a key with other datasets

# Bing API documentation: https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/find-a-location-by-address
# Bing license requirements: http://mapsforenterprise.binginternal.com/en-us/maps/product
# limit: 5 calls per second, 50K per day for non-profit uses

# set components of URL of format: base + address_string + ?key + token
base <- "http://dev.virtualearth.net/REST/v1/Locations/CA/"
token <- Sys.getenv("BING_TOKEN")

# create empty fields for geocode process
geocode_apt_registry <- extract_apt_registry %>%
  mutate(bing_status_code  = NA_character_,
         bing_address      = NA_character_,
         bing_municipality = NA_character_,
         bing_postal_code  = NA_character_,
         bing_method       = NA_character_,
         bing_confidence   = NA_character_,
         bing_latitude     = NA_real_,
         bing_longitude    = NA_real_)

# construct call for each address to Bing geocoder endpoint and decode from json
for(i in 1:nrow(geocode_apt_registry)) {

  address_string <- str_replace_all(geocode_apt_registry$SITE_ADDRESS[i], "[^a-zA-Z0-9]", "%20")
  call <- paste0(base, address_string, "Toronto%20ON", "?key=", token)

  geocode_result <- GET(call)
  geocode_apt_registry$bing_status_code[i] <- geocode_result$status_code

  if (geocode_result$status_code == 200) {

    geocode_json <- content(geocode_result, "text") %>%
      fromJSON(get_address_text, flatten = TRUE)

    geocode_apt_registry$bing_address[i]           <- geocode_json$resourceSets[[1]]$resources[[1]]$address$addressLine

    if(!is.null(geocode_json$resourceSets[[1]]$resources[[1]]$address$adminDistrict2)) {
      geocode_apt_registry$bing_municipality[i]    <- geocode_json$resourceSets[[1]]$resources[[1]]$address$adminDistrict2
    }

    if(!is.null(geocode_json$resourceSets[[1]]$resources[[1]]$address$postalCode)) {
      geocode_apt_registry$bing_postal_code[i]     <- geocode_json$resourceSets[[1]]$resources[[1]]$address$postalCode
    }

    geocode_apt_registry$bing_method[i]            <- geocode_json$resourceSets[[1]]$resources[[1]]$geocodePoints[[1]]$calculationMethod
    geocode_apt_registry$bing_confidence[i]        <- geocode_json$resourceSets[[1]]$resources[[1]]$confidence
    geocode_apt_registry$bing_latitude[i]          <- geocode_json$resourceSets[[1]]$resources[[1]]$geocodePoints[[1]]$coordinates[[1]]
    geocode_apt_registry$bing_longitude[i]         <- geocode_json$resourceSets[[1]]$resources[[1]]$geocodePoints[[1]]$coordinates[[2]]

  }

  # display progress
  print(paste("Fetching", i, "-", geocode_apt_registry$SITE_ADDRESS[i], "- Status:", geocode_result$status_code))

  # pause to prevent exceeding 5 calls per second (basic license limitation)
  Sys.sleep(0.25)

}

# corrections from manual inspection of missing geocoded entries
corrections <- tribble(
  ~SITE_ADDRESS,               ~manual_address,        ~manual_latitude,     ~manual_longitude,
  "2877 A  ELLESMERE RD",      "2877 Ellesmere Rd",     43.780633,            -79.20349,
  "2  KINGSTON RD",            "2 Kingston Rd",         43.774061,            -79.183909,
  "10  VENA WAY",              "10 Vena Way",           43.75049883092916,    -79.5416203305828,
  "6  VENA WAY",               "6 Vena Way",            43.75040688113031,    -79.54156401524023,
  "2  VENA WAY",               "2 Vena Way",            43.7493214854571,     -79.54115877106196,
  "360  BLOOR ST W",           "360 Bloor St W",        43.666789,            -79.405123,
  "21  MAYFAIR AVE",           "21 Mayfair Ave",        43.703712,            -79.422213,
  "127  ISABELLA ST",          "127 Isabella St",       43.669023,            -79.377737,
  "74  HUBBARD BLVD",          "74 Hubbard Blvd",       43.669107,            -79.291128
)

# replace with corrections
corrected_apt_registry <- geocode_apt_registry %>%
  left_join(corrections, by = "SITE_ADDRESS") %>%
  mutate(bing_address   = if_else(!is.na(manual_address), manual_address, bing_address),
         bing_latitude  = if_else(!is.na(manual_latitude), manual_latitude, bing_latitude),
         bing_longitude = if_else(!is.na(manual_longitude), manual_longitude, bing_longitude)) %>%
  select(-manual_address, -manual_latitude, -manual_longitude)

# convert to sf
apt_registry_sf <- corrected_apt_registry %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)
