# Geocode AGI addresses

library(dplyr)
library(purrr)
library(progress)
library(tidyr)
library(stringr)
devtools::load_all()

agi_applications <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "extract", "agi_applications.rds"))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

# Don't geocode duplicate addresses
agi_applications <- agi_applications %>%
  mutate(
    # Combine address, city (Toronto, ON), and FSA for better results when geocoding
    address = glue::glue("{street_number} {street_name}, Toronto, ON {postal_code}"),
    id = row_number()
  )

# Fix one address with NA street number and that's just an intersection
# Found the actual address via https://www.hpta.ca/updates/gwl-realty-case-hearing-tsl-95773-18-update-35-65-amp-95-high-park-ave-and-66-pacific-ave
# Case number match

# And fix a couple that will not geocode properly without some coaxing

agi_applications <- agi_applications %>%
  mutate(
    address = case_when(
      address == "NA High Park & Pacific Avenue, Toronto, ON M6P2R6" ~ "66 Pacific Avenue, Toronto, ON M6P 2P4",
      address == "NA Crestview Apts (leacrest & Mallory), Toronto, ON M4G1E6" ~ "30-75 Leacrest Rd, Toronto, ON M4G 1E6",
      address == "25 San Romano Way, Toronto, ON M3N2Z1" ~ "25 San Romanoway, Toronto, ON M3N2Z1",
      TRUE ~ as.character(address)
    ),
    address_for_geocoding = case_when(
      address == "30 1/2 Macaulay Avenue, Toronto, ON M6P3P6" ~ "30 Macaulay Avenue, Toronto, ON M6P3P6",
      address == "409 1/2 Roncesvalles Avenue, Toronto, ON M6R2N1" ~ "409 Roncesvalles Ave, Toronto, ON M6R 2N1",
      TRUE ~ as.character(address)
    )
  )

agi_applications_addresses <- agi_applications %>%
  distinct(address, address_for_geocoding)

# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(agi_applications_addresses))

agi_applications_addresses_geocoded <- agi_applications_addresses %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

 # Separate results from errors
agi_applications_addresses_geocoded <- agi_applications_addresses_geocoded %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
agi_applications_addresses_geocoded <- agi_applications_addresses_geocoded %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

# Requery failed results - seems to work!
agi_applications_addresses_geocoded_missing <- agi_applications_addresses_geocoded %>%
  filter(bing_status_code == "404" | bing_confidence == "Low") %>%
  select(address_for_geocoding)

agi_applications_addresses_geocoded_fixed <- agi_applications_addresses_geocoded_missing %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      # Court -> Crt seems to help
      x %>%
        str_replace("Court", "Crt") %>%
        safely_geocode_address()
    })
  ) %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

agi_applications_addresses_geocoded_missing <- agi_applications_addresses_geocoded_missing %>%
  left_join(agi_applications_addresses_geocoded_fixed, by = "address_for_geocoding")

agi_applications_addresses_geocoded <- agi_applications_addresses_geocoded %>%
  rows_update(agi_applications_addresses_geocoded_missing, by = "address_for_geocoding")

agi_applications <- agi_applications %>%
  left_join(agi_applications_addresses_geocoded, by = c("address", "address_for_geocoding"))

# Remove Toronto etc from address
agi_applications <- agi_applications %>%
  separate(address, into = "address", sep = ",", extra = "drop")

# Clean up some addresses that are written differently
agi_applications %>%
  select(bing_address, address) %>%
  distinct() %>%
  janitor::get_dupes(bing_address)

agi_applications <- agi_applications %>%
  mutate(address = case_when(
    address == "156 Overbrook Place North" ~ "156 Overbrook Place Road",
    address == "1651 Victoria Park Avenue East" ~ "1651 Victoria Park Avenue",
    address == "186 Kingsview Boulevard" ~ "186 Kingsview Blvd",
    address == "24 Leith Hill Road" ~ "24 Leith Hill Rd",
    address == "2405 Finch Avenue West" ~ "2405 Finch Avenue",
    address == "3967 Lawrence Avenue East" ~ "3967 Lawrence Ave E",
    address == "50 Cordova Court" ~ "50 Cordova Avenue",
    address %in% c("500 Murray Ross Parkway", "500 Murray Ross Pky") ~ "500 Murray Ross Pkwy",
    address == "51 Trailridge Crescent" ~ "51 Trailridge Cres",
    address == "53 Widdicombe Hill Boulevard" ~ "53 Widdicombe Hill Blvd",
    address == "555 Russell Hil Road" ~ "555 Russell Hill Road",
    address == "57 Widdicombe Hill Boulevard" ~ "57 Widdicombe Hill Blvd",
    address == "980 Lawrence Avenue East Toronto" ~ "980 Lawrence Avenue East",
    TRUE ~ address
  ))

saveRDS(agi_applications, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "geocode", "agi_applications.rds"))
