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
    address_for_geocoding = glue::glue("{street_number} {street_name}, Toronto, ON {postal_code}"),
    id = row_number()
  )

agi_applications_addresses <- agi_applications %>%
  distinct(address_for_geocoding)

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

agi_applications_addresses_geocoded_still_404 <- agi_applications_addresses_geocoded_fixed %>%
  filter(bing_status_code == "404" | bing_confidence == "Low") %>%
  mutate(
    fixed_address_for_geocoding = case_when(
      address_for_geocoding == "30 1/2 Macaulay Avenue, Toronto, ON M6P3P6" ~ "30 Macaulay Avenue, Toronto, ON M6P3P6",
      address_for_geocoding == "409 1/2 Roncesvalles Avenue, Toronto, ON M6R2N1" ~ "409 Roncesvalles Ave, Toronto, ON M6R 2N1",
      address_for_geocoding == "NA Crestview Apts (leacrest & Mallory), Toronto, ON M4G1E6" ~ "30-75 Leacrest Rd, Toronto, ON M4G 1E6"
    )
  ) %>%
  select(address_for_geocoding, fixed_address_for_geocoding) %>%
  mutate(address_geocode = map(fixed_address_for_geocoding, function(x) {
    safely_geocode_address(x)
  })) %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

agi_applications_addresses_geocoded_fixed <- agi_applications_addresses_geocoded_fixed %>%
  rows_update(agi_applications_addresses_geocoded_still_404 %>%
    select(-fixed_address_for_geocoding), by = "address_for_geocoding")

agi_applications_addresses_geocoded_missing <- agi_applications_addresses_geocoded_missing %>%
  left_join(agi_applications_addresses_geocoded_fixed, by = "address_for_geocoding")

agi_applications_addresses_geocoded <- agi_applications_addresses_geocoded %>%
  rows_update(agi_applications_addresses_geocoded_missing, by = "address_for_geocoding")

agi_applications <- agi_applications %>%
  left_join(agi_applications_addresses_geocoded, by = "address_for_geocoding")

saveRDS(agi_applications, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "geocode", "agi_applications.rds"))
