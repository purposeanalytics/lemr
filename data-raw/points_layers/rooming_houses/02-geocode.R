# geocode rooming houses

library(dplyr)
library(janitor)
library(stringr)
library(progress)
library(purrr)
library(tidyr)
library(sf)
devtools::load_all()


rooming_houses <- readRDS(here::here("data-raw", "points_layers", "rooming_houses", "extract", "rooming_houses.rds"))


# assemble address and reconcile in date and final ldate columns
rooming_houses <- rooming_houses %>%
  clean_names() %>%
  mutate(address = glue::glue("{number} {street} {street_type} {direction}, {city}, ON {postal_code}", .na = ""),
         address = str_replace(address, " *,", ",")) %>%
  # observed a few duplicates
  distinct() %>%
  group_by(address) %>%
  arrange(in_date, address) %>%
  # assume if there are two dates "In dates" in a row with no "Final ldate" that the second one supercedes the previous one
  mutate(final_ldate = if_else(is.na(final_ldate) & is.na(lead(final_ldate)), lead(in_date), final_ldate),
         Count = n()) %>%
  ungroup()


# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(rooming_houses))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

rooming_houses_geocoded <- rooming_houses %>%
  mutate(
    # Combine address, city (Toronto, ON), and FSA for better results when geocoding
    address_geocode = map(address, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

# Separate results from errors
rooming_houses_geocoded <- rooming_houses_geocoded %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
rooming_houses_geocoded <- rooming_houses_geocoded %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))


# Sometimes the call is "successful" but nothing actually comes through
# For ones that are missing, requery - they mostly come up again!
geocode_missing <- rooming_houses_geocoded %>%
  select(address, starts_with("bing")) %>%
  filter(is.na(bing_latitude) | is.na(bing_longitude) | is.na(bing_postal_code))

geocode_missing_filled <- geocode_missing %>%
  select(address) %>%
  mutate(address_geocode = map(address, function(x) {
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
if (nrow(geocode_missing_filled) > 0) {
  rooming_houses_geocoded <- rooming_houses_geocoded %>%
    rows_update(geocode_missing_filled, by = c("_id", "address_for_geocoding"))
}



saveRDS(rooming_houses_geocoded, here::here("data-raw", "points_layers", "rooming_houses", "geocode", "rooming_houses_geocoded.rds"))
