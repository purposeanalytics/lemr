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

# get missing values
rooming_houses_missing <- rooming_houses_geocoded %>%
  filter(is.na(bing_status_code))  %>%
  select(!starts_with("bing"), -address_geocode_error) %>%
  mutate(
    address = str_sub(address, 1, -3L),
    # Combine address, city (Toronto, ON), and FSA for better results when geocoding
    address_geocode = map(address, function(x) {
      safely_geocode_address(x)
    })
  )

# Separate results from errors
rooming_houses_missing <- rooming_houses_missing %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
rooming_houses_missing <- rooming_houses_missing %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

rooming_houses_geocoded <- rooming_houses_geocoded %>%
  filter(!is.na(bing_status_code)) %>%
  bind_rows(rooming_houses_missing)



# make spatial
rooming_houses_sf <- rooming_houses_geocoded %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = 26917) %>%
  select(-address_geocode_error, -c(bing_status_code:bing_confidence))

saveRDS(rooming_houses_sf, here::here("data-raw", "points_layers", "rooming_houses", "geocoded", "rooming_houses_sf.rds"))
