# Geocode the data to try to match with the apartment registry

# We only need to geocode the ones that don't have a market type, in order to get that

library(dplyr)
library(sf)
library(purrr)
library(progress)
library(tidyr)
devtools::load_all()

rental_data <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "extract", "rental_data.rds"))

missing_market_type <- rental_data %>%
  filter(is.na(market_type))

pb <- progress_bar$new(total = nrow(missing_market_type))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

missing_market_type <- missing_market_type %>%
  mutate(
    address_geocode = map(clean_address, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

# Separate results from errors
missing_market_type <- missing_market_type %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
missing_market_type <- missing_market_type %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

# Classify as primary rental if they're in the registry
classified_primary_rental <- missing_market_type %>%
  select(id, title, description, clean_address, bing_address) %>%
  semi_join(apartment_building_registry, by = "bing_address") %>%
  mutate(market_type = "Primary Rental") %>%
  select(-bing_address)

# Update
rental_data <- rental_data %>%
  rows_update(classified_primary_rental, by = c("id"))

# Let's just assume it's all Non-Condo Secondary Rental
# Except for townhouses that AREN'T room / shared room

rental_data <- rental_data %>%
  mutate(market_type = case_when(
    !is.na(market_type) ~ market_type,
    type == "Townhouse" & !bedrooms %in% c("Room", "Shared Room") ~ "Condo Rental",
    TRUE ~ "Non-Condo Secondary Rental"
  ))

saveRDS(rental_data, here::here("data-raw", "aggregate_data", "affordable_rental_market", "geocode", "rental_data.rds"))
