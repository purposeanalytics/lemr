# Aggregate rental supply

# Breakdown is:
# Primary market:
# - Apartment
# - Non-apartment (row-houses)
# Secondary:
# - Condo
# - Non-condo (diff of total renters - primary - condo)

# Non-market -> TBD ******
# - Social housing
# - Supportive housing

library(dplyr)
library(purrr)

# Total renters ----

neighbourhood_profile <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "neighbourhood_profiles.rds"))

renters_by_neighbourhood <- neighbourhood_profile %>%
  transpose() %>%
  pluck("household_tenure") %>%
  bind_rows() %>%
  filter(group == "Renter") %>%
  select(neighbourhood, renters = value)

city_profile <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "city_profile.rds"))

renters_city <- city_profile[["household_tenure"]] %>%
  filter(group == "Renter") %>%
  select(renters = value)

# Primary market -----

primary_market_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_by_neighbourhood.rds"))

primary_market_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_city.rds"))

# Secondary market -----

## Condos -----

secondary_condo_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "aggregate", "secondary_condo_by_neighbourhood.rds"))

secondary_condo_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "aggregate", "secondary_condo_city.rds"))

## Non-condo -----

secondary_non_condo_by_neighbourhood <- renters_by_neighbourhood %>%
  left_join(primary_market_by_neighbourhood %>%
    filter(group == "primary_rental") %>%
    select(neighbourhood, primary_rental = value),
  by = "neighbourhood"
  ) %>%
  full_join(secondary_condo_by_neighbourhood %>%
    select(neighbourhood, secondary_condo = value), by = "neighbourhood") %>%
  mutate(value = renters - primary_rental - secondary_condo) %>%
  select(neighbourhood, value) %>%
  mutate(group = "secondary non-condo")

secondary_non_condo_city <- renters_city %>%
  bind_cols(primary_market_city %>%
    filter(group == "primary_rental") %>%
    select(primary_rental = value)) %>%
  bind_cols(secondary_condo_city %>%
    select(secondary_condo = value)) %>%
  mutate(value = renters - primary_rental - secondary_condo) %>%
  select(value) %>%
  mutate(group = "secondary non-condo")

# Breakdown -----

rental_supply_by_neighbourhood <- primary_market_by_neighbourhood %>%
  filter(group != "primary_rental") %>%
  bind_rows(secondary_condo_by_neighbourhood %>%
    select(neighbourhood, value) %>%
    mutate(group = "condo")) %>%
  bind_rows(secondary_non_condo_by_neighbourhood) %>%
  mutate(
    group = recode(group, apartments = "Apartment", row_houses = "Non-Apartment", condo = "Condo", "secondary non-condo" = "Non-Condo"),
    market = case_when(
      group %in% c("Apartment", "Non-Apartment") ~ "Primary",
      group %in% c("Condo", "Non-Condo") ~ "Secondary"
    )
  ) %>%
  group_by(neighbourhood, market) %>%
  mutate(market_value = sum(value)) %>%
  group_by(neighbourhood) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  arrange(neighbourhood, market, group) %>%
  select(neighbourhood, total, market, market_value, group, value) %>%
  mutate(prop = value / total)

rental_supply_city <- primary_market_city %>%
  filter(group != "primary_rental") %>%
  bind_rows(secondary_condo_city %>%
    select(value) %>%
    mutate(group = "condo")) %>%
  bind_rows(secondary_non_condo_city) %>%
  mutate(
    group = recode(group, apartments = "Apartment", row_houses = "Non-Apartment", condo = "Condo", "secondary non-condo" = "Non-Condo"),
    market = case_when(
      group %in% c("Apartment", "Non-Apartment") ~ "Primary",
      group %in% c("Condo", "Non-Condo") ~ "Secondary"
    )
  ) %>%
  group_by(market) %>%
  mutate(market_value = sum(value)) %>%
  ungroup() %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  arrange(market, group) %>%
  select(total, market, market_value, group, value) %>%
  mutate(prop = value / total)

# Save -----

saveRDS(rental_supply_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))
saveRDS(rental_supply_city, here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_city.rds"))

