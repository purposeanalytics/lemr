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
library(tidyr)

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
  full_join(primary_market_by_neighbourhood %>%
    filter(group == "primary_rental") %>%
    select(neighbourhood, primary_rental = value),
  by = "neighbourhood"
  ) %>%
  full_join(secondary_condo_by_neighbourhood %>%
    select(neighbourhood, secondary_condo = value), by = "neighbourhood") %>%
  mutate(across(c(primary_rental, secondary_condo), coalesce, 0)) %>%
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
  mutate(market_value = sum(value, na.rm = TRUE)) %>%
  group_by(neighbourhood) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(neighbourhood, market, group) %>%
  select(neighbourhood, total, market, market_value, group, value) %>%
  mutate(prop = value / total)

# Need to fix one issue, for Forest Hill South, # apartments > # renters
# Probably due to the time offset of the data - census in May 2016, housing market survey in October 2016
# Rather than overwrite any census data, just treat it as the source of truth - and set # apartments = # renters, everything else = 0
# Since the area is largely just apartment buildings anyways

# It causes some confusion with structure type, since there are semi-detached houses there for the neighbourhood - but that's a fun easter egg if anyone finds issues with our data :~)

rental_supply_forest_hill_south <- rental_supply_by_neighbourhood %>%
  filter(neighbourhood == "Forest Hill South") %>%
  mutate(
    market_value = ifelse(market == "Secondary", 0, total),
    value = ifelse(group == "Apartment", total, 0),
    prop = ifelse(group == "Apartment", 1, 0)
  )

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  rows_update(rental_supply_forest_hill_south, by = c("neighbourhood", "market", "group"))

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

# Version for mapping ----

# Add groups for colour, then make wide
# 6 groups in c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  select(neighbourhood, group, prop) %>% complete(neighbourhood, group, fill = list(prop = 0)) %>%
  mutate(prop_group = cut(prop, seq(0, 1, length.out = 7), include.lowest = TRUE, labels = FALSE))

usethis::use_data(rental_supply_by_neighbourhood, overwrite = TRUE)
