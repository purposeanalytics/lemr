# Aggregate rental supply

# Breakdown is:
# Primary market:
# - Apartment
# - Non-apartment (row-houses)
# Secondary:
# - Condo
# - Non-condo (diff of total renters - primary - condo - non-market)

# Non-market
# - Toronto Community Housing
# - Other non-market

library(dplyr)
library(purrr)
library(tidyr)
devtools::load_all()

# Total renters ----

neighbourhood_profile <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "neighbourhood_profiles.rds"))

renters_by_neighbourhood <- neighbourhood_profile %>%
  transpose() %>%
  pluck("household_tenure") %>%
  bind_rows() %>%
  filter(group == "Renter") %>%
  select(neighbourhood, renters_prop = prop)

households_by_neighbourhood <- neighbourhood_profile %>%
  transpose() %>%
  pluck("households") %>%
  map(as_tibble) %>%
  bind_rows(.id = "neighbourhood")

renters_by_neighbourhood <- renters_by_neighbourhood %>%
  left_join(households_by_neighbourhood, by = "neighbourhood") %>%
  mutate(renters = round(value * renters_prop))

city_profile <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "city_profile.rds"))

renters_city <- city_profile[["household_tenure"]] %>%
  filter(group == "Renter") %>%
  pull(prop)

households_city <- city_profile[["households"]]

renters_city <- tibble(renters = round(households_city * renters_city))

# Primary market -----

primary_market_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_by_neighbourhood.rds"))

primary_market_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_city.rds"))

# Non-market

social_housing_and_tch_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "aggregate", "social_housing_and_tch_by_neighbourhood.rds")) %>%
  select(neighbourhood, tch = tch_units_rgi, other_non_market = other_social_units_rgi)

social_housing_and_tch_city <- social_housing_and_tch_by_neighbourhood %>%
  summarise(across(c(tch, other_non_market), sum))

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
  full_join(social_housing_and_tch_by_neighbourhood, by = "neighbourhood") %>%
  mutate(across(c(primary_rental, secondary_condo, tch, other_non_market), coalesce, 0)) %>%
  mutate(
    value = renters - primary_rental - secondary_condo - tch - other_non_market,
    value = ifelse(value < 0, 0, value)
  ) %>%
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
  bind_rows(
    social_housing_and_tch_by_neighbourhood %>%
      pivot_longer(-neighbourhood, names_to = "group", values_to = "value")
  ) %>%
  mutate(
    group = recode(group, apartments = "Apartment", row_houses = "Non-Apartment", condo = "Condo", "secondary non-condo" = "Non-Condo", tch = "Toronto Community Housing", other_non_market = "Other Non-Market")
  ) %>%
  arrange(neighbourhood, group) %>%
  select(neighbourhood, group, value) %>%
  left_join(renters_by_neighbourhood %>%
    select(neighbourhood, renters), by = "neighbourhood")

# Need to fix some issues
# For Forest Hill South, # apartments > # renters
# Probably due to the time offset of the data - census in May 2016, housing market survey in October 2016
# Rather than overwrite any census data, just treat it as the source of truth - and set # apartments = # renters, everything else = 0
# Since the area is largely just apartment buildings anyways

# It causes some confusion with structure type, since there are semi-detached houses there for the neighbourhood - but that's a fun easter egg if anyone finds issues with our data :~)

rental_supply_forest_hill_south <- rental_supply_by_neighbourhood %>%
  filter(neighbourhood == "Forest Hill South") %>%
  mutate(
    value = ifelse(group == "Apartment", renters, 0)
  )

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  rows_update(rental_supply_forest_hill_south, by = c("neighbourhood", "group"))

# Some other issues where primary + secondary + non-market > total renters
# Likely due to TCH having vacancies/demolition and reconstruction of units
# So clip from there first, then other non-market if still over

rental_supply_fix_social_housing <- rental_supply_by_neighbourhood %>%
  group_by(neighbourhood) %>%
  filter(sum(value) > renters) %>%
  mutate(diff = sum(value) - renters) %>%
  ungroup() %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(
    `Toronto Community Housing` = `Toronto Community Housing` - diff,
    `Other Non-Market` = ifelse(`Toronto Community Housing` < 0, `Other Non-Market` - abs(`Toronto Community Housing`), `Other Non-Market`),
    `Toronto Community Housing` = ifelse(`Toronto Community Housing` < 0, 0, `Toronto Community Housing`)
  ) %>%
  select(-diff) %>%
  pivot_longer(-c(neighbourhood, renters), names_to = "group", values_to = "value")

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  rows_update(rental_supply_fix_social_housing, by = c("neighbourhood", "renters", "group"))

# Round everything to the nearest 25, to make clear that it's an estimate
rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  mutate(value = plyr::round_any(value, 25)) %>%
  group_by(neighbourhood) %>%
  mutate(renters = sum(value)) %>%
  ungroup()

# Add market info and proportion
rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  mutate(market = case_when(
    group %in% c("Apartment", "Non-Apartment") ~ "Primary",
    group %in% c("Condo", "Non-Condo") ~ "Secondary",
    group %in% c("Toronto Community Housing", "Other Non-Market") ~ "Non-market"
  )) %>%
  group_by(neighbourhood, market) %>%
  mutate(market_value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    prop = round(value / renters, 3),
    market_prop = round(market_value / renters, 3)
  )

# Complete data
rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  complete(nesting(market, group), neighbourhood,
    fill = list(value = 0, market_value = 0, prop = 0, market_prop = 0)
  )

# Aggregate for city
rental_supply_city <- rental_supply_by_neighbourhood %>%
  group_by(market, group) %>%
  summarise(value = sum(value), .groups = "drop_last") %>%
  mutate(market_value = sum(value)) %>%
  ungroup() %>%
  mutate(renters = sum(value)) %>%
  mutate(
    prop = round(value / renters, 3),
    market_prop = round(market_value / renters, 3)
  )

# Save -----

saveRDS(rental_supply_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))
saveRDS(rental_supply_city, here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_city.rds"))

# Version for mapping ----

# Add groups for colour, then make wide

# Collapse primary and non-market

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  mutate(map_group = case_when(
    group == "Condo" ~ "rental_supply_condo",
    group == "Non-Condo" ~ "rental_supply_non_condo",
    market == "Primary" ~ "rental_supply_primary",
    market == "Non-market" ~ "rental_supply_non_market"
  )) %>%
  group_by(neighbourhood, group = map_group) %>%
  summarise(prop = sum(prop), .groups = "drop") %>%
  mutate(
    prop_group = cut(prop, seq(0, 1, length.out = length(low_high_legend_colors())), include.lowest = FALSE, labels = FALSE),
    prop_group = ifelse(prop == 0, 0, prop_group)
  )

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  select(-prop) %>%
  pivot_wider(names_from = group, values_from = prop_group)

saveRDS(rental_supply_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood_layer.rds"))
