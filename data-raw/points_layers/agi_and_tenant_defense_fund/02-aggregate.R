# Aggregate AGI and TDFs

library(dplyr)
library(purrr)
library(sf)
library(tidyr)
library(forcats)

agi_applications_and_tdf <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "clean", "agi_applications_and_tdf.rds"))

apartment_building_registry <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "clean", "apartment_building_registry.rds"))

apartment_building_registry <- apartment_building_registry %>%
  as_tibble() %>%
  select(bing_address, property_type, neighbourhood) %>%
  mutate(apartment = TRUE)

# AGIs ----
# For AGIs in apartment buildings, get AGI rate
# Unique buildings with AGIs in the last 5 years / # buildings

agi_tdf_buildings <- agi_applications_and_tdf %>%
  as_tibble() %>%
  select(address, bing_address, neighbourhood, tdf) %>%
  mutate(agi = TRUE) %>%
  group_by(address) %>%
  mutate(tdf = any(tdf)) %>%
  ungroup() %>%
  # One exception! 65A Hillside Drive in AGI, but only appears once, as 65 Hillside Drive, in registry
  mutate(address = recode(address, "65A Hillside Drive" = "65 Hillside Drive")) %>%
  distinct() %>%
  # Left join instead of flagging based on address to account for multiple buildings at an address
  left_join(apartment_building_registry, by = c("bing_address", "neighbourhood")) %>%
  mutate(apartment = coalesce(apartment, FALSE))

neighbourhoods <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "apartments_by_neighbourhood.rds")) %>%
  names()

buildings_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  filter(property_type == "PRIVATE") %>%
  mutate(neighbourhood = fct_expand(neighbourhood, neighbourhoods)) %>%
  count(neighbourhood, .drop = FALSE, name = "value")

# For apartments, count rows - since could have multiple buildings at an address
agi_by_neighbourhood_apartments <- agi_tdf_buildings %>%
  filter(apartment) %>%
  count(neighbourhood, apartment) %>%
  mutate(neighbourhood = fct_expand(neighbourhood, neighbourhoods)) %>%
  complete(neighbourhood, apartment, fill = list(n = 0)) %>%
  full_join(buildings_by_neighbourhood, by = "neighbourhood") %>%
  mutate(
    prop = n / value
  )

# For non-apartments, count addresses
agi_by_neighbourhood_non_apartments <- agi_tdf_buildings %>%
  filter(!apartment) %>%
  group_by(neighbourhood, apartment) %>%
  summarise(n = n_distinct(address), .groups = "drop") %>%
  mutate(neighbourhood = fct_expand(neighbourhood, neighbourhoods)) %>%
  complete(neighbourhood, apartment, fill = list(n = 0)) %>%
  full_join(buildings_by_neighbourhood, by = "neighbourhood") %>%
  mutate(
    prop = n / value
  )

agi_by_neighbourhood <- agi_by_neighbourhood_apartments %>%
  bind_rows(agi_by_neighbourhood_non_apartments)

agi_city <- agi_by_neighbourhood %>%
  group_by(apartment) %>%
  summarise(
    n = sum(n),
    value = sum(value)
  ) %>%
  mutate(prop = n / value) %>%
  select(-value)

# TDFs -----
# Buildings with TDFs / buildings with AGIs

tdf_by_neighbourhood <- agi_tdf_buildings %>%
  filter(tdf, apartment) %>%
  group_by(neighbourhood) %>%
  summarise(n = n_distinct(bing_address)) %>%
  full_join(agi_by_neighbourhood %>% filter(apartment), by = "neighbourhood", suffix = c("_tdf", "_agi")) %>%
  mutate(
    n_tdf = coalesce(n_tdf, 0),
    prop = ifelse(n_agi == 0, NA_real_, n_tdf / n_agi)
  ) %>%
  select(-value) %>%
  rename(n = n_tdf) %>%
  select(neighbourhood, n, n_agi, prop)

tdf_city <- tdf_by_neighbourhood %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    value = sum(n_agi, na.rm = TRUE)
  ) %>%
  mutate(prop = n / value) %>%
  select(n, prop)

tdf_by_neighbourhood <- tdf_by_neighbourhood %>%
  select(-n_agi)

# Clean up AGIs to be more appropriate / clear on apartment building / not

agi_by_neighbourhood <- agi_by_neighbourhood %>%
  select(-value) %>%
  mutate(
    group = ifelse(apartment, "Apartment building", "Non-apartment building"),
    prop = ifelse(!apartment, NA_real_, prop)
  ) %>%
  select(neighbourhood, group, value = n, prop)

agi_city <- agi_city %>%
  mutate(
    group = ifelse(apartment, "Apartment building", "Non-apartment building"),
    prop = ifelse(!apartment, NA_real_, prop)
  ) %>%
  select(group, value = n, prop)

# Save ----

saveRDS(tdf_by_neighbourhood, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_by_neighbourhood.rds"))
saveRDS(agi_by_neighbourhood, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_by_neighbourhood.rds"))
saveRDS(agi_city, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_city.rds"))
saveRDS(tdf_city, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_city.rds"))
