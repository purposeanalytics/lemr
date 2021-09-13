# Aggregate AGI and TDFs

library(dplyr)
library(purrr)
devtools::load_all()

agi_applications_and_tdf <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "clean", "agi_applications_and_tdf.rds"))

apartment_building_registry <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "clean", "apartment_building_registry.rds"))

# AGIs ----
# Unique buildings with AGIs in the last 5 years / # buildings

agi_tdf_buildings <- agi_applications_and_tdf %>%
  as_tibble() %>%
  select(bing_address, neighbourhood, tdf) %>%
  mutate(agi = TRUE) %>%
  group_by(bing_address) %>%
  mutate(tdf = any(tdf)) %>%
  ungroup() %>%
  distinct() %>%
  semi_join(apartment_building_registry, by = "bing_address")

agi_by_neighbourhood <- agi_tdf_buildings %>%
  filter(agi) %>%
  group_by(neighbourhood) %>%
  summarise(n = n_distinct(bing_address))

buildings_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "apartments_by_neighbourhood.rds")) %>%
  map(as_tibble) %>%
  bind_rows(.id = "neighbourhood")

agi_by_neighbourhood <- agi_by_neighbourhood %>%
  full_join(buildings_by_neighbourhood, by = "neighbourhood") %>%
  mutate(
    n = coalesce(n, 0),
    prop = n / value
  )

agi_city <- sum(agi_by_neighbourhood[["n"]]) / sum(agi_by_neighbourhood[["value"]])

# TDFs -----
# Buildings with TDFs / buildings with AGIs

tdf_by_neighbourhood <- agi_tdf_buildings %>%
  filter(tdf) %>%
  group_by(neighbourhood) %>%
  summarise(n = n_distinct(bing_address))

tdf_by_neighbourhood <- tdf_by_neighbourhood %>%
  full_join(agi_by_neighbourhood, by = "neighbourhood", suffix = c("_tdf", "_agi")) %>%
  mutate(
    n_tdf = coalesce(n_tdf, 0),
    prop = ifelse(n_agi == 0, NA_real_, n_tdf / n_agi)
  ) %>%
  select(-value) %>%
  rename(n = n_tdf)

tdf_city <- sum(tdf_by_neighbourhood[["n"]], na.rm = TRUE) / sum(tdf_by_neighbourhood[["n_agi"]], na.rm = TRUE)

# Save ----

saveRDS(tdf_by_neighbourhood, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_by_neighbourhood.rds"))
saveRDS(agi_by_neighbourhood, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_by_neighbourhood.rds"))
saveRDS(agi_city, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_city.rds"))
saveRDS(tdf_city, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_city.rds"))
