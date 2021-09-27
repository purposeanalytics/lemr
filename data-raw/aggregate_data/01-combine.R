# Combine aggregate data sets into two data sets:
# neighbourhoods_aggregate and city_aggregate

# Census profiles data via census_profiles_2016/
# Rental supply via rental_supply/
# Structure type for renters via rental_supply/census_custom_tab_2016_table1/
# Bedrooms for renters via census_custom_tab_2016_table2/
# Household size for renters via census_custom_tab_2016_table2/
# Household income via census_custom_tab_2016_table1_income/
# Proximity measures / amenity density via proximity_measures/
# LEM via affordable_rental_market/
# Core housing need via core_housing_need/
# Evictions data via evictions_by_neighbourhood/
# Vacancy rate via vacancy_rate/

# Point data aggregated
# Apartment buildings / units via points_layers/apartment_building_registry/
# RentSafeTO Scores via points_layers/apartment_building_evaluation/
# AGI and Tenant Defense Fund via points_layers/agi_and_tenant_defense_fund/
# Rooming houses via points_layers/rooming_houses/

library(dplyr)
library(purrr)
library(tidyr)
library(sf)
devtools::load_all()

# Census profiles -----

neighbourhood_profiles <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "neighbourhood_profiles.rds"))

city_profile <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "aggregate", "city_profile.rds"))

# Structure type for renters -----

structure_type_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "aggregate", "structure_type_by_neighbourhood.rds"))

structure_type_by_neighbourhood <- structure_type_by_neighbourhood %>%
  split(.$neighbourhood)

structure_type_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "aggregate", "structure_type_city.rds"))

# Bedrooms for renters -----

bedrooms_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "aggregate", "number_of_bedrooms_by_neighbourhood.rds"))
bedrooms_city <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "aggregate", "number_of_bedrooms_city.rds"))

# Household size for renters ----

household_size_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "aggregate", "household_size_by_neighbourhood.rds"))
household_size_city <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "aggregate", "household_size_city.rds"))

# Household income for renters ----

average_income_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "aggregate", "average_income_by_neighbourhood.rds")) %>%
  split(.$neighbourhood)

average_income_city <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "aggregate", "average_income_city.rds"))

# Rental supply -----

rental_supply_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  split(.$neighbourhood)

rental_supply_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_city.rds"))

# Proximity measures / amenity density -----

amenity_density_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "proximity_measures", "aggregate", "amenity_density_by_neighbourhood.rds"))

amenity_density_city <- readRDS(here::here("data-raw", "aggregate_data", "proximity_measures", "aggregate", "amenity_density_city.rds"))

# LEM -----

lem_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_neighbourhood_breakdown.rds"))

lem_city <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_city_breakdown.rds"))

# Core housing need -----

core_housing_need_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood.rds"))
core_housing_need_city <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_city.rds"))
core_housing_need_distribution <- core_housing_need_by_neighbourhood %>%
  map(as_tibble) %>%
  bind_rows()

# Evictions -----

evictions_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "aggregate", "evictions_by_neighbourhood.rds"))
evictions_city <- readRDS(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "aggregate", "evictions_city.rds"))
evictions_distribution <- evictions_by_neighbourhood %>%
  map(as_tibble) %>%
  bind_rows()

# Vacancy rate ----

vacancy_rate_by_neighbourhood_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2016.rds"))
vacancy_rate_city_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2016.rds")) %>%
  pull(vacancy_rate) %>%
  round(3)
vacancy_rate_distribution_2016 <- vacancy_rate_by_neighbourhood_2016 %>%
  map(as_tibble) %>%
  bind_rows()

vacancy_rate_by_neighbourhood_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2020.rds"))
vacancy_rate_city_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2020.rds")) %>%
  pull(vacancy_rate) %>%
  round(3)
vacancy_rate_distribution_2020 <- vacancy_rate_by_neighbourhood_2020 %>%
  map(as_tibble) %>%
  bind_rows()

# Apartment buildings -----

number_of_apartments_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_city.rds"))
number_of_units_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_units_city.rds"))
number_of_apartments_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_distribution.rds"))
units_by_neighbourhood_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood_distribution.rds"))
apartments_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "apartments_by_neighbourhood.rds"))
units_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood.rds"))

## Apartment buildings by type ----

apartments_by_type_by_neighbourhood <- lemr::buildings %>%
  as_tibble() %>%
  filter(apartment) %>%
  group_by(neighbourhood, group = property_type) %>%
  summarise(
    buildings = n(),
    units = sum(units),
    .groups = "drop"
  ) %>%
  complete(neighbourhood, group, fill = list(buildings = 0, units = 0)) %>%
  filter(!is.na(group))

apartments_by_type_by_neighbourhood_buildings <- apartments_by_type_by_neighbourhood %>%
  split(.$neighbourhood) %>%
  map(~ split(.x, .x$group)) %>%
  map_depth(.depth = 2, pull, "buildings")

apartments_by_type_by_neighbourhood_units <- apartments_by_type_by_neighbourhood %>%
  split(.$neighbourhood) %>%
  map(~ split(.x, .x$group)) %>%
  map_depth(.depth = 2, pull, "units")

apartments_by_type_city <- apartments_by_type_by_neighbourhood %>%
  group_by(group) %>%
  summarise(buildings = sum(buildings), units = sum(units))

apartments_by_type_city_buildings <- apartments_by_type_city %>%
  split(.$group) %>%
  map(pull, "buildings")

apartments_by_type_city_units <- apartments_by_type_city %>%
  split(.$group) %>%
  map(pull, "units")

# RentSafeTO -----

median_score_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "median_score_city.rds"))
apartment_building_evaluation_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "apartment_building_evaluation_distribution.rds"))
median_score_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "median_score_by_neighbourhood.rds"))

# AGI / TDF -----

tdf_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_by_neighbourhood.rds")) %>%
  split(.$neighbourhood)

agi_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_by_neighbourhood.rds")) %>%
  split(.$neighbourhood)

agi_city <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_city.rds"))
tdf_city <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_city.rds"))

# Rooming houses ----

rooming_houses_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "rooming_houses", "aggregate", "rooming_houses_by_neighbourhood.rds"))

rooming_houses_city <- rooming_houses_by_neighbourhood %>%
  group_by(group) %>%
  summarise(value = sum(value))

rooming_houses_by_neighbourhood <- rooming_houses_by_neighbourhood %>%
  split(.$neighbourhood)

# Make aggregate data set ----

neighbourhood_aggregate <- neighbourhood_profiles

city_aggregate <- city_profile

for (i in names(neighbourhood_aggregate)) {
  neighbourhood_aggregate_i <- neighbourhood_aggregate[[i]]

  neighbourhood_aggregate_i[["structure_type"]] <- structure_type_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["bedrooms"]] <- bedrooms_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["household_size"]] <- household_size_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["average_total_income"]] <- average_income_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["rental_supply"]] <- rental_supply_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["amenity_density"]] <- amenity_density_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["lem"]] <- lem_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["core_housing_need"]] <- core_housing_need_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["evictions"]] <- evictions_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["vacancy_rate_2016"]] <- vacancy_rate_by_neighbourhood_2016[[i]]
  neighbourhood_aggregate_i[["vacancy_rate_2020"]] <- vacancy_rate_by_neighbourhood_2020[[i]]

  neighbourhood_aggregate_i[["number_of_buildings"]] <- apartments_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["number_of_buildings_private"]] <- apartments_by_type_by_neighbourhood_buildings[[i]][["Privately owned"]]
  neighbourhood_aggregate_i[["number_of_buildings_tch"]] <- apartments_by_type_by_neighbourhood_buildings[[i]][["Toronto Community Housing"]]
  neighbourhood_aggregate_i[["number_of_buildings_social_housing"]] <- apartments_by_type_by_neighbourhood_buildings[[i]][["Social housing"]]
  neighbourhood_aggregate_i[["number_of_units"]] <- units_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["number_of_units_private"]] <- apartments_by_type_by_neighbourhood_units[[i]][["Privately owned"]]
  neighbourhood_aggregate_i[["number_of_units_tch"]] <- apartments_by_type_by_neighbourhood_units[[i]][["Toronto Community Housing"]]
  neighbourhood_aggregate_i[["number_of_units_social_housing"]] <- apartments_by_type_by_neighbourhood_units[[i]][["Social housing"]]
  neighbourhood_aggregate_i[["apartment_building_evaluation"]] <- median_score_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["agi"]] <- agi_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["tdf"]] <- tdf_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["rooming_houses"]] <- rooming_houses_by_neighbourhood[[i]]

  neighbourhood_aggregate[[i]] <- neighbourhood_aggregate_i
}

city_aggregate[["structure_type"]] <- structure_type_city
city_aggregate[["bedrooms"]] <- bedrooms_city
city_aggregate[["household_size"]] <- household_size_city
city_aggregate[["average_total_income"]] <- average_income_city
city_aggregate[["rental_supply"]] <- rental_supply_city
city_aggregate[["amenity_density"]] <- amenity_density_city
city_aggregate[["lem"]] <- lem_city
city_aggregate[["core_housing_need"]] <- core_housing_need_city
city_aggregate[["core_housing_need_distribution"]] <- core_housing_need_distribution
city_aggregate[["evictions"]] <- evictions_city
city_aggregate[["evictions_distribution"]] <- evictions_distribution
city_aggregate[["vacancy_rate_2016"]] <- vacancy_rate_city_2016
city_aggregate[["vacancy_rate_2020"]] <- vacancy_rate_city_2020
city_aggregate[["vacancy_rate_2016_distribution"]] <- vacancy_rate_distribution_2016
city_aggregate[["vacancy_rate_2020_distribution"]] <- vacancy_rate_distribution_2020

city_aggregate[["number_of_buildings"]] <- number_of_apartments_city
city_aggregate[["number_of_buildings_distribution"]] <- number_of_apartments_distribution
city_aggregate[["number_of_buildings_private"]] <- apartments_by_type_city_buildings[["Privately owned"]]
city_aggregate[["number_of_buildings_tch"]] <- apartments_by_type_city_buildings[["Toronto Community Housing"]]
city_aggregate[["number_of_buildings_social_housing"]] <- apartments_by_type_city_buildings[["Social housing"]]
city_aggregate[["number_of_units"]] <- number_of_units_city
city_aggregate[["number_of_units_private"]] <- apartments_by_type_city_units[["Privately owned"]]
city_aggregate[["number_of_units_tch"]] <- apartments_by_type_city_units[["Toronto Community Housing"]]
city_aggregate[["number_of_units_social_housing"]] <- apartments_by_type_city_units[["Social housing"]]
city_aggregate[["number_of_units_distribution"]] <- units_by_neighbourhood_distribution
city_aggregate[["apartment_building_evaluation"]] <- median_score_city[["value"]]
city_aggregate[["apartment_building_evaluation_distribution"]] <- apartment_building_evaluation_distribution
city_aggregate[["agi"]] <- agi_city
city_aggregate[["tdf"]] <- tdf_city
city_aggregate[["rooming_houses"]] <- rooming_houses_city

# Save data sets ----

usethis::use_data(neighbourhood_aggregate, overwrite = TRUE)
usethis::use_data(city_aggregate, overwrite = TRUE)
