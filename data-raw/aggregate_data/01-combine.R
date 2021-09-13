# Combine aggregate data sets into two data sets:
# neighbourhoods_aggregate and city_aggregate

# Census profiles data via census_profiles_2016/
# Rental supply via rental_supply/
# Structure type for renters via rental_supply/census_custom_tab_2016_table1/
# Bedrooms for renters via census_custom_tab_2016_table2/
# Proximity measures / amenity density via proximity_measures/
# LEM via affordable_rental_market/
# Core housing need via core_housing_need/

# Point data aggregated
# Apartment buildings / units via points_layers/apartment_building_registry/
# RentSafeTO Scores via points_layers/apartment_building_evaluation/
# AGI and Tenant Defense Fund via points_layers/agi_and_tenant_defense_fund/

library(dplyr)

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

# Rental supply -----

rental_supply_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))

rental_supply_by_neighbourhood <- rental_supply_by_neighbourhood %>%
  split(.$neighbourhood)

rental_supply_city <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_city.rds"))

# Proximity measures / amenity density -----

amenity_density_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "proximity_measures", "aggregate", "amenity_density_by_neighbourhood.rds"))

amenity_density_city <- readRDS(here::here("data-raw", "aggregate_data", "proximity_measures", "aggregate", "amenity_density_city.rds"))

# LEM -----

lem_by_neighbourhood <- readRDS( here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_neighbourhood_breakdown.rds"))

lem_city <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_city_breakdown.rds"))

# Core housing need -----

core_housing_need_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood.rds"))
core_housing_need_city <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_city.rds"))

# Apartment buildings -----

number_of_apartments_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_city.rds"))
number_of_units_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_units_city.rds"))
number_of_apartments_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_distribution.rds"))
units_by_neighbourhood_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood_distribution.rds"))
apartments_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "apartments_by_neighbourhood.rds"))
units_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood.rds"))

# RentSafeTO -----

median_score_city <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "median_score_city.rds"))
apartment_building_evaluation_distribution <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "apartment_building_evaluation_distribution.rds"))
median_score_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "aggregate", "median_score_by_neighbourhood.rds"))

# AGI / TDF -----

tdf_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_by_neighbourhood.rds")) %>%
  select(-n_agi) %>%
  split(.$neighbourhood)

agi_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_by_neighbourhood.rds")) %>%
  select(-value) %>%
  split(.$neighbourhood)

agi_city <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "agi_city.rds"))
tdf_city <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "aggregate", "tdf_city.rds"))

# Make aggregate data set ----

neighbourhood_aggregate <- neighbourhood_profiles

city_aggregate <- city_profile

for (i in names(neighbourhood_aggregate)) {

  neighbourhood_aggregate_i <- neighbourhood_aggregate[[i]]

  neighbourhood_aggregate_i[["structure_type"]] <- structure_type_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["bedrooms"]] <- bedrooms_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["rental_supply"]] <- rental_supply_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["amenity_density"]] <- amenity_density_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["lem"]] <- lem_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["core_housing_need"]] <- core_housing_need_by_neighbourhood[[i]]

  neighbourhood_aggregate_i[["number_of_buildings"]] <- apartments_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["number_of_units"]] <- units_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["apartment_building_evaluation"]] <- median_score_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["agi"]] <- agi_by_neighbourhood[[i]]
  neighbourhood_aggregate_i[["tdf"]] <- tdf_by_neighbourhood[[i]]

  neighbourhood_aggregate[[i]] <- neighbourhood_aggregate_i
}

city_aggregate[["structure_type"]] <- structure_type_city
city_aggregate[["bedrooms"]] <- bedrooms_city
city_aggregate[["rental_supply"]] <- rental_supply_city
city_aggregate[["amenity_density"]] <- amenity_density_city
city_aggregate[["lem"]] <- lem_city
city_aggregate[["core_housing_need"]] <- core_housing_need_city

city_aggregate[["number_of_buildings"]] <- number_of_apartments_city
city_aggregate[["number_of_buildings_distribution"]] <- number_of_apartments_distribution
city_aggregate[["number_of_units"]] <- number_of_units_city
city_aggregate[["number_of_units_distribution"]] <- units_by_neighbourhood_distribution
city_aggregate[["apartment_building_evaluation"]] <- median_score_city[["value"]]
city_aggregate[["apartment_building_evaluation_distribution"]] <- apartment_building_evaluation_distribution
city_aggregate[["agi"]] <- agi_city
city_aggregate[["tdf"]] <- tdf_city

# Save data sets ----

usethis::use_data(neighbourhood_aggregate, overwrite = TRUE)
usethis::use_data(city_aggregate, overwrite = TRUE)
