# Combine aggregate data sets into two data sets:
# neighbourhoods_aggregate and city_aggregate

# Census profiles data via census_profiles_2016/
# Rental supply via rental_supply/
# Structure type for renters via rental_supply/census_custom_tab_2016_table1/
# Bedrooms for renters via census_custom_tab_2016_table2/
# Proximity measures / amenity density via proximity_measures/
# LEM via affordable_rental_market/

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

# Make aggregate data set ----

neighbourhood_aggregate <- neighbourhood_profiles

city_aggregate <- city_profile

for (i in names(neighbourhood_aggregate)) {

  neighbourhood_aggregate[[i]][["structure_type"]] <- structure_type_by_neighbourhood[[i]]
  # neighbourhood_aggregate[[i]][["bedrooms"]] <- bedrooms_by_neighbourhood[[i]]
  neighbourhood_aggregate[[i]][["rental_supply"]] <- rental_supply_by_neighbourhood[[i]]
  neighbourhood_aggregate[[i]][["amenity_density"]] <- amenity_density_by_neighbourhood[[i]]
  neighbourhood_aggregate[[i]][["lem"]] <- lem_by_neighbourhood[[i]]
}

city_aggregate[["structure_type"]] <- structure_type_city
# city_aggregate[["bedrooms"]] <- bedrooms_city
city_aggregate[["rental_supply"]] <- rental_supply_city
city_aggregate[["amenity_density"]] <- amenity_density_city
city_aggregate[["lem"]] <- lem_city

# Save data sets ----

usethis::use_data(neighbourhood_aggregate, overwrite = TRUE)
usethis::use_data(city_aggregate, overwrite = TRUE)
