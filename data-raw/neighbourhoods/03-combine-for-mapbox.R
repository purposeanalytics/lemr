# Combine neighbourhoods data with other aggregates, to use as mapbox layer

library(sf)
library(dplyr)

devtools::load_all()

# LEM ----

lem <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_by_neighbourhood_layer.rds"))

# LEM Percent ----

lem_percent <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_by_neighbourhood_layer.rds"))

# Rental supply ----

rental_supply <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood_layer.rds"))

# Core housing need ----

core_housing_need <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood_layer.rds"))

# Eviction rate ----

eviction_rate <- readRDS(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "aggregate", "eviction_rate_by_neighbourhood_layer.rds"))

# Vacancy rate

vacancy_rate <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2020_layer.rds"))

# Combine ----
neighbourhoods <- lemr::neighbourhoods %>%
  left_join(lem, by = "neighbourhood") %>%
  left_join(lem_percent, by = "neighbourhood") %>%
  left_join(rental_supply, by = "neighbourhood") %>%
  left_join(core_housing_need, by = "neighbourhood") %>%
  left_join(vacancy_rate, by = "neighbourhood") %>%
  left_join(eviction_rate, by = "neighbourhood") %>%
  relocate(geometry, .after = eviction_rate)

# Save dataset - as geojson for mapbox
file <- here::here("data-raw", "neighbourhoods", "final", "neighbourhoods.geojson")
if (fs::file_exists(file)) {
  fs::file_delete(file)
}
st_write(neighbourhoods, here::here("data-raw", "neighbourhoods", "final", "neighbourhoods.geojson"))
