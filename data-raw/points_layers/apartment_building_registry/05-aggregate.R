# Aggregate number of apartments and units per neighbourhood and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

apartment_building_registry <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "clean", "apartment_building_registry.rds"))

apartments_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  count(neighbourhood, name = "value")

units_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(units, na.rm = TRUE))

# Not all neighbourhoods have apartments, so need to complete the data set

neighbourhoods <- tibble(neighbourhood = lemur::neighbourhoods[["neighbourhood"]])

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood") %>%
  mutate(value = coalesce(value, 0))

units_by_neighbourhood <- units_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood") %>%
  mutate(value = coalesce(value, 0))

# Add to city profile

number_of_apartments_city <- apartments_by_neighbourhood %>%
  pull(value) %>%
  sum()

number_of_units_city <- units_by_neighbourhood %>%
  pull(value) %>%
  sum()

number_of_apartments_distribution <- apartments_by_neighbourhood %>%
  select(value)

units_by_neighbourhood_distribution <- units_by_neighbourhood %>%
  select(value)

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

units_by_neighbourhood <- units_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

saveRDS(number_of_apartments_city, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_city.rds"))
saveRDS(number_of_units_city, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_units_city.rds"))
saveRDS(number_of_apartments_distribution, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "number_of_apartments_distribution.rds"))
saveRDS(units_by_neighbourhood_distribution, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood_distribution.rds"))
saveRDS(apartments_by_neighbourhood, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "apartments_by_neighbourhood.rds"))
saveRDS(units_by_neighbourhood, here::here("data-raw", "points_layers", "apartment_building_registry", "aggregate", "units_by_neighbourhood.rds"))
