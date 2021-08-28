# Amenity density only - by neighbourhood
# Aggregate and add to city_profiles and neighbourhood_profiles for easier visualizing

library(dplyr)
library(lemur)
library(tidyr)
library(purrr)
library(forcats)
library(sf)

proximity_measures <- readRDS(here::here("data-raw", "proximity_measures", "final", "proximity_measures.rds"))

amenity_density <- proximity_measures %>%
  as_tibble() %>%
  distinct(dbuid, population, amenity_dense, neighbourhood) %>%
  mutate(
    amenity_dense = fct_relevel(amenity_dense, "High", "Medium", "Low", "Unknown"),
    amenity_dense = fct_rev(amenity_dense)
  ) %>%
  arrange(amenity_dense)

# Aggregate by neighbourhood
amenity_density_by_neighbourhood <- amenity_density %>%
  group_by(neighbourhood, group = amenity_dense) %>%
  summarise(population = sum(population), .groups = "drop_last") %>%
  mutate(prop = population / sum(population)) %>%
  ungroup() %>%
  complete(neighbourhood, group, fill = list(prop = 0)) %>%
  select(neighbourhood, group, prop) %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood)

for (i in seq_along(neighbourhood_profiles)) {
  neighbourhood_profiles[[i]][["amenity_density"]] <- amenity_density_by_neighbourhood[[i]]
}

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)

# Aggregate by city and add to profile
amenity_density_city <- amenity_density %>%
  group_by(group = amenity_dense) %>%
  summarise(population = sum(population), .groups = "drop_last") %>%
  mutate(prop = population / sum(population)) %>%
  select(group, prop)

city_profile[["amenity_density"]] <- amenity_density_city

usethis::use_data(city_profile, overwrite = TRUE)
