# Amenity density only - by neighbourhood

library(dplyr)
library(lemur)
library(tidyr)

proximity_measures <- readRDS(here::here("data-raw", "proximity_measures", "final", "proximity_measures.rds"))

amenity_density <- proximity_measures %>%
  as_tibble() %>%
  distinct(dbuid, population, amenity_dense, neighbourhood)

# Aggregate by neighbourhood
amenity_density_by_neighbourhood <- amenity_density %>%
  group_by(neighbourhood, amenity_dense) %>%
  summarise(population = sum(population), .groups = "drop_last") %>%
  mutate(proportion = population / sum(population)) %>%
  ungroup() %>%
  complete(neighbourhood, amenity_dense, fill = list(proportion = 0)) %>%
  select(neighbourhood, amenity_dense, proportion)

usethis::use_data(amenity_density_by_neighbourhood, overwrite = TRUE)
