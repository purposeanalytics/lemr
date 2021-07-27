# Amenity density only - breakdown by neighbourhood

library(dplyr)
library(lemur)
library(tidyr)

amenity_density <- proximity_measures %>%
  select(dbuid, population, amenity_dense, neighbourhood)

amenity_density_by_neighbourhood <- amenity_density %>%
  group_by(neighbourhood, amenity_dense) %>%
  summarise(population = sum(population), .groups = "drop_last") %>%
  mutate(proportion = population / sum(population)) %>%
  ungroup() %>%
  complete(neighbourhood, amenity_dense, fill = list(proportion = 0)) %>%
  select(neighbourhood, amenity_dense, proportion)

usethis::use_data(amenity_density_by_neighbourhood, overwrite = TRUE)
