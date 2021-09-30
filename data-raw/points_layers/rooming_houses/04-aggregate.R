# Aggregate number of rooming houses per neighbourhood and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
library(sf)
library(tidyr)
devtools::load_all()

rooming_houses_clean <- readRDS(here::here("data-raw", "points_layers", "rooming_houses", "clean", "rooming_houses.rds"))

# Count per neighbourhood, including zeros
rooming_houses_by_neighbourhood <- rooming_houses_clean %>%
  as_tibble() %>%
  count(neighbourhood, group = status, name = "value") %>%
  mutate(neighbourhood = forcats::fct_expand(neighbourhood, lemr::neighbourhoods[["neighbourhood"]])) %>%
  complete(neighbourhood, group, fill = list(value = 0))

# Save
saveRDS(rooming_houses_by_neighbourhood, here::here("data-raw", "points_layers", "rooming_houses", "aggregate", "rooming_houses_by_neighbourhood.rds"))
