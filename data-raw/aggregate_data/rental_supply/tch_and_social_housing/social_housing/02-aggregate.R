# Clean social housing data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## read social housing unit counts ----

social_housing <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "extract", "social_housing.rds"))

# attach actual neighbourhoods
social_housing <- social_housing %>%
  mutate(id = as.numeric(neighbourhood)) %>%
  select(-neighbourhood) %>%
  left_join(lemr::neighbourhoods, by = "id") %>%
  select(-id, -geometry) %>%
  relocate(neighbourhood, .before = NULL)

saveRDS(social_housing, here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "aggregate", "social_housing_by_neighbourhood.rds"))
