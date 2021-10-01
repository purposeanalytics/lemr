# Aggregate census tracts to neighbourhoods

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)
devtools::load_all()

#### Read data ----
core_housing_need_cts <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "clean", "core_housing_need.rds"))

## Core housing need ---------------------------------------------------------------------

core_housing_need_by_neighbourhood <- core_housing_need_cts %>%
  group_by(neighbourhood) %>%
  summarize(
    value = sum(total_in_core_housing_need, na.rm = TRUE),
    prop = value / sum(households_tested_for_core_housing_need, na.rm = TRUE),
    prop = round(prop, 3)
  )

core_housing_need_by_neighbourhood <- core_housing_need_by_neighbourhood %>%
  split(.$neighbourhood) %>%
  map(pull, prop)

core_housing_need_city <- core_housing_need_cts %>%
  summarize(value = sum(total_in_core_housing_need, na.rm = TRUE), prop = value / sum(households_tested_for_core_housing_need, na.rm = TRUE)) %>%
  pull(prop) %>%
  round(3)

# Save

saveRDS(core_housing_need_by_neighbourhood, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood.rds"))
saveRDS(core_housing_need_city, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_city.rds"))

# Version for mapping ----

# Add groups for colour, then make wide

core_housing_need_by_neighbourhood <- core_housing_need_by_neighbourhood %>%
  map(as_tibble) %>%
  bind_rows(.id = "neighbourhood") %>%
  rename(prop = value) %>%
  mutate(
    core_housing_need = cut(prop, seq(0, 0.60, length.out = length(low_high_legend_colors())), include.lowest = FALSE, labels = FALSE),
    core_housing_need = ifelse(prop == 0, 0, core_housing_need)
  ) %>%
  select(-prop)

saveRDS(core_housing_need_by_neighbourhood, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood_layer.rds"))
