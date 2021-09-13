# Aggregate census tracts to neighbourhoods

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)

#### Read data ----
core_housing_need_cts <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "clean", "core_housing_need.rds"))

## Core housing need ---------------------------------------------------------------------

core_housing_need_by_neighbourhood <- core_housing_need_cts %>%
  group_by(neighbourhood) %>%
  summarize(value = sum(total_in_core_housing_need, na.rm = TRUE), prop = value/sum(households_tested_for_core_housing_need, na.rm = TRUE))

core_housing_need_by_neighbourhood <- core_housing_need_by_neighbourhood %>%
  split(.$neighbourhood)

core_housing_need_city <- core_housing_need_cts %>%
  summarize(value = sum(total_in_core_housing_need, na.rm = TRUE), prop = value/sum(households_tested_for_core_housing_need, na.rm = TRUE))

# Save

saveRDS(core_housing_need_by_neighbourhood, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood.rds"))
saveRDS(core_housing_need_city, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_city.rds"))
