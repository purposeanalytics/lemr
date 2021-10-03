# Aggregate census tracts to neighbourhoods

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)
library(classInt)
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
  bind_rows(.id = "neighbourhood")

# Break into groups using Jenks

# Leave white for actual 0
n <- length(low_high_legend_colors()) - 1

jenks_breaks <- core_housing_need_by_neighbourhood %>%
  filter(value > 0) %>%
  pull(value) %>%
  classIntervals(n = n, style = "jenks") %>%
  pluck("brks")

# Take -1 from the first break, since the Jenks interval includes it but cut does not, so need to update the breaks
jenks_breaks[1] <- jenks_breaks[1] - 1

core_housing_need_by_neighbourhood_layer <- core_housing_need_by_neighbourhood %>%
  mutate(
    core_housing_need = cut(value, breaks = jenks_breaks, labels = FALSE),
    core_housing_need = coalesce(core_housing_need, 0),
  ) %>%
  select(neighbourhood, core_housing_need)

saveRDS(core_housing_need_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "core_housing_need", "aggregate", "core_housing_need_by_neighbourhood_layer.rds"))
