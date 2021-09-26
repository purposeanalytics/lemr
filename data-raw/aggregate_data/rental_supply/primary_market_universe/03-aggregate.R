# Aggregate census tracts to neighbourhoods

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)

#### Read data ----
primary_market_cts <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "clean", "primary_market_universe.rds"))

vacancy_rate_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "interpolate", "vacancy_rate_2016.rds"))

primary_market_cts <- primary_market_cts %>%
  pivot_longer(-c(neighbourhood, ct), names_to = "group", names_prefix = "total_")

# Use vacancy rate to lower numbers ----
primary_market_cts <- primary_market_cts %>%
  left_join(vacancy_rate_2016, by = c("ct", "neighbourhood")) %>%
  mutate(value = value * (1 - vacancy_rate))

## Aggregate primary market with breakdown ----

# Not calculating props yet, since those should just be out of the full market, not of the primary market

primary_market_by_neighbourhood <- primary_market_cts %>%
  group_by(neighbourhood, group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    value = round(value),
    .groups = "drop"
  )

primary_market_city <- primary_market_by_neighbourhood %>%
  group_by(group) %>%
  summarise(value = sum(value))

# Save aggregates ----
saveRDS(primary_market_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_by_neighbourhood.rds"))
saveRDS(primary_market_city, here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "aggregate", "primary_market_city.rds"))
