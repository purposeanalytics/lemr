# Flag total_rentals as affordable, combine with neighbourhoods

library(dplyr)
library(sf)
library(tidyr)
library(forcats)
library(ggplot2)
library(classInt)
library(purrr)
devtools::load_all()

rental_data <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "model", "rental_data.rds"))

rental_supply <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))

rental_supply_by_neighbourhood <- rental_supply %>%
  filter(!is.na(renters)) %>%
  distinct(neighbourhood, renters)

rental_data <- rental_data %>%
  mutate(affordable = case_when(
    bedrooms == "0" & total_rent <= 385 ~ "Deeply",
    bedrooms == "0" & total_rent <= 812 ~ "Very",
    bedrooms %in% "1" & total_rent <= 495 ~ "Deeply",
    bedrooms %in% "1" & total_rent <= 1090 ~ "Very",
    bedrooms %in% "2" & total_rent <= 929 ~ "Deeply",
    bedrooms %in% "2" & total_rent <= 1661 ~ "Very",
    bedrooms == "3+" & total_rent <= 1046 ~ "Deeply",
    bedrooms == "3+" & total_rent <= 1858 ~ "Very",
    TRUE ~ "Not affordable"
  ))

# Attach neighbourhood and aggregate
rental_data <- rental_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

rental_data_with_neighbourhoods <- rental_data %>%
  st_join(lemr::neighbourhoods)

affordable_by_neighbourhood_and_bedrooms <- rental_data_with_neighbourhoods %>%
  as_tibble() %>%
  count(neighbourhood, affordable, bedrooms) %>%
  left_join(neighbourhoods %>% as_tibble() %>% select(neighbourhood)) %>%
  complete(neighbourhood, affordable, bedrooms, fill = list(n = 0))

# Multiply values by 15, round to nearest 25
affordable_by_neighbourhood_and_bedrooms <- affordable_by_neighbourhood_and_bedrooms %>%
  mutate(
    n = n * 15,
    n = plyr::round_any(n, 25)
  )

# Clean up grouping and bedrooms
affordable_by_neighbourhood_and_bedrooms <- affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable != "Not affordable") %>%
  mutate(
    bedrooms = case_when(
      bedrooms == "0" ~ "Bachelor",
      bedrooms == 1 ~ "1 bedroom",
      bedrooms %in% c("2", "3+") ~ paste(bedrooms, "bedrooms")
    ),
    bedrooms = fct_relevel(bedrooms, "Bachelor", "1 bedroom", "2 bedrooms", "3+ bedrooms"),
    affordable = glue::glue("{affordable} Affordable")
  )

# Save aggregates -----

# Breakdown
saveRDS(affordable_by_neighbourhood_and_bedrooms, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_neighbourhood_breakdown.rds"))

lem_city_breakdown <- affordable_by_neighbourhood_and_bedrooms %>%
  group_by(affordable, bedrooms) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )

saveRDS(lem_city_breakdown, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_city_breakdown.rds"))

# Total LEM, as a %

lem_by_neighbourhood <- affordable_by_neighbourhood_and_bedrooms %>%
  group_by(neighbourhood, group = affordable) %>%
  summarise(
    value = sum(n),
    .groups = "drop"
  )

lem_city <- lem_by_neighbourhood %>%
  group_by(group) %>%
  summarise(value = sum(value))

lem_percent_by_neighbourhood <- lem_by_neighbourhood %>%
  left_join(rental_supply_by_neighbourhood, by = "neighbourhood") %>%
  mutate(prop = round(value / renters, 3)) %>%
  select(neighbourhood, group, prop)

lem_percent_city <- lem_city %>%
  bind_cols(rental_supply_by_neighbourhood %>%
    summarise(renters = sum(renters))) %>%
  mutate(prop = round(value / renters, 3)) %>%
  select(group, prop)

saveRDS(lem_percent_by_neighbourhood, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_by_neighbourhood.rds"))
saveRDS(lem_percent_city, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_city.rds"))

# Make layer ----

total_affordable_by_neighbourhood <- lem_by_neighbourhood %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(value))

# Break into groups using Jenks
# Leave white for actual 0

n <- length(low_high_legend_colors()) - 1

jenks_breaks <- total_affordable_by_neighbourhood %>%
  filter(value > 0) %>%
  pull(value) %>%
  classIntervals(n = n, style = "jenks") %>%
  pluck("brks")

# Take -1 from the first break, since the Jenks interval includes it but cut does not, so need to update the breaks
jenks_breaks[1] <- jenks_breaks[1] - 1

total_affordable_by_neighbourhood_layer <- total_affordable_by_neighbourhood %>%
  mutate(
    lem_label = cut(value, breaks = jenks_breaks),
    lem_label = coalesce(lem_label, "0"),
    lem = cut(value, breaks = jenks_breaks, labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, value, lem, lem_label)

saveRDS(total_affordable_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_by_neighbourhood_layer.rds"))

# Get % of rental supply -----

lem_percent_by_neighbourhood <- total_affordable_by_neighbourhood %>%
  left_join(rental_supply_by_neighbourhood, by = "neighbourhood") %>%
  mutate(lem_percent = value / renters)

# Break into groups using Jenks
# Leave white for actual 0

jenks_breaks <- lem_percent_by_neighbourhood %>%
  filter(lem_percent > 0) %>%
  pull(lem_percent) %>%
  classIntervals(n = n, style = "jenks") %>%
  pluck("brks")

# Take a small negative from the first break, since the Jenks interval includes it but cut does not, so need to update the breaks
jenks_breaks[1] <- jenks_breaks[1] - .0001

lem_percent_by_neighbourhood_layer <- lem_percent_by_neighbourhood %>%
  mutate(
    lem_label = cut(lem_percent, breaks = jenks_breaks),
    lem_label = coalesce(lem_label, "0"),
    lem = cut(lem_percent, breaks = jenks_breaks, labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, lem_percent = lem, lem_percent_label = lem_label)

saveRDS(lem_percent_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_by_neighbourhood_layer.rds"))
