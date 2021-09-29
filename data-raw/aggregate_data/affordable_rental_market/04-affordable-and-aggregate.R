# Flag total_rentals as affordable, combine with neighbourhoods

library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(classInt)
library(purrr)
devtools::load_all()

rental_data <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "model", "rental_data.rds"))

rental_supply <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "aggregate", "rental_supply_by_neighbourhood.rds"))

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
  ) %>%
  left_join(neighbourhoods)

saveRDS(affordable_by_neighbourhood_and_bedrooms, here::here("data-raw", "aggregate_data", "affordable_rental_market", "clean", "affordable_by_neighbourhood_and_bedrooms.rds"))

# Make layer ----

total_affordable_by_neighbourhood <- affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable %in% c("Deeply", "Very")) %>%
  group_by(neighbourhood) %>%
  summarise(n = sum(n)) %>%
  as_tibble()

# Break into groups using Jenks
# Leave white for actual 0

n <- length(low_high_legend_colors()) - 1

jenks_breaks <- total_affordable_by_neighbourhood %>%
  filter(n > 0) %>%
  pull(n) %>%
  classIntervals(n = n, style = "jenks") %>%
  pluck("brks")

# Take -1 from the first break, since the Jenks interval includes it but cut does not, so need to update the breaks
jenks_breaks[1] <- jenks_breaks[1] - 1

total_affordable_by_neighbourhood_layer <- total_affordable_by_neighbourhood %>%
  mutate(
    lem_label = cut(n, breaks = jenks_breaks),
    lem_label = coalesce(lem_label, "0"),
    lem = cut(n, breaks = jenks_breaks, labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, n, lem, lem_label)

saveRDS(total_affordable_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_by_neighbourhood_layer.rds"))

# Get % of rental supply -----

rental_supply_total <- rental_supply %>%
  filter(!is.na(renters)) %>%
  distinct(neighbourhood, renters)

lem_percent_by_neighbourhood <- total_affordable_by_neighbourhood %>%
  left_join(rental_supply_total, by = "neighbourhood") %>%
  mutate(lem_percent = n / renters)

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

usethis::use_data(lem_percent_by_neighbourhood_layer, overwrite = TRUE)

saveRDS(lem_percent_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_by_neighbourhood_layer.rds"))
