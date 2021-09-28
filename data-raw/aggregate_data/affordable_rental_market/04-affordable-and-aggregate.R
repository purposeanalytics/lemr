# Flag total_rentals as affordable, combine with neighbourhoods

library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
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
  summarise(n = sum(n),
            log_n = log(n)) %>%
  as_tibble()

# Add colour in - use log, since values are so unevenly distributed
min <- log(25)
max <- log(max(total_affordable_by_neighbourhood[["n"]]))
colors <- low_high_legend_colors()

total_affordable_by_neighbourhood_layer <- total_affordable_by_neighbourhood %>%
  mutate(
    lem = cut(log_n, breaks = seq(min, max, length.out = length(colors)), labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, n, log_n, lem)

saveRDS(total_affordable_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_by_neighbourhood_layer.rds"))

# Get % of rental supply -----

rental_supply_total <- rental_supply %>%
  filter(!is.na(renters)) %>%
  distinct(neighbourhood, renters)

lem_percent_by_neighbourhood <- total_affordable_by_neighbourhood %>%
  left_join(rental_supply_total, by = "neighbourhood") %>%
  mutate(lem_percent = n / renters,
         log_lem_percent = log(lem_percent))

min <- lem_percent_by_neighbourhood %>%
  filter(lem_percent > 0) %>%
  pull(lem_percent) %>%
  min()

lem_percent_by_neighbourhood_layer <- lem_percent_by_neighbourhood %>%
  mutate(
    lem = cut(log_lem_percent, breaks = seq(log(min), log(1), length.out = length(colors)), include.lowest = FALSE, labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, lem_percent, log_lem_percent, lem)

usethis::use_data(lem_percent_by_neighbourhood_layer, overwrite = TRUE)

saveRDS(lem_percent_by_neighbourhood_layer, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_percent_by_neighbourhood_layer.rds"))
