# Flag total_rentals as affordable, combine with neighbourhoods

library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
devtools::load_all()

rental_data <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "model", "rental_data.rds"))

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
  left_join(neighbourhoods) %>%
  st_sf()

saveRDS(affordable_by_neighbourhood_and_bedrooms, here::here("data-raw", "aggregate_data", "affordable_rental_market", "clean", "affordable_by_neighbourhood_and_bedrooms.rds"))

total_affordable_by_neighbourhood <- affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable %in% c("Deeply", "Very")) %>%
  group_by(neighbourhood) %>%
  summarise(n = sum(n)) %>%
  as_tibble()

# Add colour in
min <- 25
max <- max(total_affordable_by_neighbourhood[["n"]])
colors <- low_high_legend_colors()

total_affordable_by_neighbourhood <- total_affordable_by_neighbourhood %>%
  mutate(
    lem = cut(n, breaks = seq(min - 1, max, length.out = length(colors)), labels = FALSE),
    lem = coalesce(lem, 0)
  ) %>%
  select(neighbourhood, lem)

saveRDS(total_affordable_by_neighbourhood, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_by_neighbourhood_layer.rds"))
