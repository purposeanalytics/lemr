# Flag total_rentals as affordable, combine with neighbourhoods

library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
devtools::load_all()

rental_data <- readRDS(here::here("data-raw", "affordable-rental-market", "model", "rental_data.rds"))

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
  st_intersection(lemur::neighbourhoods)

affordable_by_neighbourhood_and_bedrooms <- rental_data_with_neighbourhoods %>%
  as_tibble() %>%
  count(neighbourhood, affordable, bedrooms) %>%
  left_join(neighbourhoods %>% as_tibble() %>% select(neighbourhood)) %>%
  complete(neighbourhood, affordable, bedrooms, fill = list(n = 0)) %>%
  left_join(neighbourhoods) %>%
  st_sf()

usethis::use_data(affordable_by_neighbourhood_and_bedrooms, overwrite = TRUE)

total_affordable_by_neighbourhood <- affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable %in% c("Deeply", "Very")) %>%
  group_by(neighbourhood) %>%
  summarise(n = sum(n))

# Add colour in

colors <- c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")
color_groups <- c("0", cut(1:100, breaks = seq(1, 100, length.out = length(colors))) %>% levels())

colors <- tibble(colour = colors, color_group = color_groups)

total_affordable_by_neighbourhood <- total_affordable_by_neighbourhood %>%
  mutate(
    color_group = cut(n, breaks = seq(1, 100, length.out = nrow(colors))),
    color_group = coalesce(color_group, "0")
  ) %>%
  left_join(colors, by = "color_group") %>%
  select(neighbourhood, n, colour, geometry)

usethis::use_data(total_affordable_by_neighbourhood, overwrite = TRUE)
