# Add to city / neighbourhood profiles

library(dplyr)
library(tidyr)
library(forcats)
library(janitor)
library(purrr)
devtools::load_all()

affordable_by_neighbourhood_and_bedrooms <- readRDS(here::here("data-raw", "aggregate_data", "affordable_rental_market", "clean", "affordable_by_neighbourhood_and_bedrooms.rds"))

lem <- affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable %in% c("Very", "Deeply")) %>%
  mutate(
    Bedrooms = case_when(
      bedrooms == "0" ~ "Bachelor",
      bedrooms == 1 ~ "1 bedroom",
      bedrooms %in% c("2", "3+") ~ paste(bedrooms, "bedrooms")
    ),
    Bedrooms = fct_relevel(Bedrooms, "Bachelor", "1 bedroom", "2 bedrooms", "3+ bedrooms"),
    affordable = glue::glue("{affordable} Affordable")
  ) %>%
  as_tibble()

lem_city <- lem %>%
  group_by(Bedrooms, affordable) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = affordable, values_from = n) %>%
  adorn_totals(where = c("row", "col"))

lem_neighbourhood <- lem %>%
  group_by(neighbourhood, Bedrooms, affordable) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = affordable, values_from = n)

lem_neighbourhood <- lem_neighbourhood %>%
  split(.$neighbourhood) %>%
  map(function(x) {
    x %>%
      select(-neighbourhood) %>%
      adorn_totals(where = c("row", "col"))
  })

saveRDS(lem_city, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_city_breakdown.rds"))
saveRDS(lem_neighbourhood, here::here("data-raw", "aggregate_data", "affordable_rental_market", "aggregate", "lem_neighbourhood_breakdown.rds"))
