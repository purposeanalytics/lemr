# Add to city / neighbourhood profiles

library(dplyr)
library(tidyr)
library(forcats)
library(janitor)
library(purrr)
devtools::load_all()

lem <- lemur::affordable_by_neighbourhood_and_bedrooms %>%
  filter(affordable %in% c("Very", "Deeply")) %>%
  mutate(Bedrooms = case_when(
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

city_profile[["lem"]] <- lem_city
usethis::use_data(city_profile, overwrite = TRUE)

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

for (i in seq_along(neighbourhood_profiles)) {
  neighbourhood_profiles[[i]][["lem"]] <- lem_neighbourhood[[i]]
}

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
