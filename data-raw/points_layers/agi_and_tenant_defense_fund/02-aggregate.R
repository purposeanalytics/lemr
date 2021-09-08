# Aggregate AGI and TDFs

library(dplyr)
library(purrr)
devtools::load_all()

# AGIs ----
# Unique buildings with AGIs in the last 5 years / # buildings

agi_tdf_buildings <- lemur::agi_applications_and_tdf %>%
  as_tibble() %>%
  select(bing_address, neighbourhood, tdf) %>%
  mutate(agi = TRUE) %>%
  group_by(bing_address) %>%
  mutate(tdf = any(tdf)) %>%
  ungroup() %>%
  distinct() %>%
  semi_join(lemur::apartment_building_registry, by = "bing_address")

agi_by_neighbourhood <- agi_tdf_buildings %>%
  filter(agi) %>%
  group_by(neighbourhood) %>%
  summarise(n = n_distinct(bing_address))

buildings_by_neighbourhood <- lemur::neighbourhood_profiles %>%
  transpose() %>%
  pluck("number_of_apartments") %>%
  map(as_tibble) %>%
  bind_rows(.id = "neighbourhood")

agi_by_neighbourhood <- agi_by_neighbourhood %>%
  full_join(buildings_by_neighbourhood, by = "neighbourhood") %>%
  mutate(
    n = coalesce(n, 0),
    prop = n / value
  )

agi_city <- sum(agi_by_neighbourhood[["n"]]) / sum(agi_by_neighbourhood[["value"]])

# TDFs -----
# Buildings with TDFs / buildings with AGIs

tdf_by_neighbourhood <- agi_tdf_buildings %>%
  filter(tdf) %>%
  group_by(neighbourhood) %>%
  summarise(n = n_distinct(bing_address))

tdf_by_neighbourhood <- tdf_by_neighbourhood %>%
  full_join(agi_by_neighbourhood, by = "neighbourhood", suffix = c("_tdf", "_agi")) %>%
  mutate(
    n_tdf = coalesce(n_tdf, 0),
    prop = ifelse(n_agi == 0, NA_real_, n_tdf / n_agi)
  ) %>%
  select(-value) %>%
  rename(n = n_tdf)

tdf_city <- sum(tdf_by_neighbourhood[["n"]], na.rm = TRUE) / sum(tdf_by_neighbourhood[["n_agi"]], na.rm = TRUE)

# Add to profiles ----

agi_by_neighbourhood_n <- agi_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map(pull, n)

agi_by_neighbourhood_prop <- agi_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map(pull, prop)

tdf_by_neighbourhood_n <- tdf_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map(pull, n)

tdf_by_neighbourhood_prop <- tdf_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map(pull, prop)

for (i in names(neighbourhood_profiles)) {
  neighbourhood_profiles[[i]][["agi_n"]] <- agi_by_neighbourhood_n[[i]]
  neighbourhood_profiles[[i]][["agi_prop"]] <- agi_by_neighbourhood_prop[[i]]
  neighbourhood_profiles[[i]][["tdf_n"]] <- tdf_by_neighbourhood_n[[i]]
  neighbourhood_profiles[[i]][["tdf_prop"]] <- tdf_by_neighbourhood_prop[[i]]
}

city_profile[["agi_n"]] <- sum(agi_by_neighbourhood[["n"]])
city_profile[["agi_prop"]] <- agi_city
city_profile[["tdf_n"]] <- sum(tdf_by_neighbourhood[["n"]])
city_profile[["tdf_prop"]] <- tdf_city

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
usethis::use_data(city_profile, overwrite = TRUE)
