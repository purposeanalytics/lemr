# Aggregate vacancy rates up to the neighbourhood

# TODO: get vacancy rate for each CT
# Change rental numbers based on that
# Then aggregate CT to neighbourhood vacancy rate either based on # of renters, or # primary market
# So that it's actually weighted properly

library(dplyr)
library(purrr)
devtools::load_all()

dimensions_by_ct <- readRDS(here::here("data-raw", "aggregate_data", "census_profiles_2016", "clean", "census_profiles_toronto_cts.rds"))

renter_by_ct <- dimensions_by_ct %>%
  filter(dimension_full == "Total - Private households by tenure - 25% sample data::Renter", dimension == "Renter") %>%
  select(geo_code, total)

vacancy_rate_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "interpolate", "vacancy_rate_2016.rds"))

vacancy_rate_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "interpolate", "vacancy_rate_2020.rds"))

aggregate_vacancy_rate_to_neighbourhood <- function(vacancy_rate) {
  vacancy_rate %>%
    left_join(renter_by_ct, by = c("ct" = "geo_code")) %>%
    mutate(total = coalesce(total, 0)) %>%
    group_by(neighbourhood) %>%
    summarise(value = weighted.mean(vacancy_rate, w = total, na.rm = TRUE),
              value = round(value, 3)) %>%
    split(.$neighbourhood) %>%
    map(pull, value)
}

vacancy_rate_2016 <- vacancy_rate_2016 %>%
  aggregate_vacancy_rate_to_neighbourhood()

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  aggregate_vacancy_rate_to_neighbourhood()

saveRDS(vacancy_rate_2016, here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2016.rds"))
saveRDS(vacancy_rate_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2020.rds"))

# Version of 2020 for mapping ----

# Add groups for colour, then make wide

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  map(as_tibble) %>%
  bind_rows(.id = "neighbourhood") %>%
  mutate(
    group = cut(value, seq(0, 0.11, length.out = length(low_high_legend_colors())), include.lowest = FALSE, labels = FALSE),
    group = ifelse(value == 0, 0, group)
  ) %>%
  select(-value) %>%
  rename(vacancy_rate = group)

saveRDS(vacancy_rate_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2020_layer.rds"))
