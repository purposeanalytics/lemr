# Aggregate vacancy rates up to the neighbourhood

# TODO: get vacancy rate for each CT
# Change rental numbers based on that
# Then aggregate CT to neighbourhood vacancy rate either based on # of renters, or # primary market
# So that it's actually weighted properly

library(dplyr)

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
    summarise(vacancy_rate = weighted.mean(vacancy_rate, w = total, na.rm = TRUE))
}

vacancy_rate_2016 <- vacancy_rate_2016 %>%
  aggregate_vacancy_rate_to_neighbourhood()

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  aggregate_vacancy_rate_to_neighbourhood()

saveRDS(vacancy_rate_2016, here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2016.rds"))
saveRDS(vacancy_rate_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "aggregate", "vacancy_rate_by_neighbourhood_2020.rds"))
