# Get "actual" vacancy rate for each neighbourhood by going from CMHC "neighbourhoods" -> CTs -> neighbourhoods

library(dplyr)
library(readr)
library(sf)
devtools::load_all()

vacancy_rate_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2016.rds"))
vacancy_rate_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2020.rds"))

vacancy_rate_toronto_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2016.rds")) %>%
  pull(vacancy_rate)

vacancy_rate_toronto_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2020.rds")) %>%
  pull(vacancy_rate)

cmhc_to_neighbourhood <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "ct_2016_to_cmhc_neighbourhood.csv"))

cmhc_to_neighbourhood <- cmhc_to_neighbourhood %>%
  mutate(ct = glue::glue("535{CTNAME}")) %>%
  select(ct, cmhc_neighbourhood = CMHC_NEIGHBOURHOOD, neighbourhood = AREA_NAME) %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

estimate_vacancy_rate <- function(vacancy_rate, city_rate) {
  vacancy_rate %>%
    inner_join(cmhc_to_neighbourhood, by = "cmhc_neighbourhood") %>%
    distinct(ct, neighbourhood, vacancy_rate) %>%
    mutate(
      estimate = is.na(vacancy_rate),
      vacancy_rate = coalesce(vacancy_rate, city_rate)
    )
}

vacancy_rate_2016 <- vacancy_rate_2016 %>%
  estimate_vacancy_rate(vacancy_rate_toronto_2016)

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  estimate_vacancy_rate(vacancy_rate_toronto_2020)

library(ggplot2)

vacancy_rate_2016 %>%
  full_join(vacancy_rate_2020, by = c("neighbourhood", "ct"), suffix = c("_2016", "_2020")) %>%
  mutate(estimate_type = case_when(estimate_2016 & !estimate_2016 ~ "2016",
                                   !estimate_2016 & estimate_2020 ~ "2020",
                                   estimate_2016 & estimate_2020 ~ "2016 and 2020",
                                   TRUE ~ "None")) %>%
  ggplot(aes(x = vacancy_rate_2016, y = vacancy_rate_2020, color = estimate_type)) +
  geom_point()

# This is not great - the 2020 estimates might be too low!
# Especially for Cabbagetown, was already 0.035 in 2016 so probably not just 0.036 in 2020...

# TODO: get vacancy rate for each CT
# Change rental numbers based on that
# Then aggregate CT to neighbourhood vacancy rate either based on # of renters, or # primary market
# So that it's actually weighted properly
