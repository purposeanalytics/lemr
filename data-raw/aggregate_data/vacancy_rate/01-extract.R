# Extract vacancy rate data
# Source: https://www03.cmhc-schl.gc.ca/hmip-pimh/en#Profile/1/1/Canada

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
devtools::load_all()

vacancy_rate_2020 <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "vacancy_rate_by_neighbourhood_2020.csv"), skip = 2)
vacancy_rate_2016 <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "vacancy_rate_by_neighbourhood_2016.csv"), skip = 2)

vacancy_rate_subdivision_2020 <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "vacancy_rate_subdivision_2020.csv"), skip = 2)
vacancy_rate_subdivision_2016 <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "vacancy_rate_subdivision_2016.csv"), skip = 2)

clean_vacancy_rate <- function(vacancy_rate) {
  # Remove footer rows, with empty Total, and empty columns
  vacancy_rate <- vacancy_rate %>%
    filter(!is.na(Total)) %>%
    remove_empty("cols")

  # Rename columns
  names(vacancy_rate) <- c("neighbourhood", "bachelor", "bachelor_reliability", "bedroom_1", "bedroom_1_reliability", "bedroom_2", "bedroom_2_reliability", "bedroom_3", "bedroom_3_reliability", "total", "total_reliability")

  # Convert to long and make NAs for suppressed values
  rates <- vacancy_rate %>%
    pivot_longer(-c(neighbourhood, ends_with("reliability")), values_to = "vacancy_rate") %>%
    select(-ends_with("reliability"))

  reliability <- vacancy_rate %>%
    pivot_longer(ends_with("reliability"), values_to = "reliability") %>%
    select(neighbourhood, name, reliability) %>%
    mutate(name = str_remove(name, "_reliability"))

  vacancy_rate <- rates %>%
    full_join(reliability, by = c("neighbourhood", "name")) %>%
    mutate(
      vacancy_rate = na_if(vacancy_rate, "**"),
      vacancy_rate = as.numeric(vacancy_rate) / 100
    )

  # Just keep total vacancy rate
  vacancy_rate <- vacancy_rate %>%
    filter(name == "total") %>%
    select(cmhc_neighbourhood = neighbourhood, vacancy_rate, reliability)
}

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  clean_vacancy_rate()

vacancy_rate_2016 <- vacancy_rate_2016 %>%
  clean_vacancy_rate()

vacancy_rate_subdivision_2020 <- vacancy_rate_subdivision_2020 %>%
  clean_vacancy_rate() %>%
  filter(cmhc_neighbourhood == "Toronto (C)")

vacancy_rate_subdivision_2016 <- vacancy_rate_subdivision_2016 %>%
  clean_vacancy_rate() %>%
  filter(cmhc_neighbourhood == "Toronto (C)")

saveRDS(vacancy_rate_2016, here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2016.rds"))
saveRDS(vacancy_rate_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2020.rds"))

saveRDS(vacancy_rate_subdivision_2016, here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2016.rds"))
saveRDS(vacancy_rate_subdivision_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2020.rds"))

