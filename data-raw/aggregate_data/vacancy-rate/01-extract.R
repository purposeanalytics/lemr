# Extract vacancy rate data
# Source: https://www03.cmhc-schl.gc.ca/hmip-pimh/en#Profile/1/1/Canada

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
devtools::load_all()

vacancy_rate <- read_csv("data-raw/vacancy-rate/raw/vacancy_rate_by_neighbourhood_2020.csv", skip = 2)

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
  select(neighbourhood, vacancy_rate, reliability)

# Limit to Toronto neighbourhoods

vacancy_rate %>%
  full_join(lemur::neighbourhoods %>%
    mutate(nbhd = TRUE), by = "neighbourhood") %>%
  select(-geometry) %>%
  filter(is.na(nbhd))
