# Aggregate census tracts to neighbourhoods
# Create multiple datasets:
# 1. Estimate of condo rental market (primary market and non-condo secondary market to be estimated by difference from Rental Market Survey)
# 2. Structural type by rental tenure

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)

#### Read data ----
core_housing_need_cts <- readRDS(here::here("data-raw", "core_housing_need", "clean", "core_housing_need.rds"))

neighbourhood <- list()
city <- list()

## Core housing need ---------------------------------------------------------------------

core_housing_need_by_neighbourhood <- core_housing_need_cts %>%
  group_by(neighbourhood) %>%
  summarize(value = sum(total_in_core_housing_need, na.rm = TRUE), prop = value/sum(households_tested_for_core_housing_need, na.rm = TRUE))

neighbourhood <- append(neighbourhood, list(core_housing_need = core_housing_need_by_neighbourhood))

# City
core_housing_need_city <- core_housing_need_cts %>%
  summarize(value = sum(total_in_core_housing_need, na.rm = TRUE), prop = value/sum(households_tested_for_core_housing_need, na.rm = TRUE))

city <- append(city, list(core_housing_need = core_housing_need_city))

### Restructure data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- neighbourhood %>%
  map(~ split(.x, .x$neighbourhood))

# Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
neighbourhood_profiles <- neighbourhood_profiles %>%
  transpose()
#
# usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
#
# city_profile <- city
# usethis::use_data(city_profile, overwrite = TRUE)
