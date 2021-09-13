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
primary_market_cts <- readRDS(here::here("data-raw", "primary_market_universe", "clean", "primary_market_universe.rds"))

neighbourhood <- list()
city <- list()

## Apartment rental ---------------------------------------------------------------------

apartments_by_neighbourhood <- primary_market_cts %>%
  group_by(neighbourhood) %>%
  summarize(value = sum(total_apartments, na.rm = TRUE))

neighbourhood <- append(neighbourhood, list(apartments = apartments_by_neighbourhood))

# City
apartments_city <- primary_market_cts %>%
  summarize(value = sum(total_apartments, na.rm = TRUE))

city <- append(city, list(apartments = apartments_city))


## Row house rental ---------------------------------------------------------------------

row_houses_by_neighbourhood <- primary_market_cts %>%
  group_by(neighbourhood) %>%
  summarize(value = sum(total_row_houses, na.rm = TRUE))

neighbourhood <- append(neighbourhood, list(row_houses = row_houses_by_neighbourhood))

# City
row_houses_city <- primary_market_cts %>%
  summarize(value = sum(total_row_houses, na.rm = TRUE), prop = value/sum(total_primary_rental, na.rm = TRUE))

city <- append(city, list(row_houses = row_houses_city))


## Total primary market rental ---------------------------------------------------------------------

primary_market_by_neighbourhood <- primary_market_cts %>%
  group_by(neighbourhood) %>%
  summarize(value = sum(total_primary_rental, na.rm = TRUE))

neighbourhood <- append(neighbourhood, list(primary_market = primary_market_by_neighbourhood))

# City
primary_market_city <- primary_market_cts %>%
  summarize(value = sum(total_primary_rental, na.rm = TRUE))

city <- append(city, list(primary_market = primary_market_city))

### Restructure data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- neighbourhood %>%
  map(~ split(.x, .x$neighbourhood))

# Some of these are just a single value, so they don't need to be in a data frame
neighbourhood_profiles[["apartments"]] <- neighbourhood_profiles[["apartments"]] %>%
  map("value")
neighbourhood_profiles[["row_houses"]] <- neighbourhood_profiles[["row_houses"]] %>%
  map("value")
neighbourhood_profiles[["primary_market"]] <- neighbourhood_profiles[["primary_market"]] %>%
  map("value")

# Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
neighbourhood_profiles <- neighbourhood_profiles %>%
  transpose()


#
# usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
#
# city_profile <- city
# usethis::use_data(city_profile, overwrite = TRUE)
