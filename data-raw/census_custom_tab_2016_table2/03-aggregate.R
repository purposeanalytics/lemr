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
custom_tab_toronto_cts <- readRDS(here::here("data-raw", "census_custom_tab_2016_table2", "clean", "custom_tab_toronto_table2.rds"))

neighbourhood <- list()
city <- list()

## Functions for getting total and prop -----

aggregate_total_city <- function(df) {
  df %>%
    summarise(value = sum(total, na.rm = TRUE))
}

aggregate_total_by_neighbourhood <- function(df) {
  df %>%
    group_by(neighbourhood) %>%
    summarise(value = sum(total, na.rm = TRUE))
}

# When we calculate proportion we can't just sum the totals - we need to use the parent dimension because of rounding / non-response
aggregate_prop_by_neighbourhood <- function(df, column_name, parent_dimension) {
  df_children <- df %>%
    filter(!!sym(column_name) != parent_dimension)

  df_children_summary <- df_children %>%
    group_by(neighbourhood, group = !!sym(column_name)) %>%
    summarise(value = sum(total, na.rm = TRUE), .groups = "drop")

  df_parent <- df %>%
    filter(!!sym(column_name) == parent_dimension)

  df_parent_summary <- df_parent %>%
    aggregate_total_by_neighbourhood() %>%
    rename(total = value)

  df_children_summary %>%
    left_join(df_parent_summary, by = "neighbourhood") %>%
    mutate(prop = value / total) %>%
    select(neighbourhood, group, value, prop) %>%
    complete(neighbourhood, group, fill = list(value = 0, prop = 0))
}

aggregate_prop_city <- function(df, column_name, parent_dimension) {
  df_children <- df %>%
    filter(!!sym(column_name) != parent_dimension)

  df_children_summary <- df_children %>%
    group_by(group = !!sym(column_name)) %>%
    summarise(value = sum(total, na.rm = TRUE), .groups = "drop")

  df_parent <- df %>%
    filter(!!sym(column_name) == parent_dimension)

  parent_summary <- df_parent %>%
    summarise(value = sum(total, na.rm = TRUE)) %>%
    pull(value)

  df_children_summary %>%
    mutate(prop = value / parent_summary) %>%
    select(group, value, prop)
}

## Rental households by number of bedrooms ---------------------------------------------------------------- -----

number_of_bedrooms_by_neighbourhood <- custom_tab_toronto_cts %>%
  filter(tenure == "Renter") %>%
  aggregate_prop_by_neighbourhood("number_of_bedrooms", "Total - Number of bedrooms")

neighbourhood <- append(neighbourhood, list(number_of_bedrooms = number_of_bedrooms_by_neighbourhood))

# City
number_of_bedrooms_city <- custom_tab_toronto_cts %>%
  filter(tenure == "Renter") %>%
  aggregate_prop_city("number_of_bedrooms", "Total - Number of bedrooms")

city <- append(city, list(number_of_bedrooms = number_of_bedrooms_city))

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
