# Aggregate census tracts to neighbourhoods, for variables of interest

# # People
#
# Population DONE
# Number of households
# Population change DONE
# Population density DONE
# Household size DONE
# One person and 2+ people incomes DONE but not 100%
# Unaffordable housing % DONE but not 100%
# Total people under poverty measure
# Visible minority population DONE

# # Places
#
# Private dwellings by structure (collapse single- and semi-detached)
# Number of bedrooms DONE
# Renter versus Owner DONE
# Shelter Cost

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)

#### Read data ----
census_profiles_toronto_cts <- readRDS(here::here("data-raw", "census_profiles_2016", "clean", "census_profiles_toronto_cts.rds"))

#### Function for keeping more detailed dimension dimensions -----

# There is so much hierarchy in some of these, so for each dimension, check if it's a parent - if so, don't keep it. If not, then it's the most detailed, and we want to keep it.

keep_most_detailed_dimension <- function(df, dimension_full_start) {
  df_dimension <- df %>%
    distinct(parent_id, dimension_full, dimension, dimension_id) %>%
    filter(str_starts(dimension_full, dimension_full_start))

  dimension_id <- df_dimension %>%
    pull(dimension_id)

  dimension_flag_parent <- map_lgl(dimension_id, ~ .x %in% df_dimension[["parent_id"]])
  names(dimension_flag_parent) <- dimension_id

  dimension_not_parent <- dimension_flag_parent[!dimension_flag_parent]
  dimension_not_parent <- names(dimension_not_parent)

  df_dimension %>%
    filter(dimension_id %in% dimension_not_parent) %>%
    select(dimension_id) %>%
    inner_join(df, by = "dimension_id")
}

## Functions for getting total and prop -----

aggregate_total_by_neighbourhood <- function(data) {
  data %>%
    group_by(neighbourhood) %>%
    summarise(value = sum(total, na.rm = TRUE))
}

aggregate_prop_by_neighbourhood <- function(data) {
  data %>%
    select(geo_code, neighbourhood, dimension, total) %>%
    group_by(neighbourhood, group = dimension) %>%
    summarise(value = sum(total, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(
      total = sum(value),
      prop = value / total
    ) %>%
    ungroup() %>%
    select(neighbourhood, group, value, prop)
}

## ----People ----- -----

people <- list()

### Population -----
# Dimension: "Population, 2016"

population_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Population, 2016") %>%
  aggregate_total_by_neighbourhood()

people <- append(people, list(population = population_by_neighbourhood))

### Households -----

# TODO

### Population change ----
# Use "Population, 2011" and compare to 2016

population_2011 <- census_profiles_toronto_cts %>%
  filter(dimension == "Population, 2011") %>%
  aggregate_total_by_neighbourhood()

population_change_by_neighbourhood <- population_by_neighbourhood %>%
  left_join(population_2011, by = "neighbourhood", suffix = c("_2016", "_2011")) %>%
  mutate(
    population_change = (value_2016 - value_2011) / value_2011,
    population_change = round(population_change, 3)
  ) %>%
  select(neighbourhood, value = population_change)

rm(population_2011)

people <- append(people, list(population_change = population_change_by_neighbourhood))

### Population density -----

population_density_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension %in% c("Population, 2016", "Land area in square kilometres")) %>%
  select(geo_code, neighbourhood, dimension, total) %>%
  mutate(dimension = case_when(
    dimension == "Population, 2016" ~ "population",
    dimension == "Land area in square kilometres" ~ "area"
  )) %>%
  pivot_wider(names_from = dimension, values_from = total) %>%
  group_by(neighbourhood) %>%
  summarize(across(c(population, area), sum, na.rm = TRUE)) %>%
  mutate(population_density = population / area) %>%
  select(neighbourhood, value = population_density)

people <- append(people, list(population_density = population_density_by_neighbourhood))

### Household size ----
# Variable: "Total - Private households by household size - 100% data"

household_size_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by household size - 100% data") %>%
  aggregate_prop_by_neighbourhood()

# Ensure ordering of dimensions
household_size_by_neighbourhood <- household_size_by_neighbourhood %>%
  mutate(
    dimension_num = parse_number(group),
    group = fct_reorder(group, dimension_num),
    group = fct_rev(group)
  )

people <- append(people, list(household_size = household_size_by_neighbourhood))

### Average one and two+ person incomes ----
# Variable "Total - Income statistics in 2015 for private households by household size - 25% sample data"
# And narrow in on average total income for one and two+ person households

average_total_income_by_ct <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Income statistics in 2015 for private households by household size - 25% sample data") %>%
  filter(dimension %in% c("Average total income of one-person households in 2015 ($)", "Average total income of two-or-more-person households in 2015 ($)")) %>%
  select(dimension, geo_code, neighbourhood, total) %>%
  mutate(size = case_when(
    str_detect(dimension, "one-person") ~ 1,
    str_detect(dimension, "two-or-more-person") ~ 2
  ))

# Need to average out for the whole neighbourhood, so get the household size, multiple out, then average across the neighbourhood

household_size_agg_by_ct <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by household size - 100% data") %>%
  select(geo_code, dimension, total) %>%
  mutate(size = ifelse(dimension == "1 person", 1, 2)) %>%
  group_by(geo_code, size) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

# This is close, but not quite right - I'll come back to it. TODO
average_total_income_by_neighbourhood <- average_total_income_by_ct %>%
  left_join(household_size_agg_by_ct, by = c("geo_code", "size"), suffix = c("_income", "_household_size")) %>%
  mutate(income_all_households = total_income * total_household_size) %>%
  group_by(neighbourhood, group = dimension) %>%
  summarise(
    households = sum(total_household_size),
    income = sum(income_all_households),
    mean_income = income / households,
    .groups = "drop"
  ) %>%
  select(neighbourhood, group, value = mean_income) %>%
  mutate(group = ifelse(str_detect(group, "one-person"), "One person households", "Two or more person households"),
         group = fct_relevel(group, "One person households", "Two or more person households"))

rm(average_total_income_by_ct, household_size_agg_by_ct)

people <- append(people, list(average_total_income = average_total_income_by_neighbourhood))

### Unaffordable housing ----
# Variable: "Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data"
# "% of tenant households spending 30% or more of its income on shelter costs"

household_tenure_by_ct <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by tenure - 25% sample data") %>%
  filter(dimension %in% c("Owner", "Renter")) %>%
  select(geo_code, neighbourhood, dimension, total)

# TODO: close but not quite right
unaffordable_housing_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data") %>%
  filter(dimension == "% of tenant households spending 30% or more of its income on shelter costs") %>%
  select(neighbourhood, geo_code, total) %>%
  left_join(household_tenure_by_ct %>%
    filter(dimension == "Renter"), by = c("neighbourhood", "geo_code"), suffix = c("_percent_unaffordable", "_renters")) %>%
  mutate(number_unaffordable = round(total_renters * total_percent_unaffordable / 100)) %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(number_unaffordable, na.rm = TRUE) / sum(total_renters, na.rm = TRUE))

people <- append(people, list(unaffordable_housing = unaffordable_housing_by_neighbourhood))

### Total people under poverty measure ----
# Using market basket measure

# TODO - MBM seems to be a different data source

### Visible minority -----
# Variable: "Total - Visible minority for the population in private households - 25% sample"

visible_minority_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Visible minority for the population in private households - 25% sample") %>%
  aggregate_prop_by_neighbourhood()

people <- append(people, list(visible_minority = visible_minority_by_neighbourhood))

### Places ------- ----

places <- list()

### Private dwellings by structure -----

# TODO: aggregate some of these, e.g. "house"

structure_type_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Occupied private dwellings by structural type of dwelling - 100% data") %>%
  aggregate_prop_by_neighbourhood()

places <- append(places, list(structure_type = structure_type_by_neighbourhood))

### Number of bedrooms ----

bedrooms_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Occupied private dwellings by number of bedrooms - 25% sample data") %>%
  aggregate_prop_by_neighbourhood()

places <- append(places, list(bedrooms = bedrooms_by_neighbourhood))

### Renter / owner split -----
# Variable: "Total - Private households by tenure - 25% sample data"
# "Band housing" (relevant when the housing is on a First Nations reserve or settlement) is not present in Toronto.
# So limit to Owner and Renter.

household_tenure_by_neighbourhood <- household_tenure_by_ct %>%
  aggregate_prop_by_neighbourhood()

places <- append(places, list(renter_owner = household_tenure_by_neighbourhood))

### Shelter cost -----

### Combine data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- append(people, places) %>%
  map( ~ split(.x, .x$neighbourhood)) %>%
  # Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
  transpose()

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
