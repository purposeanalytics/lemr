# Aggregate census tracts to neighbourhoods, for variables of interest

# # People
#
# Population DONE
# Number of households DONE
# Population change DONE
# Population density DONE
# Household size DONE
# One person and 2+ people incomes DONE
# Unaffordable housing % DONE but not 100%
# Total people under poverty measure TODO ***********
# Visible minority population DONE

# # Places
#
# Private dwellings by structure (collapse single- and semi-detached) DONE
# Number of bedrooms DONE
# Renter versus Owner DONE
# Shelter Cost DONE

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

## People ---------------------------------------------------------------- -----

people <- list()

### Population -----
# Dimension: "Population, 2016"

population_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Population, 2016") %>%
  aggregate_total_by_neighbourhood()

people <- append(people, list(population = population_by_neighbourhood))

### Households -----

households_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Total - Private households by household size - 100% data") %>%
  aggregate_total_by_neighbourhood()

people <- append(people, list(households = households_by_neighbourhood))

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

# TODO: These are a bit off compared to the Toronto profiles, but... I think theirs are wrong? e.g.

census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by household size - 100% data") %>%
  filter(neighbourhood == "Danforth", dimension == "1 person") %>%
  pull(total) %>%
  sum()

# But the city's says 1185
# Every one is off by 5 or 10, in different directions

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
  select(dimension, geo_code, avg_total_income = total) %>%
  mutate(size = case_when(
    str_detect(dimension, "one-person") ~ 1,
    str_detect(dimension, "two-or-more-person") ~ 2
  )) %>%
  select(-dimension)

# Need to average out for the whole neighbourhood:
# Get the household size and multiply with average total income to get the TOTAL income for that census tract
# Then sum that across census tracts and divide by the number of households to get the average income for the neighbourhood

household_size_agg_by_ct <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by household size - 100% data") %>%
  mutate(size = ifelse(dimension == "1 person", 1, 2)) %>%
  group_by(neighbourhood, geo_code, size) %>%
  summarise(total_households = sum(total, na.rm = TRUE), .groups = "drop")

total_income_by_household_size_by_ct <- average_total_income_by_ct %>%
  left_join(household_size_agg_by_ct, by = c("geo_code", "size")) %>%
  mutate(total_income = avg_total_income * total_households) %>%
  select(neighbourhood, geo_code, size, avg_total_income, total_households, total_income)

average_total_income_by_neighbourhood <- total_income_by_household_size_by_ct %>%
  group_by(neighbourhood, size) %>%
  summarise(
    value = sum(total_income, na.rm = TRUE) / sum(total_households, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = case_when(
      size == 1 ~ "One person households",
      size == 2 ~ "Two or more person households"
    ),
    group = fct_reorder(group, size)
  ) %>%
  select(neighbourhood, group, value)

# This is not 100% right - TODO
# Danforth has 44,139 and 135,109 in the report
average_total_income_by_neighbourhood %>%
  filter(neighbourhood == "Danforth")
# Versus 43648 and 135290 here

people <- append(people, list(average_total_income = average_total_income_by_neighbourhood))

### Unaffordable housing ----
# Variable: "Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data"
# "% of tenant households spending 30% or more of its income on shelter costs"

household_tenure_by_ct <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by tenure - 25% sample data") %>%
  filter(dimension %in% c("Owner", "Renter")) %>%
  select(neighbourhood, geo_code, dimension, total)

renter_by_ct <- household_tenure_by_ct %>%
  filter(dimension == "Renter") %>%
  select(-dimension, -neighbourhood) %>%
  rename(renter = total)

unaffordable_housing_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data") %>%
  filter(dimension == "% of tenant households spending 30% or more of its income on shelter costs") %>%
  select(neighbourhood, geo_code, percent_unaffordable = total) %>%
  left_join(renter_by_ct, by = "geo_code") %>%
  mutate(number_unaffordable = round(renter * percent_unaffordable / 100)) %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(number_unaffordable, na.rm = TRUE) / sum(renter, na.rm = TRUE))

# TODO not quite right
# Danforth shows 49.6
unaffordable_housing_by_neighbourhood %>%
  filter(neighbourhood == "Danforth")
# This gives 49.9

people <- append(people, list(unaffordable_housing = unaffordable_housing_by_neighbourhood))

### Total people under poverty measure ----
# Using market basket measure

# TODO - MBM seems to be a different data source

### Visible minority -----
# Variable: "Total - Visible minority for the population in private households - 25% sample"

# Combine Chinese, Japanese, Korean into "East Asian"
# Combine "Filipino" with "Southeast Asian"

# These numbers seem a tiny bit off compared to the City's, even before collapsing - e.g. they have 60 for Korean vs 65 here
visible_minority_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Visible minority for the population in private households - 25% sample") %>%
  mutate(dimension = case_when(
    dimension %in% c("Chinese", "Japanese", "Korean") ~ "East Asian",
    dimension == "Filipino" ~ "Southeast Asian",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_by_neighbourhood() %>%
  filter(prop != 0)

people <- append(people, list(visible_minority = visible_minority_by_neighbourhood))

### Places ------- ----

places <- list()

### Private dwellings by structure -----

structure_type_clean <- tribble(
  ~original, ~clean,
  "Apartment in a building that has fewer than five storeys", "Apartment, < 5 storeys",
  "Apartment in a building that has five or more storeys", "Apartment, 5+ storeys",
  "Apartment or flat in a duplex", "Duplex",
  "Movable dwelling", "Moveable dwelling",
  "Other single-attached house", "Single- or Semi-detached house",
  "Row house", "Row house",
  "Semi-detached house", "Single- or Semi-detached house",
  "Single-detached house", "Single- or Semi-detached house"
)

structure_type_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Occupied private dwellings by structural type of dwelling - 100% data") %>%
  left_join(structure_type_clean, by = c("dimension" = "original")) %>%
  select(-dimension) %>%
  rename(dimension = clean) %>%
  aggregate_prop_by_neighbourhood() %>%
  filter(prop != 0)

places <- append(places, list(structure_type = structure_type_by_neighbourhood))

### Number of bedrooms ----

bedrooms_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Occupied private dwellings by number of bedrooms - 25% sample data") %>%
  mutate(dimension = case_when(
    dimension == "No bedrooms" ~ "0 bedrooms",
    dimension == "4 or more bedrooms" ~ "4+ bedrooms",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_by_neighbourhood() %>%
  mutate(
    size = parse_number(group),
    group = fct_reorder(group, size)
  ) %>%
  select(-size)

places <- append(places, list(bedrooms = bedrooms_by_neighbourhood))

### Renter / owner split -----
# Variable: "Total - Private households by tenure - 25% sample data"
# "Band housing" (relevant when the housing is on a First Nations reserve or settlement) is not present in Toronto.
# So limit to Owner and Renter.

household_tenure_by_neighbourhood <- household_tenure_by_ct %>%
  aggregate_prop_by_neighbourhood()

places <- append(places, list(renter_owner = household_tenure_by_neighbourhood))

### Shelter cost -----

average_renter_shelter_cost_by_ct <- census_profiles_toronto_cts %>%
  filter(dimension == "Average monthly shelter costs for rented dwellings ($)") %>%
  select(neighbourhood, geo_code, avg_shelter_cost = total)

renter_shelter_cost_by_neighbourhood <- average_renter_shelter_cost_by_ct %>%
  left_join(renter_by_ct, by = "geo_code") %>%
  mutate(total_shelter_cost = avg_shelter_cost * renter) %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(total_shelter_cost, na.rm = TRUE) / sum(renter, na.rm = TRUE))

places <- append(places, list(average_renter_shelter_cost = renter_shelter_cost_by_neighbourhood))

### Combine data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- append(people, places) %>%
  map(~ split(.x, .x$neighbourhood))

# Some of these are just a single value, so they don't need to be in a data frame
neighbourhood_profiles[["population"]] <- neighbourhood_profiles[["population"]] %>%
  map("value")
neighbourhood_profiles[["population_change"]] <- neighbourhood_profiles[["population_change"]] %>%
  map("value")
neighbourhood_profiles[["population_density"]] <- neighbourhood_profiles[["population_density"]] %>%
  map("value")
neighbourhood_profiles[["unaffordable_housing"]] <- neighbourhood_profiles[["unaffordable_housing"]] %>%
  map("value")
neighbourhood_profiles[["average_renter_shelter_cost"]] <- neighbourhood_profiles[["average_renter_shelter_cost"]] %>%
  map("value")

# Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
neighbourhood_profiles <- neighbourhood_profiles %>%
  transpose()

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
