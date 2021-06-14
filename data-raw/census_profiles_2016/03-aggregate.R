# Aggregate census tracts to neighbourhoods, for variables of interest

# Dimensions / variables:
# Population
# Population density
# Age pyramid
# Household size
# Household income
# Renter / owner split
# Visible minority breakdown

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

### Population -----

population_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Population, 2016") %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(total, na.rm = TRUE))

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

### Age pyramid ----

# Variable: Total - Age groups and average age of the population - 100% data

age_pyramid_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Age groups and average age of the population - 100% data") %>%
  select(ct = geo_code, neighbourhood, dimension, total, female, male) %>%
  group_by(neighbourhood, dimension) %>%
  summarise(across(c(total, male, female), sum, na.rm = TRUE), .groups = "drop")

# Calculate proportion for female / male

age_pyramid_by_neighbourhood_prop <- age_pyramid_by_neighbourhood %>%
  group_by(neighbourhood) %>%
  mutate(
    full_total = sum(total),
    across(c(total, male, female), ~ .x / full_total)
  ) %>%
  ungroup() %>%
  select(neighbourhood, dimension, total, male, female) %>%
  mutate(metric = "proportion")

# Convert to long and combine

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  pivot_longer(cols = c(total, male, female), names_to = "sex", values_to = "value") %>%
  mutate(metric = "count")

age_pyramid_by_neighbourhood_prop <- age_pyramid_by_neighbourhood_prop %>%
  pivot_longer(cols = c(total, male, female), names_to = "sex", values_to = "value")

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  bind_rows(age_pyramid_by_neighbourhood_prop) %>%
  select(neighbourhood, dimension, sex, metric, value) %>%
  arrange(neighbourhood, dimension, sex, metric, value)

rm(age_pyramid_by_neighbourhood_prop)

# Clean up and reorder dimension
# e.g. we want "0 - 4" instead of "0 to 4 years", and need to make sure that the age groups in order

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  separate(dimension, into = c("discard", "max_age"), sep = " to ", fill = "left", remove = FALSE) %>%
  separate(dimension, into = "min_age", sep = " ", extra = "drop", remove = FALSE, convert = TRUE) %>%
  select(-discard) %>%
  mutate(
    max_age = parse_number(max_age),
    age_group = case_when(
      min_age < max_age ~ glue::glue("{min_age}-{max_age}"),
      min_age == max_age & min_age == 100 ~ glue::glue("100+")
    ),
    age_group = fct_reorder(age_group, min_age)
  ) %>%
  select(-min_age, -max_age) %>%
  mutate(sex = fct_relevel(sex, "male", "female"))

### Household size ----
# Variable: "Total - Private households by household size - 100% data"

household_size_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by household size - 100% data") %>%
  select(ct = geo_code, neighbourhood, dimension, total) %>%
  group_by(neighbourhood, dimension) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(
    neighbourhood_total = sum(total),
    prop = total / neighbourhood_total
  ) %>%
  ungroup()

# Ensure ordering of dimensions
household_size_by_neighbourhood <- household_size_by_neighbourhood %>%
  mutate(
    dimension_num = parse_number(dimension),
    dimension = fct_reorder(dimension, dimension_num),
    dimension = fct_rev(dimension)
  ) %>%
  select(neighbourhood, dimension, total, prop)

### Renter / owner split -----
# Variable: "Total - Private households by tenure - 25% sample data"
# "Band housing" (relevant when the housing is on a First Nations reserve or settlement) is not present in Toronto.
# So limit to Owner and Renter.

household_tenure_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Private households by tenure - 25% sample data") %>%
  filter(dimension %in% c("Owner", "Renter")) %>%
  select(ct = geo_code, neighbourhood, dimension, total) %>%
  group_by(neighbourhood, dimension) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(
    neighbourhood_total = sum(total),
    prop = total / neighbourhood_total
  ) %>%
  ungroup()

### Visible minority -----
# Variable: "Total - Visible minority for the population in private households - 25% sample"

visible_minority_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Visible minority for the population in private households - 25% sample") %>%
  select(ct = geo_code, neighbourhood, dimension, total) %>%
  group_by(neighbourhood, dimension) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(
    neighbourhood_total = sum(total),
    prop = total / neighbourhood_total
  ) %>%
  ungroup()

### Combine data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- list(
  population = population_by_neighbourhood,
  population_density = population_density_by_neighbourhood,
  age_pyramid = age_pyramid_by_neighbourhood,
  household_size = household_size_by_neighbourhood,
  household_tenure = household_tenure_by_neighbourhood,
  visible_minority = visible_minority_by_neighbourhood
) %>%
  # Split by neighbourhood
  map(~ split(.x, .x$neighbourhood)) %>%
  # Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
  transpose()

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
