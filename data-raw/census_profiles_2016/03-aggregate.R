# Aggregate census tracts to neighbourhoods, for variables of interest
# Some of the dimensions we will be showing need comparisons to the city
# Either a city average / # or a distribution of all of the neighbourhoods

# # People
#
# Population - NO COMPARISON
# Number of households - NO COMPARISON
# Population change - MAYBE COMPARISON? DISTRIBUTION?
# Population density - DISTRIBUTION
# Household size - COMPARISON
# One person and 2+ people incomes - COMPARISON
# Unaffordable housing % - COMPARISON
# Total people under poverty measure - COMPARISON - TODO
# Visible minority population - COMPARISON

# # Places
#
# Private dwellings by structure (collapse single- and semi-detached) - COMPARISON
# Number of bedrooms - COMPARISON
# Renter versus Owner - COMPARISON and DISTRIBUTION
# Shelter Cost - COMPARISON and DISTRIBUTION

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)

#### Read data ----
census_profiles_toronto_cts <- readRDS(here::here("data-raw", "census_profiles_2016", "clean", "census_profiles_toronto_cts.rds"))

# Split off Toronto from CTs
census_profiles_toronto <- census_profiles_toronto_cts %>%
  filter(geo_code == "535")

census_profiles_toronto_cts <- census_profiles_toronto_cts %>%
  filter(geo_code != "535")

neighbourhood <- list()
city <- list()

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
aggregate_prop_by_neighbourhood <- function(df, dimension_full_start) {
  df_children <- df %>%
    keep_most_detailed_dimension(dimension_full_start)

  df_children_summary <- df_children %>%
    group_by(neighbourhood, group = dimension) %>%
    summarise(value = sum(total, na.rm = TRUE), .groups = "drop")

  df_parent <- df %>%
    filter(dimension == dimension_full_start)

  df_parent_summary <- df_parent %>%
    aggregate_total_by_neighbourhood() %>%
    rename(total = value)

  df_children_summary %>%
    left_join(df_parent_summary, by = "neighbourhood") %>%
    mutate(prop = value / total) %>%
    select(neighbourhood, group, value, prop) %>%
    complete(neighbourhood, group, fill = list(value = 0, prop = 0))
}

aggregate_prop_city <- function(df, dimension_full_start) {
  df_children <- df %>%
    keep_most_detailed_dimension(dimension_full_start) %>%
    select(group = dimension, value = total) %>%
    # Aggregate in cases where there was combination of some dimensions
    group_by(group) %>%
    summarise(value = sum(value, na.rm = TRUE))

  df_parent <- df %>%
    filter(dimension == dimension_full_start) %>%
    select(total)

  df_children %>%
    bind_cols(df_parent) %>%
    mutate(prop = value / total) %>%
    select(group, value, prop)
}

## People ---------------------------------------------------------------- -----

### Population -----
# Dimension: "Population, 2016"

population_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Population, 2016") %>%
  aggregate_total_by_neighbourhood()

neighbourhood <- append(neighbourhood, list(population = population_by_neighbourhood))

# No city comparison

### Households -----

households_by_neighbourhood <- census_profiles_toronto_cts %>%
  filter(dimension == "Total - Private households by household size - 100% data") %>%
  aggregate_total_by_neighbourhood()

neighbourhood <- append(neighbourhood, list(households = households_by_neighbourhood))

# No city comparison

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

neighbourhood <- append(neighbourhood, list(population_change = population_change_by_neighbourhood))

# Compare to city with value and distribution

population_change_city <- census_profiles_toronto %>%
  filter(dimension %in% c("Population, 2011", "Population, 2016")) %>%
  select(dimension, total) %>%
  pivot_wider(names_from = dimension, values_from = total)
population_change_city <- (population_change_city[["Population, 2016"]] - population_change_city[["Population, 2011"]]) / (population_change_city[["Population, 2011"]])

population_change_city_distribution <- population_change_by_neighbourhood["value"]

city <- append(city, list(population_change = list(
  value = population_change_city,
  distribution = population_change_city_distribution
)))

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

neighbourhood <- append(neighbourhood, list(population_density = population_density_by_neighbourhood))

# Compare to city with distribution
population_density_city_distribution <- population_density_by_neighbourhood["value"]
city <- append(city, list(population_density = list(distribution = population_density_city_distribution)))

### Household size ----
# Variable: "Total - Private households by household size - 100% data"

household_size_by_neighbourhood <- census_profiles_toronto_cts %>%
  aggregate_prop_by_neighbourhood("Total - Private households by household size - 100% data")

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
    group = fct_reorder(group, dimension_num)
  )

neighbourhood <- append(neighbourhood, list(household_size = household_size_by_neighbourhood))

# Compare to city with breakdown

household_size_city <- census_profiles_toronto %>%
  aggregate_prop_city("Total - Private households by household size - 100% data")

city <- append(city, list(household_size = household_size_city))

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

neighbourhood <- append(neighbourhood, list(average_total_income = average_total_income_by_neighbourhood))

# Compare to city with breakdown

average_total_income_city <- census_profiles_toronto %>%
  keep_most_detailed_dimension("Total - Income statistics in 2015 for private households by household size - 25% sample data") %>%
  filter(dimension %in% c("Average total income of one-person households in 2015 ($)", "Average total income of two-or-more-person households in 2015 ($)")) %>%
  select(dimension, value = total) %>%
  mutate(group = case_when(
    str_detect(dimension, "one-person") ~ "One person households",
    str_detect(dimension, "two-or-more-person") ~ "Two or more person households"
  )) %>%
  select(group, value)

city <- append(city, list(average_total_income = average_total_income_city))

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

neighbourhood <- append(neighbourhood, list(unaffordable_housing = unaffordable_housing_by_neighbourhood))

# Compare to city with value and distribution
unaffordable_housing_city <- census_profiles_toronto %>%
  filter(dimension == "% of tenant households spending 30% or more of its income on shelter costs") %>%
  pull(total)

unaffordable_housing_city_distribution <- unaffordable_housing_by_neighbourhood["value"]

city <- append(city, list(unaffordable_housing = list(value = unaffordable_housing_city, distribution = unaffordable_housing_city_distribution)))

### Total people under poverty measure ----
# Using market basket measure

# TODO - MBM seems to be a different data source

### Visible minority -----
# Variable: "Total - Visible minority for the population in private households - 25% sample"

# Combine Chinese, Japanese, Korean into "East Asian"
# Combine "Filipino" with "Southeast Asian"

# These numbers seem a tiny bit off compared to the City's, even before collapsing - e.g. they have 60 for Korean vs 65 here
visible_minority_by_neighbourhood <- census_profiles_toronto_cts %>%
  mutate(dimension = case_when(
    dimension %in% c("Chinese", "Japanese", "Korean") ~ "East Asian",
    dimension == "Filipino" ~ "Southeast Asian",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_by_neighbourhood("Total - Visible minority for the population in private households - 25% sample data")

neighbourhood <- append(neighbourhood, list(visible_minority = visible_minority_by_neighbourhood))

# Compare to city with breakdown

visible_minority_city <- census_profiles_toronto %>%
  mutate(dimension = case_when(
    dimension %in% c("Chinese", "Japanese", "Korean") ~ "East Asian",
    dimension == "Filipino" ~ "Southeast Asian",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_city("Total - Visible minority for the population in private households - 25% sample data")

city <- append(city, list(visible_minority = visible_minority_city))

### Places ------- ----

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
  left_join(structure_type_clean, by = c("dimension" = "original")) %>%
  mutate(dimension = coalesce(clean, dimension)) %>%
  aggregate_prop_by_neighbourhood("Total - Occupied private dwellings by structural type of dwelling - 100% data") %>%
  filter(prop != 0)

neighbourhood <- append(neighbourhood, list(structure_type = structure_type_by_neighbourhood))

# Compare to city with breakdown

structure_type_city <- census_profiles_toronto %>%
  left_join(structure_type_clean, by = c("dimension" = "original")) %>%
  mutate(dimension = coalesce(clean, dimension)) %>%
  aggregate_prop_city("Total - Occupied private dwellings by structural type of dwelling - 100% data") %>%
  filter(prop != 0)

city <- append(city, list(structure_type = structure_type_city))

### Number of bedrooms ----

bedrooms_by_neighbourhood <- census_profiles_toronto_cts %>%
  mutate(dimension = case_when(
    dimension == "No bedrooms" ~ "0 bedrooms",
    dimension == "4 or more bedrooms" ~ "4+ bedrooms",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_by_neighbourhood("Total - Occupied private dwellings by number of bedrooms - 25% sample data") %>%
  mutate(
    size = parse_number(group),
    group = fct_reorder(group, size)
  ) %>%
  select(-size)

neighbourhood <- append(neighbourhood, list(bedrooms = bedrooms_by_neighbourhood))

# Compare to city with breakdown

bedrooms_city <- census_profiles_toronto %>%
  mutate(dimension = case_when(
    dimension == "No bedrooms" ~ "0 bedrooms",
    dimension == "4 or more bedrooms" ~ "4+ bedrooms",
    TRUE ~ dimension
  )) %>%
  aggregate_prop_city("Total - Occupied private dwellings by number of bedrooms - 25% sample data") %>%
  mutate(
    size = parse_number(group),
    group = fct_reorder(group, size)
  ) %>%
  select(-size)

city <- append(city, list(bedrooms = bedrooms_city))

### Renter / owner split -----
# Variable: "Total - Private households by tenure - 25% sample data"
# "Band housing" (relevant when the housing is on a First Nations reserve or settlement) is not present in Toronto.
# So limit to Owner and Renter.

household_tenure_by_neighbourhood <- census_profiles_toronto_cts %>%
  aggregate_prop_by_neighbourhood("Total - Private households by tenure - 25% sample data") %>%
  filter(group %in% c("Owner", "Renter"))

neighbourhood <- append(neighbourhood, list(renter_owner = household_tenure_by_neighbourhood))

# Compare to city by breakdown

household_tenure_city <- census_profiles_toronto %>%
  aggregate_prop_city("Total - Private households by tenure - 25% sample data") %>%
  filter(group %in% c("Owner", "Renter"))

city <- append(city, list(renter_owner = household_tenure_city))

### Shelter cost -----

average_renter_shelter_cost_by_ct <- census_profiles_toronto_cts %>%
  filter(dimension == "Average monthly shelter costs for rented dwellings ($)") %>%
  select(neighbourhood, geo_code, avg_shelter_cost = total)

renter_shelter_cost_by_neighbourhood <- average_renter_shelter_cost_by_ct %>%
  left_join(renter_by_ct, by = "geo_code") %>%
  mutate(total_shelter_cost = avg_shelter_cost * renter) %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(total_shelter_cost, na.rm = TRUE) / sum(renter, na.rm = TRUE))

neighbourhood <- append(neighbourhood, list(average_renter_shelter_cost = renter_shelter_cost_by_neighbourhood))

# Compare to city by value and distribution

average_renter_shelter_cost <- census_profiles_toronto %>%
  filter(dimension == "Average monthly shelter costs for rented dwellings ($)") %>%
  pull(total)

average_renter_shelter_cost_distribution <- renter_shelter_cost_by_neighbourhood["value"]

city <- append(city, list(average_renter_shelter_cost = list(
  value = average_renter_shelter_cost,
  distribution = average_renter_shelter_cost_distribution
)))

### Restructure data sets ----
# I want to make a list, one element for each neighbourhood, then within that have one element for each variable / dimension

neighbourhood_profiles <- neighbourhood %>%
  map(~ split(.x, .x$neighbourhood))

# Some of these are just a single value, so they don't need to be in a data frame
neighbourhood_profiles[["population"]] <- neighbourhood_profiles[["population"]] %>%
  map("value")
neighbourhood_profiles[["households"]] <- neighbourhood_profiles[["households"]] %>%
  map("value")
neighbourhood_profiles[["population_change"]] <- neighbourhood_profiles[["population_change"]] %>%
  map("value")
neighbourhood_profiles[["population_density"]] <- neighbourhood_profiles[["population_density"]] %>%
  map("value")
neighbourhood_profiles[["unaffordable_housing"]] <- neighbourhood_profiles[["unaffordable_housing"]] %>%
  map("value")
neighbourhood_profiles[["average_renter_shelter_cost"]] <- neighbourhood_profiles[["average_renter_shelter_cost"]] %>%
  map("value")

# Set factor levels separately for visible minority for *each* neighbourhood, so that it goes in descending order, with visible minority n.i.e., multiple visible minorities, and not a visible minority at the end

neighbourhood_profiles[["visible_minority"]] <- neighbourhood_profiles[["visible_minority"]] %>%
  map(function(x) {
    x %>%
      mutate(
        group = fct_reorder(group, prop, .desc = TRUE),
        group = fct_relevel(group, "Visible minority, n.i.e.", "Multiple visible minorities", "Not a visible minority", after = Inf)
      )
  })

# Now there's one element per variable, and within one per neighbourhood - transpose so it's inside out!
neighbourhood_profiles <- neighbourhood_profiles %>%
  transpose()

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)

city_profile <- city
usethis::use_data(city_profile, overwrite = TRUE)
