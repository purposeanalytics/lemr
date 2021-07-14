# Datasets of alt text that can easily be included in app and downloadable reports

library(dplyr)
library(purrr)
devtools::load_all()

neighbourhood_profiles_transpose <- neighbourhood_profiles %>%
  transpose()

city_profile_alt_text <- list()
neighbourhood_profiles_alt_text <- list()

# People ----

## Population change ----

population_change_values <- city_profile[["population_change_distribution"]][["value"]]

population_change_base <- glue::glue("Bar chart showing the distribution of population change from 2011 to 2016 for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} population change and the distribution is heavily skewed left with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
  min = min(population_change_values),
  max = max(population_change_values),
  skew_min = quantile(population_change_values, 0.1),
  skew_max = quantile(population_change_values, 0.9)
)

city_profile_alt_text <- append(city_profile_alt_text, list(population_change = population_change_base))

population_change_neighbourhood <- neighbourhood_profiles_transpose[["population_change"]] %>%
  tibble::enframe() %>%
  mutate(value = unlist(value)) %>%
  mutate(text = purrr::map2_chr(name, value, function(name, value) {
    glue::glue("{population_change_base} The bar containing the {name} neighbourhood's population change, {scales::percent(value, accuracy = 0.1)}, is highlighted.")
  })) %>%
  split(.$name) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(population_change = population_change_neighbourhood))

## Population density ----

population_density_values <- city_profile[["population_density_distribution"]][["value"]]

population_density_base <- glue::glue("Histogram showing the distribution of population density for each of Toronto's neighbourhoods. The values range from {round(min)} to {round(max)} people per square kilometer and the distribution is heavily skewed left with most values between {round(skew_min)} and {round(skew_max)}.",
  min = min(population_density_values),
  max = max(population_density_values),
  skew_min = quantile(population_density_values, 0.1),
  skew_max = quantile(population_density_values, 0.9)
)

city_profile_alt_text <- append(city_profile_alt_text, list(population_density = population_density_base))

population_density_neighbourhood <- neighbourhood_profiles_transpose[["population_density"]] %>%
  tibble::enframe() %>%
  mutate(value = unlist(value)) %>%
  mutate(text = purrr::map2_chr(name, value, function(name, value) {
    glue::glue("{population_density_base} The bar containing the {name} neighbourhood's population density, {round(value)}, is highlighted.")
  })) %>%
  split(.$name) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(population_density = population_density_neighbourhood))

## Household size ----

household_size_breakdown <- glue::glue("{group} {scales::percent(prop, accuracy = 0.1)}",
  group = city_profile[["household_size"]][["group"]],
  prop = city_profile[["household_size"]][["prop"]]
)

household_size_breakdown <- glue::glue_collapse(household_size_breakdown, sep = " ", last = " and ")

household_size_base <- glue::glue("Bar chart showing the distribution of household sizes for all households in Toronto. The values are: {household_size_breakdown}.")

city_profile_alt_text <- append(city_profile_alt_text, list(household_size = household_size_base))

household_size_neighbourhood <- neighbourhood_profiles_transpose[["household_size"]] %>%
  bind_rows() %>%
  left_join(city_profile[["household_size"]], by = "group", suffix = c("_neighbourhood", "_city")) %>%
  mutate(text = pmap_chr(list(group, prop_neighbourhood, prop_city), function(group, prop_neighbourhood, prop_city) {
    glue::glue("{group} {scales::percent(prop_neighbourhood, accuracy = 0.1)} versus {scales::percent(prop_city, accuracy = .1)}")
  })) %>%
  group_by(neighbourhood) %>%
  summarise(
    text = glue::glue_collapse(text, sep = " ", last = " and ")
  ) %>%
  distinct() %>%
  mutate(
    text = glue::glue("Bar chart comparing the distribution of household sizes for households in {neighbourhood} versus all households in Toronto. The values are ({neighbourhood} versus Toronto): {text}.")
  ) %>%
  split(.$neighbourhood) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(household_size = household_size_neighbourhood))

## Mean total household income ----

average_total_income <- glue::glue("{group} {scales::dollar(prop, big.mark = '')}",
  group = city_profile[["average_total_income"]][["group"]],
  prop = city_profile[["average_total_income"]][["value"]]
)

average_total_income <- glue::glue_collapse(mean_total_household_income, sep = " ", last = " and ")

average_total_income_base <- glue::glue("Bar chart comparing the total household income for 1 person versus 2+ person households in Toronto. The values are {average_total_income}.")

city_profile_alt_text <- append(city_profile_alt_text, list(average_total_income = average_total_income_base))

average_total_income_neighbourhood <- neighbourhood_profiles_transpose[["average_total_income"]] %>%
  bind_rows() %>%
  left_join(city_profile[["average_total_income"]], by = "group", suffix = c("_neighbourhood", "_city")) %>%
  mutate(text = pmap_chr(list(group, value_neighbourhood, value_city), function(group, value_neighbourhood, value_city) {
    glue::glue("{group} {scales::dollar(value_neighbourhood, accuracy = 1, big.mark = '')} versus {scales::dollar(value_city, accuracy = 1, big.mark = '')}")
  })) %>%
  group_by(neighbourhood) %>%
  summarise(
    text = glue::glue_collapse(text, sep = " ", last = " and ")
  ) %>%
  distinct() %>%
  mutate(
    text = glue::glue("Bar chart comparing household income for 1 person and 2+ person households, for households in {neighbourhood} versus all households in Toronto. The values are ({neighbourhood} versus Toronto): {text}.")
  ) %>%
  split(.$neighbourhood) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(average_total_income = average_total_income_neighbourhood))

## Unaffordable housing ----

unaffordable_housing_values <- city_profile[["unaffordable_housing_distribution"]][["value"]]

unaffordable_housing_base <- glue::glue("Histogram showing the distribution of percent of tenant households with unaffordable housing for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} of tenant households with unaffordable housing with most values between {scales::percent(skew_min)} and {scales::percent(skew_max)}.",
  min = min(unaffordable_housing_values),
  max = max(unaffordable_housing_values),
  skew_min = quantile(unaffordable_housing_values, 0.1),
  skew_max = quantile(unaffordable_housing_values, 0.9)
)

city_profile_alt_text <- append(city_profile_alt_text, list(unaffordable_housing = unaffordable_housing_base))

unaffordable_housing_neighbourhood <- neighbourhood_profiles_transpose[["unaffordable_housing"]] %>%
  tibble::enframe() %>%
  mutate(value = unlist(value)) %>%
  mutate(text = purrr::map2_chr(name, value, function(name, value) {
    glue::glue("{unaffordable_housing_base} The bar containing the {name} neighbourhood's percent, {scales::percent(value)}, is highlighted.")
  })) %>%
  split(.$name) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(unaffordable_housing = unaffordable_housing_neighbourhood))

## LIM-AT ----

lim_at_values <- city_profile[["lim_at_distribution"]][["value"]]

lim_at_base <- glue::glue("Bar chart showing the distribution of percent of households considered to be low income in each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} low income with most values between {scales::percent(skew_min)} and {scales::percent(skew_max)}.",
  min = min(lim_at_values),
  max = max(lim_at_values),
  skew_min = quantile(lim_at_values, 0.1),
  skew_max = quantile(lim_at_values, 0.9)
)

city_profile_alt_text <- append(city_profile_alt_text, list(lim_at = lim_at_base))

lim_at_neighbourhood <- neighbourhood_profiles_transpose[["lim_at"]] %>%
  tibble::enframe() %>%
  mutate(value = unlist(value)) %>%
  mutate(text = purrr::map2_chr(name, value, function(name, value) {
    glue::glue("{lim_at_base} The bar containing the {name} neighbourhood's percent, {scales::percent(value)}, is highlighted.")
  })) %>%
  split(.$name) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(lim_at = lim_at_neighbourhood))

## Visible minority population -----

city_profile[["visible_minority"]] <- city_profile[["visible_minority"]] %>%
  arrange(-prop) %>%
  mutate(group = stringr::str_replace(group, ", n.i.e.", " not included elsewhere"))

visible_minority_breakdown <- glue::glue("{group} {scales::percent(prop, accuracy = 0.1)}",
  group = city_profile[["visible_minority"]][["group"]],
  prop = city_profile[["visible_minority"]][["prop"]]
)

visible_minority_breakdown <- glue::glue_collapse(visible_minority_breakdown, sep = " ", last = " and ")

visible_minority_base <- glue::glue("Bar chart showing the sizes of visible minority populations in Toronto. The values are: {visible_minority_breakdown}.")

city_profile_alt_text <- append(city_profile_alt_text, list(visible_minority = visible_minority_base))

visible_minority_neighbourhood <- neighbourhood_profiles_transpose[["visible_minority"]] %>%
  bind_rows()  %>%
  mutate(group = stringr::str_replace(group, ", n.i.e.", " not included elsewhere")) %>%
  left_join(city_profile[["visible_minority"]], by = "group", suffix = c("_neighbourhood", "_city")) %>%
  arrange(neighbourhood, -prop_neighbourhood)%>%
  mutate(text = pmap_chr(list(group, prop_neighbourhood, prop_city), function(group, prop_neighbourhood, prop_city) {
    glue::glue("{group} {scales::percent(prop_neighbourhood, accuracy = 0.1)} versus {scales::percent(prop_city, accuracy = .1)}")
  })) %>%
  group_by(neighbourhood) %>%
  summarise(
    text = glue::glue_collapse(text, sep = " ", last = " and ")
  ) %>%
  distinct() %>%
  mutate(
    text = glue::glue("Bar chart comparing the visible minority populations in {neighbourhood} versus in Toronto. The values are ({neighbourhood} versus Toronto): {text}.")
  ) %>%
  split(.$neighbourhood) %>%
  map(~ pull(.x, text))

neighbourhood_profiles_alt_text <- append(neighbourhood_profiles_alt_text, list(visible_minority = visible_minority_neighbourhood))

# Places -----

## Structure type -----

## Number of bedrooms -----

## Households by tenure -----

## Average shelter cost for renters ----


# Save ----

# Transpose neighbourhood values, so there's one element per neighbourhood

neighbourhood_profiles_alt_text <- neighbourhood_profiles_alt_text %>%
  transpose()
