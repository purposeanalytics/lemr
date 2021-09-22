# Prep aggregate data for download

library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(stringr)
library(sf)
library(readr)
devtools::load_all()

city <- lemur::city_aggregate
neighbourhoods <- lemur::neighbourhood_aggregate

# Flattern neighbourhoods data ----

neighbourhoods <- neighbourhoods %>%
  transpose() %>%
  map_depth(.depth = 2, as_tibble) %>%
  map(bind_rows, .id = "neighbourhood")

# Select city elements by the same name, and combine with neighbourhoods ----

city <- city[names(neighbourhoods)]

## If single values, convert to tibble with neighbourhood = "City of Toronto"

city <- city %>%
  map(function(x) {
    if (!inherits(x, "tbl")) {
      tibble(
        neighbourhood = "City of Toronto",
        value = x
      )
    } else {
      x %>%
        mutate(neighbourhood = "City of Toronto")
    }
  })

## Combine with neighbourhoods ----

for (i in names(neighbourhoods)) {
  neighbourhoods[[i]] <- neighbourhoods[[i]] %>%
    bind_rows(city[[i]])
}

# Clean up data for combining into a single table -----

## First restructure LEM since it's inconsistently formatted, oops ----

# Make a separate element for Deeply / Very affordable
lem <- neighbourhoods[["lem"]] %>%
  filter(`Bedrooms` != "Total") %>%
  select(-Total) %>%
  pivot_longer(cols = c(`Deeply Affordable`, `Very Affordable`)) %>%
  rename(group = Bedrooms) %>%
  split(.$name) %>%
  map(select, -name)

neighbourhoods$lem <- NULL
neighbourhoods <- append(neighbourhoods, lem)

## Rename n to value in agi and tdf ----
neighbourhoods[c("agi", "tdf")] <- neighbourhoods[c("agi", "tdf")] %>%
  map(rename, value = n)

## Remove total, market, and market value in rental supply ----
neighbourhoods["rental_supply"] <- neighbourhoods["rental_supply"] %>%
  map(select, -renters, -market, -market_value, -value, -market_prop)

## Rename "prop" to value in others -----
neighbourhoods <- neighbourhoods %>%
  map(function(x) {
    if ("prop" %in% names(x) & !"value" %in% names(x)) {
      x %>%
        rename(value = prop)
    } else {
      x
    }
  })

## Combine ----
neighbourhoods <- neighbourhoods %>%
  bind_rows(.id = "variable")

# Clean up hierarchy / names -----
clean_variable_names <- tribble(
  ~variable, ~variable_clean, ~units,
  "population", "Population", "#",
  "households", "Households", "#",
  "population_change", "Population change (2011 to 2016)", "%",
  "population_density", "Population density", "people per square kilometer",
  "household_size", "Household size", "%",
  "average_total_income", "Average total household income", "$",
  "unaffordable_housing", "Tenants with unaffordable housing", "%",
  "lim_at", "Low-income measure after tax", "%",
  "visible_minority", "Visible minority population", "%",
  "household_tenure", "Household tenure", "%",
  "average_renter_shelter_cost", "Average renter shelter cost", "$",
  "structure_type", "Structure type", "%",
  "bedrooms", "Bedrooms", "%",
  "rental_supply", "Rental market supply", "%",
  "amenity_density", "Proximity to amenities", "%",
  "core_housing_need", "In core housing need", "%",
  "number_of_buildings", "Apartment buildings - Total", "#",
  "number_of_buildings_private", "Apartment buildings - Privately owned", "#",
  "number_of_buildings_tch", "Apartment buildings - Toronto Community Housing", "#",
  "number_of_buildings_social_housing", "Apartment buildings - Social Housing", "#",
  "number_of_units", "Apartment building units - Total", "#",
  "number_of_units_private", "Apartment building units - Privately owned", "#",
  "number_of_units_tch", "Apartment building units - Toronto Community Housing", "#",
  "number_of_units_social_housing", "Apartment building units - Social Housing", "#",
  "apartment_building_evaluation", "Median RentSafeTO score", "%",
  "agi", "Above guideline increase applications", "#",
  "tdf", "Tenant Defense Fund grants", "#",
  "evictions", "Evictions", "%",
  "Deeply Affordable", "Low end of market - Deeply affordable", "#",
  "Very Affordable", "Low end of market - Very affordable", "#",
)

neighbourhoods <- neighbourhoods %>%
  left_join(clean_variable_names, by = "variable") %>%
  select(variable_clean, units, group, value, neighbourhood) %>%
  mutate(group = coalesce(group, ""))

# Widen data ----
neighbourhoods <- neighbourhoods %>%
  pivot_wider(names_from = neighbourhood, values_from = value)

# Reorganize data -----

neighbourhoods <- neighbourhoods %>%
  select(Variable = variable_clean, Units = units, Group = group, `City of Toronto`, everything())

# Save data ----

write_csv(neighbourhoods, here::here("inst", "extdata", "Aggregate Data.csv"))
