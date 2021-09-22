# Read in social housing unit counts by neighbourhood

# Retrieved from: https://open.toronto.ca/dataset/social-housing-unit-density-by-neighbourhoods/
# Includes:
# Toronto Community Housing Corporation locations, Housing Connections locations, non-profits and
# co-op developments participating in the social housing wait list
# Two columns: Units & RGI
# We will use Units because a non-RGI unit is still a non-market unit
# This will get us a total for the city that does not split Social Housing from other non-market
# We will then subtract the TCHC numbers from this total to get an estimate for non-profit and
# affordable housing units. This will be undercounted by about 30% because not all non-profit
# and affordable housing units are connected to the social housing wait list.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(opendatatoronto)

social_housing <- list_package_resources("https://open.toronto.ca/dataset/social-housing-unit-density-by-neighbourhoods/") %>%
  get_resource()

# ### Tidy names
# calculate market units -- we don't need this, but don't want to forget that we have a rgi-market breakdown

social_housing <- social_housing %>%
  clean_names() %>%
  rename(social_units_total = units,
         social_units_rgi = rgi) %>%
  mutate(social_units_market = social_units_total - social_units_rgi)

# ### Save data
fs::dir_create( here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "extract"))
saveRDS(social_housing, here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "extract", "social_housing.rds"))

