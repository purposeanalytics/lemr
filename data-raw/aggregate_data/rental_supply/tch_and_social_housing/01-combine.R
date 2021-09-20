# Combine social housing and TCH data to get final counts

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## Read social housing unit counts ----

social_housing <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "aggregate", "social_housing_by_neighbourhood.rds"))

## read tch housing unit counts
tch_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "toronto_community_housing", "aggregate", "toronto_community_housing.rds"))

# ### Join files ---
# Get non-TCHC unit count by subtracting TCH units from all social housing types
# We believe TCH market units are not counted in the social housing datasets because they are
# not available on the social housing waitlist. Hence subtraction below:

social_housing_and_tch_by_neighbourhood <- social_housing %>%
  left_join(tch_by_neighbourhood, by = "neighbourhood") %>%
  mutate(
    other_social_units = social_units_total - tch_units_rgi,
    other_social_units_market = social_units_market,
    other_social_units_rgi = social_units_rgi - tch_units_rgi
  ) %>%
  group_by(neighbourhood, tch_units_total, tch_units_market, tch_units_rgi) %>%
  summarise(
    other_social_units = sum(other_social_units, na.rm = TRUE),
    other_social_units_market = sum(other_social_units_market, na.rm = TRUE),
    other_social_units_rgi = sum(other_social_units_rgi, na.rm = TRUE),
    .groups = "drop"
  )

# There are a few neighbourhoods that have negative social housing values, we will
# set these to zero, assuming that there is a mismatch in the dataset. The TCHC
# numbers are more reliable. Something to look at in more detail later.

social_housing_and_tch_by_neighbourhood <- social_housing_and_tch_by_neighbourhood %>%
  mutate(
    other_social_units = ifelse(other_social_units < 0, 0, other_social_units),
    other_social_units_rgi = ifelse(other_social_units_rgi < 0, 0, other_social_units_rgi)
  )

### Save data ----
fs::dir_create(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "aggregate"))
saveRDS(social_housing_and_tch_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "aggregate", "social_housing_and_tch_by_neighbourhood.rds"))
