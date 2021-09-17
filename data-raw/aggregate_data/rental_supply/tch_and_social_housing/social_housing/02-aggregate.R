# Combine social housing and TCH data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## read social housing unit counts ----

social_housing <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "extract", "social_housing.rds"))

## read tch housing unit counts
tch_by_neighbourhood <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "toronto_community_housing", "aggregate", "toronto_community_housing.rds"))

# remove geometry for regular join
tch_by_neighbourhood <- tch_by_neighbourhood %>%
  st_set_geometry(NULL) %>%
  select(-neighbourhood)

# fix ids
social_housing <- social_housing %>%
  mutate(neighbourhood = as.numeric(neighbourhood)) %>%
  rename(id = neighbourhood)

# Read file for converting census tract to neighbourhood
neighbourhoods <- st_read(here::here("data-raw", "neighbourhoods", "final", "neighbourhoods.geojson"))

# ### Join files ---
# Get non-TCHC unit count by subtracting TCH units from all social housing types
# We believe TCH market units are not counted in the social housing datasets because they are
# not available on the social housing waitlist. Hence subtraction below:

social_housing_by_neighbourhood <- neighbourhoods %>%
  left_join(social_housing, by = "id") %>%
  left_join(tch_by_neighbourhood, by = "id") %>%
  mutate(
    other_social_units = social_units_total - tch_units_rgi,
    other_social_units_market = social_units_market,
    other_social_units_rgi = social_units_rgi - tch_units_rgi
  ) %>%
  group_by(neighbourhood, id) %>%
  summarize(
    other_social_units = sum(other_social_units, na.rm = TRUE),
    other_social_units_market = sum(other_social_units_market, na.rm = TRUE),
    other_social_units_rgi = sum(other_social_units_rgi, na.rm = TRUE)
  )

# There are a few neighbourhoods that have negative social housing values, we will
# set these to zero, assuming that there is a mismatch in the dataset. The TCHC
# numbers are more reliable. Something to look at in more detail later.

social_housing_by_neighbourhood <- social_housing_by_neighbourhood %>%
  mutate(
    other_social_units = if_else(other_social_unitits < 0, 0, other_social_units),
    other_social_units_rgi = if_else(other_social_units_rgi < 0, 0, other_social_units_rgi)
  )

### Save data ----
saveRDS(social_housing_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "social_housing", "aggregate", "social_housing.rds"))
