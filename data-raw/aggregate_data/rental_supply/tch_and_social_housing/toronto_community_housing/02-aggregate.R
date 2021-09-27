# Clean toronto community housing data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## read TCH buildings ----

toronto_community_housing <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "toronto_community_housing", "extract", "toronto_community_housing.rds"))

# remove geometry so that we can join by id rather than spatial coords
toronto_community_housing <- toronto_community_housing %>%
  st_set_geometry(NULL)

# ### Join files ---

tch_by_neighbourhood <- lemr::neighbourhoods %>%
  left_join(toronto_community_housing, by = c("id" = "neighbourhood_id")) %>%
  group_by(neighbourhood, id) %>%
  summarize(
    tch_units_total = sum(total_units, na.rm = TRUE),
    tch_units_market = sum(market_units, na.rm = TRUE),
    tch_units_rgi = sum(rgi_units, na.rm = TRUE)
  ) %>%
  ungroup()

### Save data ----
fs::dir_create(here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "toronto_community_housing", "aggregate"))
saveRDS(tch_by_neighbourhood, here::here("data-raw", "aggregate_data", "rental_supply", "tch_and_social_housing", "toronto_community_housing", "aggregate", "toronto_community_housing.rds"))
