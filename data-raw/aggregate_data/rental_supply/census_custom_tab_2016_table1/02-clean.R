# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## CTs Data ----

toronto_census_tracts <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "extract", "custom_tab_toronto_table1.rds"))

# Read file for converting census tract to neighbourhood
geo_to_neighbourhood <- st_read(here::here("data-raw", "shared", "Census Geographies to TO Neighbourhoods.gpkg"))

# Just select relevant columns
ct_to_neighbourhood <- geo_to_neighbourhood %>%
  select(ct = CTUID, neighbourhood = AREA_NAME) %>%
  as_tibble() %>%
  select(-geom)

# ### Selecting columns ----

# Remove Toronto (535)
toronto_census_tracts <- toronto_census_tracts %>%
  filter(geography != 535) %>%
  mutate(across(c(total:two_plus_bedrooms), as.numeric))

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

toronto_census_tracts <- toronto_census_tracts %>%
  inner_join(ct_to_neighbourhood, by = c("geography" = "ct"))

### Order variables ----
toronto_census_tracts <- toronto_census_tracts %>%
  select(-one_bedroom, -two_plus_bedrooms)

### Save data ----
saveRDS(toronto_census_tracts, here::here("data-raw", "aggregate_data", "rental_supply", "census_custom_tab_2016_table1", "clean", "custom_tab_toronto_table1.rds"))
