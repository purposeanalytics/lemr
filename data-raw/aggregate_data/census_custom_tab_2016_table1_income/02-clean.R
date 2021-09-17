# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## CTs Data ----

toronto_census_tracts <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "extract", "custom_tab_toronto_table1.rds"))

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
  mutate(across(c(total_household_income_statistics:average_total_income_of_households), as.numeric))

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

toronto_census_tracts <- toronto_census_tracts %>%
  inner_join(ct_to_neighbourhood, by = c("geography" = "ct"))

### Save data ----
saveRDS(toronto_census_tracts, here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "clean", "custom_tab_toronto_table1.rds"))
