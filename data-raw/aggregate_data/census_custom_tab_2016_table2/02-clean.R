# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## CTs Data ----

toronto_census_tracts <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "extract", "custom_tab_toronto_table2.rds"))

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
  filter(geography != 535)

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

toronto_census_tracts <- toronto_census_tracts %>%
  inner_join(ct_to_neighbourhood, by = c("geography" = "ct"))

### Order variables and turn household size value back into human-readable text----
toronto_census_tracts_clean <- toronto_census_tracts %>%
  pivot_longer(c(total:five_or_more_person), names_to = "household_size", values_to = "total") %>%
  relocate(average_household_size, .after = total) %>%
  mutate(household_size = recode(household_size,
                                 "one_person" = "1 person",
                                 "two_person" = "2 person",
                                 "three_person" = "3 person",
                                 "four_person" = "4 person",
                                 "five_or_more_person" = "5+ persons",
                                 "total" = "Total - Household size"))

### Save data ----
saveRDS(toronto_census_tracts_clean, here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table2", "clean", "custom_tab_toronto_table2.rds"))
