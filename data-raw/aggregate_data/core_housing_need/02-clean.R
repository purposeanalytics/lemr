# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## CTs Data ----

core_housing_need_cts <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "extract", "core_housing_need.rds"))
households_tested_for_chn_cts <- readRDS(here::here("data-raw", "aggregate_data", "core_housing_need", "extract", "households_tested_for_chn.rds"))

# Read file for converting census tract to neighbourhood
geo_to_neighbourhood <- st_read(here::here("data-raw", "shared", "Census Geographies to TO Neighbourhoods.gpkg"))

# Just select relevant columns
ct_to_neighbourhood <- geo_to_neighbourhood %>%
  select(ct = CTUID, neighbourhood = AREA_NAME) %>%
  as_tibble() %>%
  select(-geom)

# ### Join files ---

core_housing_need_joined <- core_housing_need_cts %>%
  inner_join(households_tested_for_chn_cts, by = "ct", suffix = c("_in_core_housing_need", "_households"))

# ### Selecting columns ----

# Add 535 to ct id
core_housing_need_joined <- core_housing_need_joined %>%
  mutate(
    ct = paste0("535", ct),
    across(c(everything(), -ct), as.numeric)
  )

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

core_housing_need_joined <- core_housing_need_joined %>%
  inner_join(ct_to_neighbourhood, by = "ct")

### Save data ----
saveRDS(core_housing_need_joined, here::here("data-raw", "aggregate_data", "core_housing_need", "clean", "core_housing_need.rds"))
