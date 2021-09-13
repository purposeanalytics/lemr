# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

## CTs Data ----

apartments_cts <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "extract", "apartments.rds"))
row_houses_cts <- readRDS(here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "extract", "row_houses.rds"))

# Read file for converting census tract to neighbourhood
geo_to_neighbourhood <- st_read(here::here("data-raw", "shared", "Census Geographies to TO Neighbourhoods.gpkg"))

# Just select relevant columns
ct_to_neighbourhood <- geo_to_neighbourhood %>%
  select(ct = CTUID, neighbourhood = AREA_NAME) %>%
  as_tibble() %>%
  select(-geom)

# ### Join files ---

primary_rental <- apartments_cts %>%
  select(ct, total_apartments) %>%
  full_join(row_houses_cts %>%
               select(ct, total_row_houses), by = "ct") %>%
  mutate(total_apartments = coalesce(total_apartments, 0),
         total_row_houses = coalesce(total_row_houses, 0),
         total_primary_rental = total_apartments + total_row_houses)


# ### Selecting columns ----

# Add 535 to ct id
primary_rental <- primary_rental %>%
  mutate(ct = paste0("535", ct),
         across(c(everything(), -ct), as.numeric))

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

primary_rental <- primary_rental %>%
  full_join(ct_to_neighbourhood, by = "ct")

### Save data ----
saveRDS(primary_rental, here::here("data-raw", "aggregate_data", "rental_supply", "primary_market_universe", "clean", "primary_market_universe.rds"))

