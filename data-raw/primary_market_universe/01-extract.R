# Read in CMHC census tabulation, only keep Toronto and Toronto CTs

# Retrieved from: https://www03.cmhc-schl.gc.ca/hmip-pimh/#Profile/1/1/Canada
# Exported by:
# 1. selecting Canada > Ontario > Toronto on the map
# 2. Choosing "Full View"
# 3. Selecting "Rental Universe" from the Primary Rental Market section
# 4. Choosing "Row" from the Row/Apartment select box
# 6. Choosing "Census Tract" from the display options
# 7. Using the export button to save a .csv
# Repeat for "Apartments"

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)

### Set up paths for data sets ------

cts_apartments_path <- here::here("data-raw", "primary_market_universe", "raw", "primary_rental_apartments_by_ct_2020.csv")
cts_row_houses_path <- here::here("data-raw", "primary_market_universe", "raw", "primary_rental_row_houses_by_ct_2020.csv")

### Get Toronto census tracts -----

apartments_cts <- read_csv(cts_apartments_path, col_types = "c", skip = 2)
row_houses_cts <- read_csv(cts_row_houses_path, col_types = "c", skip = 2)

# ### Tidy names

apartments_cts <- apartments_cts %>%
  clean_names() %>%
  rename(
    ct = x1,
    total_apartments = total
  ) %>%
  discard(~all(is.na(.) | . ==""))

row_houses_cts <- row_houses_cts %>%
  clean_names() %>%
  rename(
    ct = x1,
    total_row_houses = total
  ) %>%
  discard(~all(is.na(.) | . ==""))

# ### Save Toronto census tracts

saveRDS(apartments_cts, here::here("data-raw", "primary_market_universe", "extract", "apartments.rds"))
saveRDS(row_houses_cts, here::here("data-raw", "primary_market_universe", "extract", "row_houses.rds"))

