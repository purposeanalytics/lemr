# Read in CMHC census tabulation, only keep Toronto and Toronto CTs

# Retrieved from: https://www03.cmhc-schl.gc.ca/hmip-pimh/#Profile/1/1/Canada
# Exported by:
# 1. selecting Canada > Ontario > Toronto on the map
# 2. Choosing "Full View"
# 3. Selecting "Housing Standards" from the Core Housing Need section
# 4. Choosing "Renters" from the Tenure type
# 5. Choosing "Households in Core Housing Need" as the Universe
# 6. Choosing "Census Tract" from the display options
# 7. Using the export button to save a .csv
# Repeat for "Households tested for Core Housing Need"

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)

### Set up paths for data sets ------

cts_core_housing_need_path <- here::here("data-raw", "aggregate_data", "core_housing_need", "raw", "renter_core_housing_need_by_ct_2016.csv")
cts_households_tested_for_chn_path <- here::here("data-raw", "aggregate_data",  "core_housing_need", "raw", "renter_households_tested_for_chn_by_ct_2016.csv")

### Get Toronto census tracts -----

core_housing_need_cts <- read_csv(cts_core_housing_need_path, col_types = "c", skip = 2)
households_tested_for_chn_cts <- read_csv(cts_households_tested_for_chn_path, col_types = "c", skip = 2)

# ### Tidy names

core_housing_need_cts <- core_housing_need_cts %>%
  clean_names() %>%
  rename(
    ct = x1,
    total_in_core_housing_need = total
  ) %>%
  discard(~all(is.na(.) | . ==""))

households_tested_for_chn_cts <- households_tested_for_chn_cts %>%
  clean_names() %>%
  rename(
    ct = x1,
    households_tested_for_core_housing_need = total
  )  %>%
  discard(~all(is.na(.) | . ==""))

# ### Save Toronto census tracts

saveRDS(core_housing_need_cts, here::here("data-raw", "aggregate_data", "core_housing_need", "extract", "core_housing_need.rds"))
saveRDS(households_tested_for_chn_cts, here::here("data-raw", "aggregate_data", "core_housing_need", "extract", "households_tested_for_chn.rds"))

