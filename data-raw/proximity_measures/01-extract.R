# Extract proximity measures data
# Source: https://www150.statcan.gc.ca/n1/pub/17-26-0002/172600022020001-eng.htm

library(dplyr)
library(readr)
library(janitor)

proximity_measures_raw <- read_csv(here::here("data-raw", "proximity_measures", "raw", "PMD_en", "PMD-en.csv"))

# Select relevant columns, filter to Toronto
proximity_measures_toronto <- proximity_measures %>%
  clean_names() %>%
  filter(csdname == "Toronto") %>%
  select(dbuid, dbpop, starts_with("prox_idx"), transit_na, amenity_dense, suppressed)

saveRDS(proximity_measures_toronto, here::here("data-raw", "proximity_measures", "extract", "proximity_measures_toronto.rds"))
