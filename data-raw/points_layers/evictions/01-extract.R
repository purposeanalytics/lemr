# Extract evictions data

library(dplyr)
library(readr)
library(tidyr)
library(janitor)

evictions <- read_csv(here::here("data-raw", "points_layers", "evictions", "raw", "data.csv"))

# Convert into wide format - one row for each address, one column for landlord / property management and one for number of hearings

evictions <- evictions %>%
  clean_names() %>%
  select(-category) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  rename(
    landlord_property_mgmt = `Landlord and/or property manager`,
    hearings = `Number of hearings at this address:`
  ) %>%
  mutate(
    landlord_property_mgmt = na_if(landlord_property_mgmt, "-"),
    landlord_property_mgmt = na_if(landlord_property_mgmt, "--"),
    hearings = as.numeric(hearings)
  )

# Save data

saveRDS(evictions, here::here("data-raw", "points_layers", "evictions", "extract", "evictions.rds"))
