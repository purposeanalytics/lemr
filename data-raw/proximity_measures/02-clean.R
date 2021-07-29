# Clean apartment building registry
# Convert into long format, recode / expand variables

library(dplyr)
library(tidyr)
library(ggplot2)

proximity_measures_toronto_raw <- readRDS(here::here("data-raw", "proximity_measures", "extract", "proximity_measures_toronto.rds"))

# Convert to long ----
proximity_measures_toronto <- proximity_measures_toronto_raw %>%
  pivot_longer(
    cols = starts_with("prox_idx"),
    names_to = "proximity_measure",
    values_to = "index",
    names_prefix = "prox_idx_"
  )

# Get numeric index (some are .. or F, not available / suppressed) ----
proximity_measures_toronto <- proximity_measures_toronto %>%
  rename(index_original = index) %>%
  mutate(index = as.numeric(index_original))

# Recode proximity measures to their "full" name ----
proximity_measures_full_names <- tribble(
  ~proximity_measure_code, ~proximity_measure_full,
  "childcare", "Child care",
  "educpri", "Primary education",
  "educsec", "Secondary education",
  "emp", "Employment",
  "grocery", "Grocery stores",
  "health", "Health care",
  "lib", "Libraries",
  "parks", "Neighbourhood parks",
  "pharma", "Pharmacies",
  "transit", "Public transit",
)

proximity_measures_toronto <- proximity_measures_toronto %>%
  left_join(proximity_measures_full_names, by = c("proximity_measure" = "proximity_measure_code")) %>%
  select(-proximity_measure) %>%
  rename(proximity_measure = proximity_measure_full)

# Recode NAs ----
# .. = "Not available" actually means 0!
# F = "Suppressed"

proximity_measures_toronto <- proximity_measures_toronto %>%
  mutate(index = case_when(
    index_original == ".." ~ 0,
    is.na(index) ~ NA_real_,
    TRUE ~ index
  ))

# Recode amenity density ----
proximity_measures_toronto <- proximity_measures_toronto %>%
  mutate(amenity_dense = case_when(
    amenity_dense == 0 ~ "Low",
    amenity_dense == 1 ~ "Medium",
    amenity_dense == 2 ~ "High",
    TRUE ~ "Unknown"
  ))

# Rearrange columns ----
proximity_measures_toronto <- proximity_measures_toronto %>%
  select(dbuid, population = dbpop, amenity_dense, proximity_measure, index_original, index)

# Save -----
saveRDS(proximity_measures_toronto, here::here("data-raw", "proximity_measures", "clean", "proximity_measures_toronto.rds"))
