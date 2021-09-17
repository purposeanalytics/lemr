# Clean evictions data, just keep relevant column
# Add spatial elements in mapbox, otherwise it the data takes up too much room n the package!

library(dplyr)
library(sf)

# Read data
evictions_rate <- readRDS(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "extract", "evictions_rate.rds"))

# Clean up neighbourhood names
evictions_rate <- evictions_rate %>%
  mutate(neighbourhood = case_when(
    neighbourhood == "Mimico" ~ "Mimico (includes Humber Bay Shores)",
    neighbourhood == "Danforth-East York" ~ "Danforth East York",
    neighbourhood == "Dovercourt-Wallace Emerson-Juncti" ~ "Dovercourt-Wallace Emerson-Junction",
    TRUE ~ neighbourhood
  ))

# Keep relevant columns
evictions_by_neighbourhood <- evictions_rate %>%
  select(neighbourhood, evictions = l1or_l2_2016, eviction_rate = filing_rate2016, renter_households) %>%
  mutate(eviction_rate = round(eviction_rate, 3))

# Evictions for the whole city
evictions_city <- evictions_by_neighbourhood %>%
  summarise(
    evictions = sum(evictions),
    renter_households = sum(renter_households),
    eviction_rate = evictions / renter_households
  )

evictions_by_neighbourhood <- evictions_by_neighbourhood %>%
  select(-renter_households)

# Save
saveRDS(evictions_by_neighbourhood, here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "extract", "evictions_by_neighbourhood.rds"))
saveRDS(evictions_city, here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "extract", "evictions_city.rds"))
