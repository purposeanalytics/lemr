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
    value = sum(evictions),
    renter_households = sum(renter_households)
  ) %>%
  mutate(prop = value / renter_households) %>%
  select(prop)

evictions_by_neighbourhood <- evictions_by_neighbourhood %>%
  select(neighbourhood, prop = eviction_rate)

# Save
saveRDS(evictions_city, here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "aggregate", "evictions_city.rds"))

# Version for mapping ----

# Add groups for colour, then make wide
# 6 groups in c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")

evictions_by_neighbourhood <- evictions_by_neighbourhood %>%
  select(neighbourhood, prop) %>%
  mutate(prop_group = cut(prop, seq(0, 0.20, length.out = 7), include.lowest = TRUE, labels = FALSE))

usethis::use_data(evictions_by_neighbourhood, overwrite = TRUE)

evictions_by_neighbourhood <- evictions_by_neighbourhood %>% select(-prop_group) %>% split(.$neighbourhood)

saveRDS(evictions_by_neighbourhood, here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "aggregate", "evictions_by_neighbourhood.rds"))
