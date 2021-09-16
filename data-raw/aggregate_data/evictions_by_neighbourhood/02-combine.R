# Join evictions data to neighbourhoods shapefile

library(dplyr)
library(sf)

# Read data
evictions_rate <- read_rds(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "extract", "evictions_rate.rds")) %>%
  select(-neighbourhood)

# Read neighbourhoods shapefile
neighbourhoods <- read_sf(here::here("data-raw", "neighbourhoods", "final", "neighbourhoods.geojson"))

# ### Spatialize---
evictions_by_neighbourhood <- neighbourhoods %>%
  inner_join(evictions_rate, by = "id")

# Take it from here Sharla!
