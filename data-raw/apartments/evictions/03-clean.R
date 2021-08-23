# Clean

library(dplyr)
library(stringr)
library(tidyr)

evictions <- readRDS(here::here("data-raw", "apartments", "evictions", "geocode", "evictions.rds"))

# Select relevant columns

evictions <- evictions %>%
  select(address, landlord_property_mgmt, hearings, bing_address, bing_latitude, bing_longitude)

# Clean landlord / property management

evictions <- evictions %>%
  separate(landlord_property_mgmt, into = "landlord", sep = ",") %>%
  mutate(landlord = str_to_title(landlord) %>%
    str_remove_all("\\.|\\,") %>%
    str_remove_all("Limited|Incorporated|Corporation|Inc|Ltd|Corp|Lp") %>%
    str_trim())

eviction_hearings <- evictions

usethis::use_data(eviction_hearings, overwrite = TRUE)
