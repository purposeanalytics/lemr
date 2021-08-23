# Clean

library(dplyr)
library(stringr)
library(tidyr)

evictions <- readRDS(here::here("data-raw", "points_layers", "evictions", "geocode", "evictions.rds"))

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

# Clean address
eviction_hearings <- evictions %>%
  mutate(
    address = str_remove(address, " Ontario"),
    address = str_remove(address, "Toronto|Scarborough|Etobicoke|North York|York|East York|Scarborugh|City"),
    address = str_trim(address)
  )

usethis::use_data(eviction_hearings, overwrite = TRUE)
