library(opendatatoronto)
library(dplyr)
library(here)
library(readr)
library(fs)
library(stringr)

# Retrieved May 6
apartment_building_registry_raw <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-registration/") %>%
  get_resource()

# Save copy of raw data before processing
dir_create(here::here("data-raw", "apartment_building_registry"))
write_csv(apartment_building_registry_raw, here::here("data-raw", "apartment_building_registry", "2021-05-06-apartment_building_registry.csv"))

# Just retain address for address search
apartment_building_registry <- apartment_building_registry_raw %>%
  select(address = SITE_ADDRESS) %>%
  mutate(
    address = str_squish(address), # remove excess whitespace
    address = str_to_title(address) # convert case
  )

usethis::use_data(apartment_building_registry, overwrite = TRUE)
