# Extract apartment building registry data, save with date extracted

library(opendatatoronto)
library(dplyr)
library(janitor)
library(readr)

# Get Apartment Building Registration Resource ----
# Extracted May 19, 2021
apartment_building_registry <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-registration/") %>%
  filter(name == "Apartment Building Registration Data") %>%
  get_resource()

# Save resource with date extracted
write_csv(apartment_building_registry, here::here("data-raw", "apartment_building_registry", "extract", glue::glue("{Sys.Date()}-apartment_building_registry.csv")))
