# Extract apartment building registry data, save with date extracted

library(opendatatoronto)
library(dplyr)
library(janitor)
library(readr)

# Potential TODO later on: look at the date this data set was last modified, and only download it if it is *after* the latest date we grabbed it?
# Might be easier to keep track of date grabbed in a CSV rather than relying solely on the filenames (which are slightly more annoying to parse)

# Get Apartment Building Registration Resource ----
apartment_building_registry <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-registration/") %>%
  filter(name == "Apartment Building Registration Data") %>%
  get_resource()

# Save resource with date extracted
write_csv(apartment_building_registry, here::here("data-raw", "apartment_building_registry", "extract", glue::glue("{Sys.Date()}-apartment_building_registry.csv")))
