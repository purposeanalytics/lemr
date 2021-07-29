# Extract apartment building evaluation data, save with date extracted

library(opendatatoronto)
library(dplyr)
library(janitor)

# Get Apartment Building Evaluation Resource ----
apartment_building_evaluation <- list_package_resources("https://open.toronto.ca/dataset/apartment-building-evaluation/") %>%
  filter(name == "Apartment Building Evaluation") %>%
  get_resource() %>%
  clean_names()

# Save resource with date extracted
saveRDS(apartment_building_evaluation, here::here("data-raw", "apartment_building_evaluation", "extract", glue::glue("{Sys.Date()}-apartment_building_evaluation.rds")))
