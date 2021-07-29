# Clean apartment building evaluation

library(dplyr)
library(lubridate)
library(readr)
library(stringr)

apartment_building_evaluation <- readRDS(here::here("data-raw", "apartment_building_evaluation", "geocode", "apartment_building_evaluation.rds"))

# Use readr to fix column types, and convert "N/A" to NA

temp <- tempfile(fileext = ".csv")
write_csv(apartment_building_evaluation, temp)

apartment_building_evaluation <- read_csv(temp, na = c("", "NA", "N/A"), guess_max = 10000)

# Fix date column type
apartment_building_evaluation <- apartment_building_evaluation %>%
  mutate(evaluation_completed_on = mdy(evaluation_completed_on))

# Reorder columns
apartment_building_evaluation <- apartment_building_evaluation %>%
  select(id, rsn, site_address, starts_with("bing"), property_type, neighbourhood, ward, year_built, year_registered, evaluation_completed_on, score, results_of_score, no_of_areas_evaluated, confirmed_storeys, confirmed_units, everything())

# Clean some up
apartment_building_evaluation <- apartment_building_evaluation %>%
  mutate(
    site_address = str_squish(site_address),
    site_address = str_to_title(site_address),
    property_type = str_to_title(property_type),
    property_type = ifelse(property_type == "Tchc", "TCHC", property_type)
  )

# Save final dataset
usethis::use_data(apartment_building_evaluation, overwrite = TRUE)
