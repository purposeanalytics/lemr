# Geocode apartment building evaluation
# The data set can be connected to lemur::apartment_building_evaluation, so don't actually need to geocode - yay!

library(dplyr)
library(sf)
devtools::load_all() # Load package itself to get geocode_address and read_latest_file

# Read in latest extract
apartment_building_evaluation <- read_latest_file(directory = here::here("data-raw", "apartments", "apartment_building_evaluation", "extract"), suffix = "-apartment_building_evaluation.rds", fileext = "rds")

apartment_building_evaluation <- apartment_building_evaluation %>%
  left_join(apartment_building_registry %>%
    as_tibble() %>%
      mutate(rsn = as.character(rsn)) %>%
    select(rsn, starts_with("bing"), neighbourhood),
  by = "rsn"
  )

# Check that all apartments have geocoding
apartments_need_geocoding <- apartment_building_evaluation %>%
  filter(is.na(bing_address)) %>%
  nrow()

if (apartments_need_geocoding == 0) {
  usethis::ui_done("All apartments have geocoding!")
} else {
  usethis::ui_todo("Not all apartments were in apartment_building_registry! {apartments_need_geocoding} need additional geocoding.")
}

# Save geocoded
saveRDS(apartment_building_evaluation, here::here("data-raw", "apartments", "apartment_building_evaluation", "geocode", "apartment_building_evaluation.rds"))
