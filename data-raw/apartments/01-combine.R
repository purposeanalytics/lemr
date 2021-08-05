# Once the apartment_building_registry, apartment_building_evaluation data sets have been created, combine them

library(dplyr)
devtools::load_all()

apartment_buildings <- apartment_building_registry %>%
  # Only joining by RSN, because the registry addresses have ranges but the evaluation addresses do not - use the addresses from registry
  full_join(apartment_building_evaluation, by = "rsn", suffix = c("", ".y")) %>%
  mutate(
    site_address = coalesce(site_address, site_address.y),
    bing_address = coalesce(bing_address, bing_address.y),
    year_built = coalesce(year_built, year_built.y),
    neighbourhood = coalesce(neighbourhood, neighbourhood.y)
  ) %>%
  select(rsn, site_address, bing_address, neighbourhood, property_type, year_built, year_registered, confirmed_units, confirmed_storeys, evaluation_completed_on, score, results_of_score, score_colour = color)

usethis::use_data(apartment_buildings, overwrite = TRUE)
