# Aggregate number of apartments and units per neighbourhood and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

apartments_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  count(neighbourhood, name = "value")

units_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  group_by(neighbourhood) %>%
  summarise(value = sum(confirmed_units, na.rm = TRUE))

# Not all neighbourhoods have apartments, so need to complete the data set

neighbourhoods <- tibble(neighbourhood = names(neighbourhood_profiles))

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood") %>%
  mutate(value = coalesce(value, 0))

units_by_neighbourhood <- units_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood") %>%
  mutate(value = coalesce(value, 0))

# Add to city profile

number_of_apartments_distribution <- apartments_by_neighbourhood %>%
  select(value)

units_by_neighbourhood_distribution <- units_by_neighbourhood %>%
  select(value)

city_profile[["number_of_apartments_distribution"]] <- number_of_apartments_distribution

city_profile[["number_of_units_distribution"]] <- units_by_neighbourhood_distribution

usethis::use_data(city_profile, overwrite = TRUE)

# Add to neighbourhood profile

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

units_by_neighbourhood <- units_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

for(i in seq_along(neighbourhood_profiles)){
  neighbourhood_profiles[[i]][["number_of_apartments"]] <- apartments_by_neighbourhood[[i]]
  neighbourhood_profiles[[i]][["number_of_units"]] <- units_by_neighbourhood[[i]]
}

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
