# Aggregate number of apartments per neighbourhood and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

apartments_by_neighbourhood <- apartment_building_registry %>%
  as_tibble() %>%
  count(neighbourhood, name = "value")

# Not all neighbourhoods have apartments, so need to complete the data set

neighbourhoods <- tibble(neighbourhood = names(neighbourhood_profiles))

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood") %>%
  mutate(value = coalesce(value, 0))

# Add to city profile

number_of_apartments_distribution <- apartments_by_neighbourhood %>%
  select(value)

city_profile <- append(city_profile, list(number_of_apartments_distribution = number_of_apartments_distribution))

usethis::use_data(city_profile, overwrite = TRUE)

# Add to neighbourhood profile

apartments_by_neighbourhood <- apartments_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood)

for(i in seq_along(neighbourhood_profiles)){
  neighbourhood_profiles[[i]] <- append(neighbourhood_profiles[[i]], list(number_of_apartments = apartments_by_neighbourhood[[i]]))
}

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)
