# Aggregate median RentSafeTO Scores and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

median_score_by_neighbourhood <- apartment_building_evaluation %>%
  group_by(neighbourhood) %>%
  summarise(value = median(score, na.rm = TRUE))

# Not all neighbourhoods have apartments, so need to complete the data set - but keep as NAs

neighbourhoods <- tibble(neighbourhood = names(neighbourhood_profiles))

median_score_by_neighbourhood <- median_score_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood")

# Add to city profile

# Median score for the city

median_score_city <- apartment_building_evaluation %>%
  summarise(value = median(score, na.rm = TRUE))

city_profile[["apartment_building_evaluation"]] <- median_score_city

apartment_building_evaluation_distribution <- median_score_by_neighbourhood %>%
  select(value) %>%
  filter(!is.na(value))

city_profile[["apartment_building_evaluation_distribution"]] <- apartment_building_evaluation_distribution

usethis::use_data(city_profile, overwrite = TRUE)

# Add to neighbourhood profile

median_score_by_neighbourhood <- median_score_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

for(i in seq_along(neighbourhood_profiles)){
  neighbourhood_profiles[[i]][["apartment_building_evaluation"]] <- median_score_by_neighbourhood[[i]]
}

usethis::use_data(neighbourhood_profiles, overwrite = TRUE)

