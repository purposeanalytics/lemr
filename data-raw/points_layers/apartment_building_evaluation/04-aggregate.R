# Aggregate median RentSafeTO Scores and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

median_score_by_neighbourhood <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "clean", "apartment_building_evaluation.rds")) %>%
  group_by(neighbourhood) %>%
  summarise(value = median(score, na.rm = TRUE))

# Not all neighbourhoods have apartments, so need to complete the data set - but keep as NAs

neighbourhoods <- tibble(neighbourhood = names(neighbourhood_profiles))

median_score_by_neighbourhood <- median_score_by_neighbourhood %>%
  right_join(neighbourhoods, by = "neighbourhood")


# Median score for the city

median_score_city <- apartment_building_evaluation %>%
  summarise(value = median(score, na.rm = TRUE))

apartment_building_evaluation_distribution <- median_score_by_neighbourhood %>%
  select(value) %>%
  filter(!is.na(value))

# And by neighbourhood

median_score_by_neighbourhood <- median_score_by_neighbourhood %>%
  arrange(neighbourhood) %>%
  split(.$neighbourhood) %>%
  map("value")

# Save

saveRDS()
