# Aggregate number of rooming houses per neighbourhood and add to city_profile / neighbourhood_profile for easy visualization

library(dplyr)
library(purrr)
devtools::load_all()

rooming_houses_clean <- readRDS(here::here("data-raw", "points_layers", "rooming_houses", "clean", "rooming_houses_clean.rds"))


# remove neighbourhoods
rooming_houses_clean <- rooming_houses_clean %>%
  select(-neighbourhood_id, -neighbourhood)

# Get neighbourhood for each building
rooming_houses_by_neighbourhood <- neighbourhoods %>%
  st_join(rooming_houses_clean) %>%
  group_by(neighbourhood, neighbourhood_id = id) %>%
  summarize(licensed = sum(status == "licensed", na.rm = TRUE),
            new = sum(status == "licensed after 2018", na.rm = TRUE),
            lapsed = sum(status == "lapsed", na.rm = TRUE),
            percent_lapsed = lapsed/(licensed + new + lapsed)) %>%
  mutate(percent_lapsed = coalesce(percent_lapsed, 0))

# Save
saveRDS(rooming_houses, here::here("data-raw", "points_layers", "rooming_houses", "aggregate", "rooming_houses.rds"))
