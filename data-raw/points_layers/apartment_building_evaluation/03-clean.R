# Clean apartment building evaluation

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(forcats)

apartment_building_evaluation <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "geocode", "apartment_building_evaluation.rds"))

# Use readr to fix column types, and convert "N/A" to NA

temp <- tempfile(fileext = ".csv")
write_csv(apartment_building_evaluation, temp)

apartment_building_evaluation <- read_csv(temp, na = c("", "NA", "N/A"), guess_max = 100)

# Keep the LATEST evaluation for each address only!

apartment_building_evaluation <- apartment_building_evaluation %>%
  group_by(rsn) %>%
  filter(evaluation_completed_on == max(evaluation_completed_on)) %>%
  ungroup()

# Reorder columns
apartment_building_evaluation <- apartment_building_evaluation %>%
  select(id, rsn, site_address, starts_with("bing"), property_type, neighbourhood, ward, year_built, year_registered, evaluation_completed_on, score, no_of_areas_evaluated, confirmed_storeys, confirmed_units, everything())

# Clean some up
apartment_building_evaluation <- apartment_building_evaluation %>%
  mutate(
    site_address = str_squish(site_address),
    site_address = str_to_title(site_address),
    property_type = str_to_title(property_type),
    property_type = ifelse(property_type == "Tchc", "TCHC", property_type)
  )

# Check data - 1s all across the board will lead to a very low score, and may be incorrect data
apartment_building_evaluation_outlier <- apartment_building_evaluation %>%
  filter(if_all(.cols = balcony_guards:water_pen_ext_bldg_elements, .fns = ~ .x == 1))

if (nrow(apartment_building_evaluation_outlier) > 0) {
  usethis::ui_todo("{nrow(apartment_building_evaluation_outlier)} examples of all 1s in score:")
  apartment_building_evaluation_outlier
}

# 2 Main St is a known issue - any others?

apartment_building_evaluation <- apartment_building_evaluation %>%
  anti_join(apartment_building_evaluation_outlier)

# Select relevant columns

apartment_building_evaluation <- apartment_building_evaluation %>%
  select(id, rsn, site_address, bing_address, property_type, neighbourhood, year_built, year_registered, evaluation_completed_on, score)

# Generate score as percent for display
apartment_building_evaluation <- apartment_building_evaluation %>%
  mutate(score_percent = case_when(
    is.na(score) ~ NA_character_,
    TRUE ~ paste0(score, "%")
  ))

# Colour points
# 0 - 50
# 51 - 65
# 66 - 80
# 81 - 100

# Break into buckets
apartment_building_evaluation <- apartment_building_evaluation %>%
  dplyr::mutate(
    score_bucket = cut(score, breaks = c(0, 50, 65, 80, 100), label = FALSE)
  )

apartment_building_evaluation <- apartment_building_evaluation %>%
  rename(address = site_address)

# Save clean dataset
saveRDS(apartment_building_evaluation, here::here("data-raw", "points_layers", "apartment_building_evaluation", "clean", "apartment_building_evaluation.rds"))
