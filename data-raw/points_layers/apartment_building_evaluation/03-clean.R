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
  select(id, rsn, site_address, starts_with("bing"), property_type, neighbourhood, ward, year_built, year_registered, evaluation_completed_on, score, results_of_score, no_of_areas_evaluated, confirmed_storeys, confirmed_units, everything())

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
  select(id, rsn, site_address, bing_address, property_type, neighbourhood, year_built, year_registered, evaluation_completed_on, score, results_of_score)

# Generate score as percent for display
apartment_building_evaluation <- apartment_building_evaluation %>%
  mutate(score_percent = case_when(
    is.na(score) ~ NA_character_,
    TRUE ~ paste0(score, "%")
  ))

# Colour points
# All 50% and lower will get the lightest color
# And the rest of the gradient will be 51 - 100

# Set colours
n <- 7
apartment_building_evaluation <- apartment_building_evaluation %>%
  dplyr::mutate(
    score_bucket = cut(score, breaks = seq(51, 100, length.out = n - 1)),
    score_bucket = fct_explicit_na(score_bucket, na_level = "<50"),
    score_bucket = fct_relevel(score_bucket, "<50", after = 0)
  )

score_bucket_colors <- dplyr::tibble(
  score_bucket = levels(apartment_building_evaluation[["score_bucket"]]),
  score_colour = c("#FFFFCC", "#FED976", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126")
)

apartment_building_evaluation <- apartment_building_evaluation %>%
  dplyr::left_join(score_bucket_colors, by = "score_bucket")

apartment_building_evaluation <- apartment_building_evaluation %>%
  select(-score_bucket)

apartment_building_evaluation <- apartment_building_evaluation %>%
  rename(address = site_address) %>%
  select(-id)

# Save final dataset
usethis::use_data(apartment_building_evaluation, overwrite = TRUE)
