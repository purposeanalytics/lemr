# Model the "total" rent for "Room" and "Shared Room"

library(dplyr)
library(ggplot2)
library(tidymodels)
library(randomForest)
library(ggbeeswarm)
library(stringr)

set.seed(1234)

rental_data <- readRDS(here::here("data-raw", "affordable-rental-market", "geocode", "rental_data.rds"))

# Get postal code (really FSA) for modelling)
rental_data <- rental_data %>%
  mutate(postal_code = str_extract(clean_address, "M[0-9][A-Z]"))

# Initial attempt / analysis -----

# total_rent is just the rent if it's not shared

rental_data <- rental_data %>%
  mutate(total_rent = case_when(
    bedrooms %in% c("Room", "Shared Room") ~ NA_real_,
    TRUE ~ rent
  ))

# Looking for "two / three bedroom" etc
# Also look for reference of "X other" tenants
# Then we can get the number of bedrooms, and number of tenants
# If no # of tenants, assume # of bedrooms (x2 if Shared Room)
# If no # bedrooms, assume # of tenants (/2 if Shared Room)
rental_data_rooms_or_others <- rental_data %>%
  filter(bedrooms %in% c("Room", "Shared Room")) %>%
  mutate(
    description = tolower(description),
    total_rooms = case_when(
      str_detect(description, "two bed|two room|2 bed|2 room|two bdrm|2 bdrm") ~ 2,
      str_detect(description, "three bed|three room|3 bed|3 room|three bdrm|3 bdrm") ~ 3,
      str_detect(description, "four bed|four room|4 bed|4 room|four bdrm|4 bdrm") ~ 3,
      str_detect(description, "five bed|five room|5 bed|5 room|five bdrm|5 bdrm") ~ 3,
      str_detect(description, "six bed|six room|6 bed|6 room|six bdrm|6 bdrm") ~ 3
    ),
    other_tenants = str_extract(description, "[0-9] other"),
    other_tenants = readr::parse_number(other_tenants),
    total_tenants_from_rooms = total_rooms * ifelse(bedrooms == "Shared Room", 2, 1),
    total_tenants_from_others = other_tenants + 1,
    total_tenants = map2_dbl(
      total_tenants_from_rooms, total_tenants_from_others,
      function(x, y) {
        if (is.na(x) & is.na(y)) {
          NA_real_
        } else {
          max(x, y, na.rm = TRUE)
        }
      }
    ),
    # Derive number of rooms from total tenants if it's not available otherwise
    # If the room is shared, take total tenants / 2
    # If it's not, just take total tenants
    total_rooms = case_when(
      !is.na(total_rooms) ~ total_rooms,
      bedrooms == "Room" ~ total_tenants,
      bedrooms == "Shared Room" ~ round(total_tenants / 2)
    )
  ) %>%
  filter(!is.na(total_rooms))

rental_data_rooms_or_others <- rental_data_rooms_or_others %>%
  mutate(
    total_rent = rent * total_tenants,
    bedrooms = case_when(
      total_rooms == 1 ~ "1",
      total_rooms == 2 ~ "2",
      TRUE ~ "3+"
    )
  ) %>%
  select(all_of(names(rental_data)), total_rooms)

rental_data <- rental_data %>%
  mutate(total_rooms = NA_real_) %>%
  rows_update(rental_data_rooms_or_others, by = "id")

# Now create a model to get the rest! ----

# Based on rent per room, market type, and postal code, try to get how many bedrooms there are TOTAL for "Room" and "Shared Room", then get the total rent for those units.

# Convert bedrooms to numeric to get "rent per room", and keep 0 bedrooms separate for modeling
rental_data_modeling <- rental_data %>%
  filter(!is.na(postal_code)) %>%
  filter(!bedrooms %in% c("Room", "Shared Room")) %>%
  mutate(
    rooms = recode(bedrooms, "0" = 1, "1" = 1, "1 + den" = 1.5, "2" = 2, "2 + den" = 2.5, "3+" = 3),
    rooms = coalesce(total_rooms, rooms),
    rent_per_room = rent / rooms,
    rooms = as.character(rooms),
    rooms = case_when(
      bedrooms == "0" ~ "bachelor",
      TRUE ~ rooms
    ),
    rooms = as.factor(rooms),
    market_type = as.factor(market_type)
  )

# More cleaning up for modelling - combine some factors, drop factors without much data (they will cause issues in CV / tuning)
tidy_split <- rental_data_modeling %>%
  mutate(
    rooms = as.character(rooms),
    rooms = case_when(
      rooms == "1.5" ~ "1",
      rooms == "2.5" ~ "2",
      TRUE ~ rooms
    )
  ) %>%
  filter(!rooms %in% c("4", "5", "6", "7")) %>%
  mutate(rooms = forcats::fct_drop(rooms)) %>%
  initial_split(prop = .8)

# Set up training, test sets, and cross validation
tidy_train <- training(tidy_split)
tidy_test <- testing(tidy_split)
tidy_kfolds <- vfold_cv(tidy_train, v = 5)

# Create recipe of model with rent per room, market type, and postal code
# Generate dummy variables and remove variables that only contain a single value
tidy_rec <- recipe(rooms ~ rent_per_room + market_type + postal_code, data = tidy_train) %>%
  step_novel(postal_code) %>%
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

# Set up the random forest model for tuning
tidy_rf_model <- rand_forest(
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Tune!
# This all takes a while so not running it again - just using the final results
# trees: 1111
# min_n: 6
# rf_grid <- grid_regular(parameters(tidy_rf_model), levels = 10)
# rf_tune <- tune_grid(tidy_rf_model,
#   tidy_rec,
#   resamples = tidy_kfolds,
#   grid = rf_grid
# )
#
# # Pick best parameters based on ROC/AUC or Accuracy
# # ROC gave us the best accuracy, about 64% - not bad on a classification of 4 groups
# roc_param <- rf_tune %>% select_best("roc_auc")
# accuracy_param <- rf_tune %>% select_best("accuracy")
# # Apply parameters to the models
# tidy_rf_model <- finalize_model(tidy_rf_model, roc_param)
#
# boosted_wf <- workflow() %>%
#   add_model(tidy_rf_model) %>%
#   add_recipe(tidy_rec)
#
# boosted_res <- last_fit(boosted_wf, tidy_split)
#
# boosted_res %>%
#   unnest(.predictions) %>%
#   conf_mat(truth = rooms, estimate = .pred_class)
#
# boosted_res %>%
#   unnest(.predictions) %>%
#   mutate(accurate = rooms == .pred_class) %>%
#   pull(accurate) %>%
#   mean()

# Final model:
roc_params <- tibble(
  trees = 1111,
  min_n = 6
)

rf_model <- finalize_model(tidy_rf_model, roc_params)

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(tidy_rec)

rf_res <- rf_workflow %>%
  fit(tidy_train)

data_for_predictions <- rental_data %>%
  filter(bedrooms %in% c("Room", "Shared Room")) %>%
  mutate(rent_per_room = case_when(
    bedrooms == "Shared Room" ~ rent * 2,
    TRUE ~ rent
  )) %>%
  filter(!is.na(postal_code)) %>%
  select(id, latitude, longitude, rent_per_room, market_type, postal_code)

predictions <- rf_res %>%
  predict(data_for_predictions) %>%
  rename(rooms = .pred_class) %>%
  bind_cols(data_for_predictions) %>%
  mutate(
    rooms_numeric = as.character(rooms),
    rooms_numeric = case_when(
      rooms_numeric == "bachelor" ~ 1,
      TRUE ~ readr::parse_number(rooms_numeric)
    ),
    total_rent = rent_per_room * rooms_numeric
  ) %>%
  select(id, latitude, longitude, rooms, market_type, postal_code, total_rent) %>%
  mutate(estimated = TRUE)

# Re-combine

full_data_not_used_in_model <- rental_data %>%
  filter(!bedrooms %in% c("Room", "Shared Room")) %>%
  anti_join(rental_data_modeling, by = "id")

rental_data_combined <- rental_data_modeling %>%
  bind_rows(full_data_not_used_in_model) %>%
  bind_rows(predictions) %>%
  mutate(
    estimated = coalesce(estimated, FALSE),
    bedrooms = case_when(
      rooms == "bachelor" ~ "0",
      rooms %in% c("1", "1.5") ~ "1",
      rooms %in% c("2", "2.5") ~ "2",
      TRUE ~ "3+"
    )
  )

# Looks pretty decent
rental_data_combined %>%
  ggplot(aes(x = bedrooms, y = total_rent, color = estimated)) +
  geom_point(position = position_jitterdodge()) +
  facet_wrap(vars(market_type))

# Clean up estimate data ----
rental_data <- rental_data_combined %>%
  select(id, latitude, longitude, bedrooms, market_type, total_rent, estimated)

saveRDS(rental_data, here::here("data-raw", "affordable-rental-market", "model", "rental_data.rds"))
