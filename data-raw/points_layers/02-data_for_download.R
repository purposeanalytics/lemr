# Create a version of the points layers for downloading

library(dplyr)
library(stringr)
library(sf)
library(readr)
devtools::load_all()

data <- lemr::buildings %>%
  mutate(id = row_number())

# Extract lat and long ----
data_coords <- data %>%
  st_coordinates() %>%
  as_tibble()

names(data_coords) <- c("Longitude", "Latitude")

data <- data %>%
  as_tibble() %>%
  select(-geometry)

data_coords <- data_coords %>%
  bind_cols(data["id"])

# Remove columns that won't be used -----

data <- data %>%
  select(-rsn, -bing_address, -score_percent, -score_bucket, -tooltip)

# Convert names to Title Case ----
names(data) <- str_replace_all(names(data), "_", " ")
names(data) <- str_to_title(names(data))

# Rename relevant columns ----
data <- data %>%
  rename(
    `Apartment Building` = Apartment,
    `Apartment Building Property Type` = `Property Type`,
    `RentSafeTO Evaluation Completed On` = `Evaluation Completed On`,
    `Apartment Building Units` = Units,
    `Apartment Building Storeys` = Storeys,
    `RentSafeTO Score` = Score,
    `Has AGI` = Agi,
    `Dates AGI Initiated` = `Date Agi Initiated`,
    `Received TDF` = Tdf,
    `Received TDF Year` = `Tdf Year`,
    `TDF Reduced Increase By` = `Reduced Increase By`
  )

# Add Latitude / Longitude in ----
data <- data %>%
  left_join(data_coords, by = c("Id" = "id")) %>%
  relocate(Longitude, Latitude, .after = Address) %>%
  select(-Id)

# Convert NA to ""
data <- data %>%
  mutate_if(~ is.numeric(.x) | inherits(.x, "Date"), as.character) %>%
  mutate_if(is.character, coalesce, "")

# Save data -----
write_csv(data, here::here("inst", "extdata", "points_data.csv"))
