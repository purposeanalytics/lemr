library(dplyr)
library(readr)
library(janitor)

rental_data <- read_csv(here::here("data-raw", "affordable-rental-market", "raw", "Rental Market Listings Full Dataset, 20190228.csv")) %>%
  clean_names()

# Keep relevant columns
rental_data <- rental_data %>%
  select(title, description, clean_address, latitude, longitude, bedrooms, rent, type, market_type)

# Add ID
rental_data <- rental_data %>%
  mutate(id = row_number())

saveRDS(rental_data, here::here("data-raw", "affordable-rental-market", "extract", "rental_data.rds"))
