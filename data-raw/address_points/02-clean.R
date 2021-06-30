library(dplyr)
library(sf)
library(RSQLite)

address_points_raw <- readRDS(here::here("data-raw", "address_points", "extract", "address_points.rds"))

# Just construct and keep the full address, plus lat and long
address_points <- address_points_raw %>%
  mutate(
    address = glue::glue("{address} {lfname}"),
    address = as.character(address)
  ) %>%
  as_tibble() %>%
  select(geo_id, address, latitude, longitude)

# Save to SQLite database
lemur_db <- dbConnect(SQLite(), here::here("inst", "extdata", "lemur.sqlite"))
dbWriteTable(lemur_db, "address_points", address_points, overwrite = TRUE)

# Save just addresses for easy searching
address_points_just_address <- address_points[["address"]] %>%
  unique()
address_points_just_address <- address_points_just_address[order(nchar(address_points_just_address), address_points_just_address)]

usethis::use_data(address_points_just_address, overwrite = TRUE)
