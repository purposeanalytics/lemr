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
