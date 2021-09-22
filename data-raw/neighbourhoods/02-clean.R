# Clean neighbourhoods file

library(dplyr)
library(tidyr)
library(sf)
devtools::load_all()

neighbourhoods_raw <- read_latest_file(directory = here::here("data-raw", "neighbourhoods", "extract"), suffix = "-neighbourhoods.rds", fileext = "rds")

# Only keep needed columns
neighbourhoods <- neighbourhoods_raw %>%
  select(neighbourhood = AREA_NAME, geometry, id = AREA_SHORT_CODE) %>%
  mutate(id = as.numeric(id))

# Clean up neighbourhood names
neighbourhoods <- neighbourhoods %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

# Projection is already 4326, so good to go
st_crs(neighbourhoods)

# Save dataset - as geojson for mapbox
st_write(neighbourhoods, here::here("data-raw", "neighbourhoods", "final", "neighbourhoods.geojson"))
usethis::use_data(neighbourhoods, overwrite = TRUE)
