# Clean neighbourhoods file

library(dplyr)
library(tidyr)
library(sf)
devtools::load_all()

neighbourhoods_raw <- read_latest_file(directory = here::here("data-raw", "neighbourhoods", "extract"), suffix = "-neighbourhoods.rds", fileext = "rds")

# Only keep needed columns
neighbourhoods <- neighbourhoods_raw %>%
  select(neighbourhood = AREA_NAME, geometry)

# Clean up neighbourhood names - remove any (##)
neighbourhoods <- neighbourhoods %>%
  # Splitting into two "new" columns to avoid regex, and to check against original column
  separate(neighbourhood, into = c("neighbourhood_name", "neighbourhood_number"), sep = " \\([0-9]", remove = FALSE) %>%
  select(neighbourhood = neighbourhood_name, geometry)

# Projection is already 4326, so good to go
st_crs(neighbourhoods)

# Save dataset
usethis::use_data(neighbourhoods, overwrite = TRUE)
