library(opendatatoronto)
library(dplyr)
library(janitor)

address_points <- list_package_resources("https://open.toronto.ca/dataset/address-points-municipal-toronto-one-address-repository/") %>%
  filter(name == "Municipal address points (wgs84) - shapefile") %>%
  get_resource() %>%
  clean_names()

saveRDS(address_points, here::here("data-raw", "address_points", "extract", "address_points.rds"))
