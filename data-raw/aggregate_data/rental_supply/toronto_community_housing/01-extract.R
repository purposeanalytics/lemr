# Read in Toronto Community Housing unit counts

# Retrieved from: https://open.toronto.ca/dataset/toronto-community-housing-data/
# Includes TCHC developments and buildings larger than 5 units or 6 units or larger.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(opendatatoronto)

toronto_community_housing <- list_package_resources("https://open.toronto.ca/dataset/toronto-community-housing-data/") %>%
  filter(name == "Community Housing Data") %>%
  get_resource()

# ### Tidy names

toronto_community_housing <- toronto_community_housing %>%
  clean_names() %>%
  select(-id, -objectid) %>%
  rename(
    neighbourhood_id = nghbrhd_num,
    building_id = bld_id,
    development_id = dev_id,
    development_name = dev_name,
    police_division = police_div,
    postal_code = pstl_code,
    total_units = ttl_res_unit,
    market_units = mrkt_unit,
    year_built = yr_built,
    building_tyology = bld_typo,
    floors_above_ground = flr_abv_gr,
    building_description = bld_desc
  )

# ### Save Toronto Community Housing buildings
fs::dir_create(here::here("data-raw", "aggregate_data", "rental_supply", "toronto_community_housing", "extract"))
saveRDS(toronto_community_housing, here::here("data-raw", "aggregate_data", "rental_supply", "toronto_community_housing", "extract", "toronto_community_housing.rds"))
