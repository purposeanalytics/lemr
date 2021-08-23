# Combine with Tenant Defense Fund data, which likely has more complete data / addresses

library(dplyr)
library(tidyr)
library(sf)

agi_applications <- readRDS(here::here("data-raw", "points_layers", "renovictions_to", "clean", "agi_applications.rds"))

tenant_defense_fund <- readRDS(here::here("data-raw", "points_layers", "tenant_defense_fund", "clean", "tenant_defense_fund.rds"))

tenant_defense_fund <- tenant_defense_fund %>%
  select(case_number, address, bing_address, bing_latitude, bing_longitude) %>%
  mutate(tdf = TRUE)

# Combine all files
agi_applications_tdf <- agi_applications %>%
  full_join(tenant_defense_fund, by = c("case_number", "bing_address"), suffix = c("_agi", "_tdf"))

# If an address is in TDF but not AGI, but its case number IS in AGI (with a different address)
# We want to fill in the remaining data - AGI, landlord, etc
# So it's attached to both addresses

agi_applications_tdf <- agi_applications_tdf %>%
  group_by(case_number) %>%
  arrange(address_agi) %>%
  fill(date_agi_initiated, landlord, landlord_care_of, landlord_alt, .direction = "down") %>%
  ungroup()

# And do the same for the TDF flag - set it to TRUE if any addresses on the file have a TDF

agi_applications_tdf <- agi_applications_tdf %>%
  group_by(case_number) %>%
  mutate(tdf = any(tdf, na.rm = TRUE)) %>%
  ungroup()

# Fill in missing fields
agi_applications_tdf <- agi_applications_tdf %>%
  as_tibble() %>%
  mutate(
    address = coalesce(address_agi, address_tdf),
    bing_latitude = coalesce(bing_latitude_agi, bing_latitude_tdf),
    bing_longitude = coalesce(bing_longitude_agi, bing_longitude_tdf)
  ) %>%
  select(-ends_with("_agi"), -ends_with("_tdf"))

# Make spatial
agi_applications_and_tdf <-  agi_applications_tdf %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)

usethis::use_data(agi_applications_and_tdf, overwrite = TRUE)
