# Combine AGI with Tenant Defense Fund data, which likely has more complete data / addresses

library(dplyr)
library(tidyr)
library(sf)
library(janitor)
devtools::load_all()

agi_applications <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "clean", "agi_applications.rds"))

tenant_defense_fund <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "tenant_defense_fund", "clean", "tenant_defense_fund.rds"))

tenant_defense_fund <- tenant_defense_fund %>%
  select(case_number, address, bing_address, bing_latitude, bing_longitude, tdf_year, reduced_increase_by) %>%
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
  fill(date_agi_initiated, landlord, .direction = "down") %>%
  ungroup()

# And do the same for the TDF flag - set it to TRUE if any addresses on the file have a TDF
# And fill in tdf_year / reduced_increase_by

agi_applications_tdf <- agi_applications_tdf %>%
  group_by(case_number) %>%
  mutate(tdf = any(tdf, na.rm = TRUE)) %>%
  fill(tdf_year, reduced_increase_by, .direction = "downup") %>%
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

# Combine some inconsistent addresses
agi_applications_tdf %>%
  distinct(address, bing_address) %>%
  get_dupes(bing_address) %>%
  arrange(bing_address)

agi_applications_tdf <- agi_applications_tdf %>%
  mutate(address = case_when(
    address == "10 Roanoke" ~ "10 Roanoke Road",
    address == "11 Dervock Crescent" ~ "11 Dervock Cresc.",
    address == "12 Bexhill Court" ~ "12 Bexhill Crt.",
    address == "1340 Danforth Road" ~ "1340 Danforth Rd.",
    address == "135 Rose Avenue" ~ "135 Rose Ave.",
    address == "1350 Danforth Road" ~ "1350 Danforth Rd.",
    address == "1360 Danforth Road" ~ "1360 Danforth Rd.",
    address == "15 Eva Road" ~ "15 Eva Rd.",
    address == "19 Rosemount" ~ "19 Rosemount Drive",
    address == "2001 Bloor Street West" ~ "2001 Bloor St. W.",
    address == "207 Morningside Avenue" ~ "207 Morningside Ave.",
    address == "21 Vaughan rd." ~ "21 Vaughan Road",
    address == "2493 Lakeshore Blvd" ~ "2493 Lakeshore Blvd W",
    address %in% c("2700 Lawrence Avenue East", "2700 Lawrence Ave. E.") ~ "2700 Lawrence Ave. E",
    address == "2702 Lawrence Avenue East" ~ "2702 Lawrence Ave. E",
    address == "3000 Victoria Park Avenue" ~ "3000 Victoria Park Ave.",
    address == "45 Dunfield Avenue" ~ "45 Dunfield Ave.",
    address == "8 Roanoke" ~ "8 Roanoke Road",
    TRUE ~ address
  ))

# Make spatial
agi_applications_and_tdf <- agi_applications_tdf %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)

# Add neighbourhood

agi_applications_and_tdf <- agi_applications_and_tdf %>%
  st_join(lemur::neighbourhoods)

# Select relevant columns

agi_applications_and_tdf <- agi_applications_and_tdf %>%
  select(case_number, address, bing_address, landlord, neighbourhood, date_agi_initiated, tdf, tdf_year, reduced_increase_by, geometry)

saveRDS(agi_applications_and_tdf, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "clean", "agi_applications_and_tdf.rds"))
