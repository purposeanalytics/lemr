# Final cleaning of apartment building registry, convert to spatial

library(sf)
library(janitor)
library(stringr)
library(dplyr)
devtools::load_all()

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartments", "apartment_building_registry", "geocode_clean"), suffix = "-apartment_building_registry_geocoded_clean.rds", fileext = "rds")

# Move address fields to start, clean up column names
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  clean_names() %>%
  select(id, site_address, starts_with("bing"), everything()) %>%
  select(-bing_status_code, -bing_method, -bing_confidence, -address_geocode_error, -address_for_geocoding)

# Clean up original address
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(
    site_address = str_squish(site_address),
    site_address = str_to_title(site_address)
  )

# Clean up property management, retain "original" version
property_mgmt <- apartment_building_registry_geocoded %>%
  distinct(prop_management_company_name) %>%
  mutate(
    property_management = str_to_title(prop_management_company_name),
    property_management_clean = property_management %>%
      str_remove_all("\\.|\\,") %>%
      str_remove_all("Limited|Incorporated|Corporation|Inc|Ltd|Corp|Lp") %>%
      str_replace_all("Communites", "Communities") %>%
      str_replace_all("Mangement|Managemet|Mangaement|Managment|Mng|Managent", "Management") %>%
      str_replace_all("Partenership", "Partnership") %>%
      str_replace_all("Proeprties", "Properties") %>%
      str_replace_all(" & ", "&") %>%
      str_replace_all("Canda", "Canada") %>%
      str_squish()
  )

mgmt_fixes <- tribble(
  ~wrong, ~correct,
  "Wj Propertiesca", "Wj Properties",
  "Preston Group - Toronto On", "Preston Group",
  "Penwoth Holdings", "Penworth Holdings",
  "Morguar", "Morguard",
  "Linfred Investment", "Linfred Investments",
  "King's College Management", "Kings College Management",
  "Akeliuscanadaltd", "Akelius Canada",
  "Orchard Apartments (Bloor)", "Orchard Apartments",
  "Orchard Apartments (307)", "Orchard Apartments",
  "Orchard Apartments (2328)", "Orchard Apartments",
  "Aiderbook Management", "Aiderbrook Management",
  "Amk2000 Holdings", "Amk 2000 Holdings",
  "Bentall Kennedy Canadace", "Bentall Kennedy (Canada)",
  "Bentallgreenoak (Canada)", "Bentallgreenoak",
  "Cromewll Management", "Cromwell Management",
  "Kpm Property Management", "Kpm",
  "Megapro X Properties", "Megapro Properties",
  "Royallepage", "Royal Lepage Signature Realty"
)

property_mgmt <- property_mgmt %>%
  left_join(mgmt_fixes, by = c("property_management" = "wrong")) %>%
  mutate(
    property_management_clean = coalesce(correct, property_management_clean),
    property_management_clean = case_when(
      property_management %in% c("Not Applicable", "None", "N/A") ~ NA_character_,
      str_starts(property_management_clean, "Metcap") ~ "Metcap",
      str_starts(property_management_clean, "Aykler") ~ "Aykler",
      str_starts(property_management_clean, "Cogir") ~ "Cogir",
      str_starts(property_management_clean, "Greenwin") ~ "Greenwin",
      str_starts(property_management_clean, "Morguard") ~ "Morguard",
      TRUE ~ property_management_clean
    )
  ) %>%
  select(-correct)

property_mgmt <- property_mgmt %>%
  mutate(across(
    c(property_management, property_management_clean),
    function(x) {
      x %>%
        str_replace("^Tch$", "TCH") %>%
        coalesce("Unknown")
    }
  ))

apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  left_join(property_mgmt, by = "prop_management_company_name") %>%
  select(-prop_management_company_name)

# Convert to SF
apartment_building_registry_sf <- apartment_building_registry_geocoded %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326, remove = FALSE)

# Get neighbourhood for each building
apartment_with_neighbourhood <- apartment_building_registry_sf %>%
  st_intersection(neighbourhoods) %>%
  as_tibble() %>%
  select(id, neighbourhood)

apartment_building_registry <- apartment_building_registry_sf %>%
  left_join(apartment_with_neighbourhood, by = "id") %>%
  select(id, site_address, starts_with("bing"), neighbourhood, everything())

# Keep relevant columns

apartment_building_registry <- apartment_building_registry %>%
  select(id, rsn, site_address, bing_address, neighbourhood, confirmed_units, confirmed_storeys, year_built, property_management, property_management_clean, geometry)

# Check values
apartment_building_registry %>%
  filter(is.na(year_built))

# Can't really find data on year built, so will have to go with "Unknown"

apartment_building_registry %>%
  filter(confirmed_units < 10 | is.na(confirmed_units))

apartment_building_registry %>%
  filter(confirmed_storeys < 3 | is.na(confirmed_storeys))

# Flag as NA

apartment_building_registry <- apartment_building_registry %>%
  mutate(across(
    c(confirmed_units, confirmed_storeys),
    function(x) {
      case_when(
        cur_data()[["rsn"]] %in% c(4820405, 4904686) ~ NA_real_,
        TRUE ~ x
      )
    }
  ))

# Save as data set in package
usethis::use_data(apartment_building_registry, overwrite = TRUE)
