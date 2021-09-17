# link corporate landlords file to apartment building registry
# instead of running through geocode, match to apartment building registry addresses

library(dplyr)
library(stringr)
library(tidyr)

# load Sharla's helper code
source(here::here(".", "R", "read_latest_file.R"))

# read corporate landlords file
corporate_landlords <- read_latest_file(directory = here::here("data-raw", "points_layers", "corporate_landlords", "extract"), suffix = "-corporate_landlords.csv")

# read apartment building registry
apartment_building_registry <- read_latest_file(directory = here::here("data-raw", "points_layers", "apartment_building_registry", "extract"),  suffix = "-apartment_building_registry.csv")

# build street code conversion list based on street codes in corporate landlords dataset
street_code_conversion <- tribble(~street_code_full, ~street_code_short,
                                  "AVENUE", "AVE",
                                  "BOULEVARD", "BLVD",
                                  "CIRCLE", "CRCL",
                                  "CIRCUIT", "CRCT",
                                  "COURT", "CRT",
                                  "CRESCENT", "CRES",
                                  "DRIVE", "DR",
                                  "GARDEN", "GDNS",
                                  "GATE", "GT",
                                  "GROVE", "GRV",
                                  "HEIGHTS", "HTS",
                                  "PARKWAY", "PKWY",
                                  "PLACE", "PL",
                                  "ROAD", "RD",
                                  "SQUARE", "SQ",
                                  "STREET", "ST",
                                  "TERRACE", "TER",
                                  "TRAIL", "TRL")

# remove multiple address from units
corporate_landlords <- corporate_landlords %>%
  distinct(`Roll Number`, .keep_all = TRUE) %>%
  select(-Unit, -`Roll Number`) %>%
  distinct()


# create multiple join columns because high and low numbers are not always present in apartment building registry
# apostrophes also appear to be missing, so remove blank spaces and strip punctuation in address points
corporate_landlords_for_join <- corporate_landlords %>%
  clean_names() %>%
  left_join(street_code_conversion, by = c("street_code" = "street_code_full")) %>%
  mutate(street_code_short = coalesce(street_code_short, street_code),
         street_direction_short = case_when(
           street_direction == "NORTH" ~ "N",
           street_direction == "EAST" ~ "E",
           street_direction == "SOUTH" ~ "S",
           street_direction == "WEST" ~ "W",
           TRUE ~ street_direction),
         to_street_number = if_else(to_street_number == 0 | is.na(to_street_number), street_number, to_street_number),
         to_street_number = if_else(to_street_number < street_number, street_number, to_street_number),
         number_range = ifelse(street_number == to_street_number, street_number, glue::glue("{street_number}-{to_street_number}")),
         address_full_join = glue::glue("{number_range}{street_name}{street_code_short}{street_direction_short}", .na = ""),
         address_full_join = str_replace_all(address_full_join, "[^[:alnum:]]", "")) %>%
  rowwise() %>%
  mutate(address_range_join = list(seq(street_number, to_street_number, by = 2))) %>%
  unnest(address_range_join) %>%
  ungroup() %>%
  mutate(
    address_range_join = glue::glue("{address_range_join}{street_name}{street_code_short}{street_direction_short}", .na = ""),
    address_range_join = str_replace_all(address_range_join, "[^[:alnum:]]", "")) %>%
  pivot_longer(cols = c(address_full_join, address_range_join), names_to = "type", values_to = "address_join") %>%
  filter(!is.na(address_join)) %>%
  select(-type) %>%
  distinct(address_join, .keep_all = TRUE)


# prep apartment building address for join
apartment_building_registry_for_join <- apartment_building_registry %>%
  clean_names() %>%
  select(rsn, site_address, prop_management_company_name, property_type) %>%
  mutate(address_full_join = str_replace_all(site_address, "[^[:alnum:]]", ""))

# join
apartment_building_registry_w_landlords <- apartment_building_registry_for_join %>%
  inner_join(corporate_landlords_for_join, by = c("address_full_join" = "address_join")) %>%
  relocate(business_name, .after = prop_management_company_name)

# anti-join for join quality check
anti_apartment_building_registry_w_landlords <- apartment_building_registry_for_join %>%
  anti_join(corporate_landlords_for_join, by = c("address_full_join" = "address_join"))

# seems like a number of the missing joins are truly missing from the corporate_landlords dataset
# TODO: decide if and how we want to use this info
