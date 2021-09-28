# Clean up corporate landlords data for joining with buildings data

library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
devtools::load_all()

# read corporate landlords file
corporate_landlords <- read_latest_file(directory = here::here("data-raw", "points_layers", "corporate_landlords", "extract"), suffix = "-corporate_landlords.rds", fileext = "rds")

# build street code conversion list based on street codes in corporate landlords dataset
street_code_conversion <- tribble(
  ~street_code_full, ~street_code_short,
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
  "TRAIL", "TRL"
)

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
  mutate(
    street_code_short = coalesce(street_code_short, street_code),
    street_direction_short = case_when(
      street_direction == "NORTH" ~ "N",
      street_direction == "EAST" ~ "E",
      street_direction == "SOUTH" ~ "S",
      street_direction == "WEST" ~ "W",
      TRUE ~ street_direction
    ),
    to_street_number = if_else(to_street_number == 0 | is.na(to_street_number), street_number, to_street_number),
    to_street_number = if_else(to_street_number < street_number, street_number, to_street_number),
    number_range = ifelse(street_number == to_street_number, street_number, glue::glue("{street_number}-{to_street_number}")),
    address_full_join = glue::glue("{number_range}{street_name}{street_code_short}{street_direction_short}", .na = ""),
    address_full_join = str_replace_all(address_full_join, "[^[:alnum:]]", "")
  ) %>%
  rowwise() %>%
  mutate(address_range_join = list(seq(street_number, to_street_number, by = 2))) %>%
  unnest(address_range_join) %>%
  ungroup() %>%
  mutate(
    address_range_join = glue::glue("{address_range_join}{street_name}{street_code_short}{street_direction_short}", .na = ""),
    address_range_join = str_replace_all(address_range_join, "[^[:alnum:]]", "")
  ) %>%
  pivot_longer(cols = c(address_full_join, address_range_join), names_to = "type", values_to = "address_join") %>%
  filter(!is.na(address_join)) %>%
  select(-type) %>%
  distinct(address_join, .keep_all = TRUE)

# Keep only address fields, and business name, clean up business names
corporate_landlords <- corporate_landlords_for_join %>%
  select(address_join, business_name) %>%
  filter(!is.na(business_name)) %>%
  mutate(
    business_name = str_to_title(business_name),
    business_name = str_remove_all(business_name, "\\."),
    business_name = case_when(
      str_starts(business_name, "Toronto Community Housing") ~ "Toronto Community Housing",
      str_starts(business_name, "Akelius") ~ "Akelius Canada Ltd",
      TRUE ~ business_name
    )
  )

saveRDS(corporate_landlords, here::here("data-raw", "points_layers", "corporate_landlords", "clean", "corporate_landlords.rds"))
