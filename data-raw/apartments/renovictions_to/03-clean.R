# Clean up AGI data

library(dplyr)
library(stringr)
library(tidyr)
library(sf)

agi_applications <- readRDS(here::here("data-raw", "apartments", "renovictions_to", "geocode", "agi_applications.rds"))

agi_applications <- agi_applications %>%
  select(date_agi_initiated = date_initiated, landlord_original = landlord, address = address_for_geocoding, bing_address, bing_latitude, bing_longitude) %>%
  mutate(address = as.character(address))

# There are some addresses that are slightly different (e.g. postal code, or one contains "West") but they get geocoded to the same address
# We want to combine them so that we can see multiple AGI applications at the same address
# Just take the first one
agi_applications_first_address <- agi_applications %>%
  group_by(bing_address) %>%
  arrange(address) %>%
  slice(1) %>%
  ungroup() %>%
  select(bing_address, first_address = address)

agi_applications <- agi_applications %>%
  left_join(agi_applications_first_address, by = "bing_address") %>%
  relocate(first_address, .before = address) %>%
  select(-address) %>%
  rename(address = first_address)

# Clean up landlord

# Clean up property management, retain "original" version
landlords <- agi_applications %>%
  distinct(landlord_original) %>%
  mutate(
    landlord = str_to_title(landlord_original)
  ) %>%
  # Separate out "may be listed as" into another name
  separate(landlord, into = c("landlord", "landlord_alt"), sep = " \\(May Be Listed As ") %>%
  # Separate out "care of" "C/O" etc into another name
  separate(landlord, into = c("landlord", "landlord_care_of"), sep = "C/O |Care Of |c/o ") %>%
  mutate(across(c(landlord, landlord_alt, landlord_care_of), function(x) {
    x %>%
      str_remove_all("\\.|\\,") %>%
      str_remove_all("Limited|Incorporated|Corporation|Inc|Ltd|Corp|Lp") %>%
      str_replace_all("Communites", "Communities") %>%
      str_replace_all("Mangement|Managemet|Mangaement|Managment|Mng|Managent", "Management") %>%
      str_replace_all("Partenership", "Partnership") %>%
      str_replace_all("Proeprties", "Properties") %>%
      str_replace_all(" & ", "&") %>%
      str_replace_all("Canda", "Canada") %>%
      str_squish()
  }),
  landlord_alt = landlord_alt %>%
    str_remove("\\)") %>%
    str_squish()
  )

agi_applications <- agi_applications %>%
  left_join(landlords, by = "landlord_original") %>%
  relocate(landlord, landlord_care_of, landlord_alt, .after = landlord_original) %>%
  select(-landlord_original)

# Make spatial
agi_applications <- agi_applications %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)

usethis::use_data(agi_applications, overwrite = TRUE)
