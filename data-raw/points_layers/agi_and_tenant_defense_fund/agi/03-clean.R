# Clean up AGI data

library(dplyr)
library(stringr)
library(tidyr)
library(sf)

agi_applications <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "geocode", "agi_applications.rds"))

agi_applications <- agi_applications %>%
  separate(address_for_geocoding, into = "address", sep = ", Toronto") %>%
  mutate(address = case_when(address == "NA Crestview Apts (leacrest & Mallory)" ~ "30-75 Leacrest Rd", TRUE ~ address)) %>%
  select(case_number, date_agi_initiated = date_initiated, landlord_original = landlord, address, bing_address, bing_latitude, bing_longitude)

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
  select(-landlord_original, -landlord_care_of, -landlord_alt)

# Remove one AGI
# 4085 used to be owned by GDH Apartment Group (who own 4087, which is a private market apartment) but is now owned by Mainstay and marked as Social Housing in the registry.
# It is unlikely that this AGI is still in effect, since it has moved from private to non-market
# So just remove from here

agi_applications <- agi_applications %>%
  filter(address != "4085 Bathurst Street")

saveRDS(agi_applications, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "agi", "clean", "agi_applications.rds"))
