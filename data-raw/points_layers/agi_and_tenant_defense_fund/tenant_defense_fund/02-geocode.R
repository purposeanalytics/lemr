library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(purrr)
library(progress)
devtools::load_all()

tenant_defense_fund <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "tenant_defense_fund", "extract", "tenant_defense_fund.rds"))

# Set correct data types
tenant_defense_fund <- tenant_defense_fund %>%
  mutate(across(c(AffectedUnits, SavingsCalc, sumunits), as.numeric))

# Add ID for updating if geocoding issues
tenant_defense_fund <- tenant_defense_fund %>%
  mutate(id = row_number())

# Add derived fields for number of buildings & avg rent per filenum
tenant_defense_fund <- tenant_defense_fund %>%
  group_by(FileNum) %>%
  mutate(
    TotalBuildings = n(),
    TotalRent = SavingsCalc / DiffIncr1 * 100 / 12,
    AvgRentPerUnit = TotalRent / sumunits,
    FileNum = toupper(FileNum)
  ) %>%
  ungroup() %>%
  select(-TotalRent)

# clean up addresses into street number and street name
tenant_defense_fund_address <- tenant_defense_fund %>%
  # create temp address so that we don't lose original field when we separate_rows
  # add comma to "99 Howard St. 670 Parliament" so that it will split into two rows
  mutate(
    AddressTemp = Address,
    AddressTemp = str_replace(AddressTemp, "99 Howard St. 670 Parliament", "99 Howard St., 670 Parliament")
  ) %>%
  # separate multiple addresses into individual rows
  separate_rows(AddressTemp, sep = ",") %>%
  separate_rows(AddressTemp, sep = " and ") %>%
  separate_rows(AddressTemp, sep = "&") %>%
  # break out street number ranges into individual rows with some regex
  rowwise() %>%
  mutate(
    LowNumber = str_extract(AddressTemp, "[[:digit:]]+"),
    LowNumber = if_else(str_detect(AddressTemp, "[[:digit:]]-[[:digit:]]"), str_extract(AddressTemp, "([[:digit:]].*?)(?=-)"), LowNumber),
    LowNumber = as.integer(LowNumber),
    HighNumber = str_extract(AddressTemp, "[[:digit:]]+"),
    HighNumber = if_else(str_detect(AddressTemp, "[[:digit:]]-[[:digit:]]"), str_extract(AddressTemp, "(?<=-)([[:digit:]]*)"), HighNumber),
    HighNumber = as.integer(HighNumber),
    StreetNumber = ifelse(!is.na(LowNumber), list(seq(LowNumber, HighNumber, by = 2)), list(NA_integer_)),
    .after = Address
  ) %>%
  unnest(StreetNumber) %>%
  ungroup() %>%
  mutate(
    StreetNumber = as.character(StreetNumber),
    StreetName = str_replace_all(AddressTemp, "[[:digit:]]?-?[[:digit:]]", ""),
    StreetName = str_squish(StreetName),
    StreetName = if_else(StreetName == "", NA_character_, StreetName),
    StreetName = if_else(nchar(StreetName) <= 4, NA_character_, StreetName),
    StreetNumber = if_else(str_detect(AddressTemp, "[[:digit:]][[:alpha:]]"), AddressTemp, StreetNumber),
    StreetNumber = str_squish(StreetNumber),
    .after = StreetNumber
  ) %>%
  fill(StreetName, .direction = "down") %>%
  filter(!is.na(StreetNumber)) %>%
  select(-AddressTemp, -LowNumber, -HighNumber)

tenant_defense_fund_address <- tenant_defense_fund_address %>%
  # Combine address, city (Toronto, ON), and FSA for better results when geocoding
  mutate(address_for_geocoding = glue::glue("{StreetNumber} {StreetName}, Toronto, ON"))

# Clean up some troublesome addresses first
tenant_defense_fund_address <- tenant_defense_fund_address %>%
  mutate(address_for_geocoding = case_when(
    address_for_geocoding == "35 Canyon, Toronto, ON" ~ "35 Canyon Ave, Toronto, ON M3H 4Y2",
    address_for_geocoding == "16 St. Joseph, Toronto, ON" ~ "16 St Joseph St Toronto, ON M4Y 1J9",
    address_for_geocoding == "100 Parkway Forest Dr., Toronto, ON" ~ "100 Parkway Forest Dr, Toronto, ON M2J 1L6",
    address_for_geocoding == "10 San Romano Way, Toronto, ON" ~ "10 San Romanoway, North York, ON M3N 2Y2",
    address_for_geocoding == "60 Oakmount Ave., Toronto, ON" ~ "60 Oakmount Rd, Toronto, ON M6P 2N2",
    address_for_geocoding == "1002 Lawrence Ave., Toronto, ON" ~ "1002 Lawrence Ave E, Toronto, ON",
    address_for_geocoding %in% c("1598 Bathurst St., Toronto, ON", "1598A Bathurst St., Toronto, ON") ~ "1596-1598 Bathurst St, Toronto, ON",
    address_for_geocoding == "25 Widdicombe Hill Blvd., Toronto, ON" ~ "25 Widdicombe Hill, Toronto, ON",
    address_for_geocoding == "35 Widdicombe Hill Blvd., Toronto, ON" ~ "35 Widdicombe Hill, Toronto, ON",
    address_for_geocoding == "4 Bexhill Crt., Toronto, ON" ~ "4 Bexhill Crt, Toronto, ON M9A 3A8",
    TRUE ~ as.character(address_for_geocoding)
  ))

# Geocode

# Iterate through addresses - function automatically waits 0.25 seconds between calls to abide by license
# Using a progress bar to say how far along we are
pb <- progress_bar$new(total = nrow(tenant_defense_fund_address))

# And a "safe" version in case there's errors!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

tenant_defense_fund_address_geocoded <- tenant_defense_fund_address %>%
  mutate(
    address_geocode = map(address_for_geocoding, function(x) {
      pb$tick()
      safely_geocode_address(x)
    })
  )

# Separate results from errors
tenant_defense_fund_address_geocoded <- tenant_defense_fund_address_geocoded %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  )

# Unnest results
tenant_defense_fund_address_geocoded <- tenant_defense_fund_address_geocoded %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

# Fix addresses of some that didn't work on first go
tenant_defense_fund_address_geocoded_missing <- tenant_defense_fund_address_geocoded %>%
  filter(bing_status_code == "404" | bing_confidence == "Low") %>%
  select(id, address_for_geocoding) %>%
  mutate(address_geocode = map(address_for_geocoding, function(x) {
    safely_geocode_address(x)
  })) %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-tidyselect::any_of("address_geocode"))

tenant_defense_fund_address_geocoded <- tenant_defense_fund_address_geocoded %>%
  mutate(id = case_when(address_for_geocoding == "1596-1598 Bathurst St, Toronto, ON" & StreetNumber == "1598A" ~ 99999L, TRUE ~ id)) %>%
  rows_update(tenant_defense_fund_address_geocoded_missing, by = c("id", "address_for_geocoding"))

# Save
saveRDS(tenant_defense_fund_address_geocoded, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "tenant_defense_fund", "geocode", "tenant_defense_fund.rds"))
