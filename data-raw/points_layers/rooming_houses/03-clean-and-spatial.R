# clean rooming houses and mark rooming houses that are no longer licensed and rooming houses that are new

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

rooming_houses_geocoded <- readRDS(here::here("data-raw", "points_layers", "rooming_houses", "geocode", "rooming_houses_geocoded.rds"))

# remove spati


# look at months that license was active and explore if there are large gaps
rooming_houses_continuity <- rooming_houses_geocoded %>%
  # remove 2021 because it is incomplete
  filter(year != "2021") %>%
  rowwise() %>%
  # create sequence of months that license was active
  mutate(
    final_ldate = if_else(is.na(final_ldate), as.Date(in_date + years(1)), as.Date(final_ldate)),
    final_ldate = if_else(final_ldate > "2020-12-31", as.Date("2020-12-01"), final_ldate),
    months_licensed = ifelse(!is.na(final_ldate), list(seq.Date(as.Date(floor_date(in_date, unit = "month")), as.Date(floor_date(final_ldate), unit = "month"), by = "1 month")), list(as.Date(NA_character_)))) %>%
  unnest(months_licensed) %>%
  ungroup() %>%
  group_by(address) %>%
  # create max and min to help with sorting chart
  mutate(last_month_licensed = max(months_licensed),
         first_month_licensed = min(months_licensed)) %>%
  ungroup() %>%
  select(address, months_licensed, last_month_licensed, first_month_licensed, bing_latitude, bing_longitude) %>%
  distinct() %>%
  arrange(last_month_licensed, first_month_licensed, address)


# look at all locations between 2016 and Jan 2020 and highlight the rooming houses that were not active as of that date
# also highlight new rooming house, but we won't go back to 2016 because licensing was incomplete at that time
rooming_houses_cleaned <- rooming_houses_continuity %>%
  filter(months_licensed <= "2020-01-01") %>%
  group_by(address) %>%
  mutate(status = if_else(max(months_licensed) == "2020-01-01", "licensed", "lapsed"),
         status = if_else(min(months_licensed) >= "2018-01-01" & status == "licensed", "licensed after 2018", status)) %>%
  ungroup() %>%
  select(address, first_month_licensed, last_month_licensed, status, bing_latitude, bing_longitude) %>%
  distinct()

# Convert to SF
rooming_houses_sf <- rooming_houses_clean %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326, remove = FALSE)

# Get neighbourhood for each building
rooming_houses_with_neighbourhood <- rooming_houses_sf %>%
  st_join(neighbourhoods) %>%
  select(address, neighbourhood_id = id, neighbourhood, status, first_month_licensed, last_month_licensed)

saveRDS(rooming_houses_with_neighbourhood, here::here("data-raw", "points_layers", "rooming_houses", "clean", "rooming_houses_clean.rds"))

# points are labelled with three status types for display on map: licensed, licensed after 2018, lapsed
# consider aggregating by neighbourhood, but showing only loses as a percentage of rooming houses

