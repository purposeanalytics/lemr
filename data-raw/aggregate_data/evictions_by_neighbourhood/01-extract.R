# Extract evictions by neighbourhood
# Source: Wellesley Institute post-processed file, data originally from the Landlord Tenant Board
# File contains list of neighbourhoods, some sociodemogrpahic census variables, and evictions counts from 2010 to 2018
# Evictions by year divided into:
# - L1 evictions which are issued for non-payment of rent
# - L2 evictions which are for non-rent-related issues including landlord own use of dwelling
# - L1 + L2 evictions
# The final column is "FilingRate2016" which is the number of evictions divided by renter households reported in the census in 2016.
# We can use the 2018 numbers and the 2016 census renter households, but for convenience let's stick with 2016 for now

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(sf)
library(stringr)
devtools::load_all()

evictions <- read_xlsx(here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "raw", "Neighbourhood Eviction Distribution - Aug 9 2019.xlsx"))

# Remove unnecessary columns and extra census variables
# For convenience, recalculate 2016 renter household count in case we want to count the rate on other neighbourhoods
evictions_rate <- evictions %>%
  clean_names() %>%
  select(-fid, -shape, -name, -fid2, -shape3, -id4, -name5, -c(neighb_number:neighb_number6)) %>%
  mutate(renter_households = round(l1or_l2_2016 / filing_rate2016), .after = neighbourhood)

saveRDS(evictions_rate, here::here("data-raw", "aggregate_data", "evictions_by_neighbourhood", "extract", "evictions_rate.rds"))
