library(dplyr)
library(sf)
library(tidyr)
devtools::load_all()

tenant_defense_fund <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "tenant_defense_fund", "geocode", "tenant_defense_fund.rds"))

# Only keep TDFs that have information on potential increase / actual increase
tenant_defense_fund <- tenant_defense_fund %>%
  filter(!is.na(LLPotIncr1) & !is.na(ActualIncr1))

tenant_defense_fund <- tenant_defense_fund %>%
  select(case_number = FileNum, address = address_for_geocoding, bing_address, bing_latitude, bing_longitude, reduced_increase_by = DiffIncr1, tdf_year = Year) %>%
  mutate(tdf_year = readr::parse_number(tdf_year))

tenant_defense_fund <- tenant_defense_fund %>%
  separate(address, into = "address", sep = ", Toronto")

saveRDS(tenant_defense_fund, here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "tenant_defense_fund", "clean", "tenant_defense_fund.rds"))
