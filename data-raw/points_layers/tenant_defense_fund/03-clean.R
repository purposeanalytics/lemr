library(dplyr)
library(sf)
devtools::load_all()

tenant_defense_fund <- readRDS(here::here("data-raw", "points_layers", "tenant_defense_fund", "geocode", "tenant_defense_fund.rds"))

tenant_defense_fund <- tenant_defense_fund %>%
  select(case_number = FileNum, address = address_for_geocoding, bing_address, bing_latitude, bing_longitude)

saveRDS(tenant_defense_fund, here::here("data-raw", "points_layers", "tenant_defense_fund", "clean", "tenant_defense_fund.rds"))
