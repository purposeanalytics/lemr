library(dplyr)
library(readxl)
library(purrr)
library(janitor)
library(stringr)

files <- fs::dir_ls(here::here("data-raw", "apartments", "tenant_defense_fund", "raw"))

tenant_defense_fund <- files %>%
  map_dfr(function(x) {
    read_excel(x, skip = 3) %>%
      mutate(Year = str_sub(basename(x), -14, -10))
  })

# Remove empty rows
tenant_defense_fund <- tenant_defense_fund %>%
  filter(!is.na(Address))

# Remove rows that contain "Averages" or "Totals" in Address
tenant_defense_fund <- tenant_defense_fund %>%
  filter(!str_detect(Address, "Averages|Totals"))

# Save data
saveRDS(tenant_defense_fund, here::here("data-raw", "apartments", "tenant_defense_fund", "extract", "tenant_defense_fund.rds"))
