# Read in custom census tabulation, only keep Toronto and Toronto CTs and derive dimension hierarchy

# Retrieved from: https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/7WR7FG
# Exported from IVT file in following order:
# Rows: Geography; Tenure; Household Size
# Cols: Household Income
# Geographic level for Toronto CTs: Census metropolitan areas (CMAs) and census tracts (CTs)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Set up paths for data sets ------

cts_census_path <- here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "raw", "TorontoCT_geography_tenure_householdsize_householdincome_Table1.csv")

### Get Toronto census tracts -----

toronto_cts <- read_csv(cts_census_path, guess_max = 100000, col_types = "c")

# ### Tidy names

toronto_cts <- toronto_cts %>%
  clean_names() %>%
  rename(
    tenure_including_subsidy = tenure_includi
  )

# ### Save Toronto census tracts

saveRDS(toronto_cts, here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "extract", "custom_tab_toronto_table1.rds"))

