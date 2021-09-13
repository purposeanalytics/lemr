# Read in custom census tabulation, only keep Toronto and Toronto CTs and derive dimension hierarchy

# Retrieved from: https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/SGQVAE
# Exported from IVT file in following order:
# Rows: Geography; Number of Bedrooms; Condominium Status; Tenure
# Cols: Household Size
# Geographic level for Toronto CTs: Census metropolitan areas (CMAs) and census tracts (CTs)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Set up paths for data sets ------

cts_census_path <- here::here("data-raw", "census_custom_tab_2016_table2", "raw", "TorontoCT_geography_bedrooms_condominium_tenure_Table2.csv")

### Get Toronto census tracts -----

toronto_cts <- read_csv(cts_census_path, guess_max = 100000, col_types = "c")

# ### Tidy names

toronto_cts <- toronto_cts %>%
  clean_names() %>%
  rename(
    number_of_bedrooms = number_of_bedr,
    condominium_status = condominium_sta,
    tenure = tenure_3,
    total = total_household_size,
    one_person = x1_person,
    two_person = x2_persons,
    three_person = x3_persons,
    four_person = x4_persons,
    five_or_more_person = x5_or_more_persons
  )

# ### Save Toronto census tracts

saveRDS(toronto_cts, here::here("data-raw", "census_custom_tab_2016_table2", "extract", "custom_tab_toronto_table2.rds"))

