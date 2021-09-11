# Read in custom census tabulation, only keep Toronto and Toronto CTs and derive dimension hierarchy

# Retrieved from: https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/7WR7FG
# Exported from IVT file in following order:
# Rows: Geography; Structural Type of Dwelling; Condominium Status; Tenure
# Cols: Number of Bedrooms
# Geographic level for Toronto CTs: Census metropolitan areas (CMAs) and census tracts (CTs)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Set up paths for data sets ------

cts_census_path <- here::here("data-raw", "census_custom_tab_2016_table1", "raw", "TorontoCT_geography_structuraltype_condominium_tenure_Table1.csv")

### Get Toronto census tracts -----

toronto_cts <- read_csv(cts_census_path, guess_max = 100000, col_types = "c")

# ### Tidy names

toronto_cts <- toronto_cts %>%
  clean_names() %>%
  rename(
    structural_type = structural_typ,
    condominium_status = condominium_st,
    tenure_including_subsidy = tenure_includi,
    total = total_number_of_bedrooms,
    one_bedroom = no_bedroom_or_1_bedroom,
    two_plus_bedrooms = x2_or_more_bedrooms
  )

# ### Save Toronto census tracts

saveRDS(toronto_cts, here::here("data-raw", "census_custom_tab_2016_table1", "extract", "toronto_census_tracts.rds"))

