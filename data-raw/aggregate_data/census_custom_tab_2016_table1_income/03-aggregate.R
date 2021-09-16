# Aggregate census tracts to neighbourhoods
# Create multiple datasets:
# 1. Estimate of condo rental market (primary market and non-condo secondary market to be estimated by difference from Rental Market Survey)
# 2. Structural type by rental tenure

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(forcats)
devtools::load_all()

#### Read data ----
custom_tab_toronto_cts <- readRDS(here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "clean", "custom_tab_toronto_table1.rds"))


## Average income by neighbourhood -----
# Multiply average income by number of households at the CT, aggregate and then divide by number of households at the neighbourhood
average_income_by_neighbourhood <- custom_tab_toronto_cts %>%
  filter(tenure_including_subsidy == "Renter" & household_size != "Total - Household size") %>%
  mutate(total_income = average_total_income_of_households * total_household_income_statistics) %>%
  group_by(neighbourhood, household_size) %>%
  summarize(average_household_income = round(sum(total_income, na.rm = TRUE)/sum(total_household_income_statistics, na.rm = TRUE))) %>%
  rename(group = household_size,
         value = average_household_income)

# City
average_income_city <- custom_tab_toronto_cts %>%
  filter(tenure_including_subsidy == "Renter" & household_size != "Total - Household size") %>%
  mutate(total_income = average_total_income_of_households * total_household_income_statistics) %>%
  group_by(household_size) %>%
  summarize(average_household_income = round(sum(total_income, na.rm = TRUE)/sum(total_household_income_statistics, na.rm = TRUE)))%>%
  rename(group = household_size,
         value = average_household_income)



### Save aggregates ----
saveRDS(average_income_by_neighbourhood, here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "aggregate", "average_income_by_neighbourhood.rds"))
saveRDS(average_income_city, here::here("data-raw", "aggregate_data", "census_custom_tab_2016_table1_income", "aggregate", "average_income_city.rds"))
