# Extract corporate landlord data, save with date extracted

library(readxl)

## Read data----
corporate_landlords <- read_excel(here::here("data-raw", "points_layers", "corporate_landlords", "raw", "Landlords_Buisness_2021.xlsx"))

# Save resource with date extracted
saveRDS(corporate_landlords, here::here("data-raw", "points_layers", "corporate_landlords", "extract", glue::glue("{Sys.Date()}-corporate_landlords.rds")))

