# Extract neighbourhoods boundary file

library(opendatatoronto)

neighbourhoods <- list_package_resources("https://open.toronto.ca/dataset/neighbourhoods/") %>%
  get_resource()

# Save resource with date extracted
saveRDS(neighbourhoods, here::here("data-raw", "neighbourhoods", "extract", glue::glue("{Sys.Date()}-neighbourhoods.rds")))
