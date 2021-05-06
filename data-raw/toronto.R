library(opendatatoronto)

toronto <- list_package_resources("https://open.toronto.ca/dataset/regional-municipal-boundary/") %>%
  get_resource()

usethis::use_data(toronto, overwrite = TRUE)
