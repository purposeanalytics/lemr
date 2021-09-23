# Process data and definitions

library(readxl)
library(dplyr)
library(janitor)

data_and_definitions <- read_xlsx(here::here("data-raw", "data_and_definitions", "data_and_definitions.xlsx")) %>%
  clean_names() %>%
  arrange(name)

usethis::use_data(data_and_definitions, overwrite = TRUE)
