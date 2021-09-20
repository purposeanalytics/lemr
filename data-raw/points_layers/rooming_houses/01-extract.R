# Extract rooming house licenses from past years
# Source: City of Toronto, requested by Richard Marshall

# Dataset contains list of licensed rooming houses and license renewal history. Licenses are only permitted in the old city of Toronto.
# The licenses themselves sometimes span multiple years. The "In date" "Final ldate" fields can be used to determine the continuous
# period with which the license was kept.

library(dplyr)
library(readxl)

rooming_houses <- read_excel(here::here("data-raw", "points_layers", "rooming_houses", "raw", "2021-00260 original COT licensed rooming houses .xlsx"), sheet = 2, skip = 4)

saveRDS(rooming_houses, here::here("data-raw", "points_layers", "rooming_houses", "extract", "rooming_houses.rds"))
