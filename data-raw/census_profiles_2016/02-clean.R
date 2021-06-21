# Clean census profiles data

library(sf)
library(dplyr)
library(stringr)
devtools::load_all()

### Data ----

toronto_census_tracts <- readRDS(here::here("data-raw", "census_profiles_2016", "extract", "toronto_census_tracts.rds"))

# Read file for converting census tract to neighbourhood
geo_to_neighbourhood <- st_read(here::here("data-raw", "census_profiles_2016", "raw", "Census Geographies to TO Neighbourhoods.gpkg"))

# Just select relevant columns
ct_to_neighbourhood <- geo_to_neighbourhood %>%
  select(ct = CTUID, neighbourhood = AREA_NAME) %>%
  as_tibble() %>%
  select(-geom)

# ### Selecting columns ----

# Keep Toronto (535) because we want to compare neighbourhoods to city numbers
toronto_census_tracts <- toronto_census_tracts %>%
  select(-census_year, -geo_level, -geo_name, -data_quality_flag, -dim_profile_of_census_tracts_2247, -notes_profile_of_census_tracts_2247, -geo_code_por, -member_id, -gnr, -gnr_lf) # Keep alt_geo_code instead because it'll be easier to consistently add decimals to

# ### Convert geo code to decimal format ----
# e.g. 535000100 -> "5350001.00", 535000701 -> "5350007.01"

toronto_census_tracts <- toronto_census_tracts %>%
  mutate(
    before_decimal = str_sub(alt_geo_code, start = 1, end = 7),
    after_decimal = str_sub(alt_geo_code, start = 8, end = 9),
    geo_code = glue::glue("{before_decimal}.{after_decimal}"),
    geo_code = ifelse(geo_code == "535.", "535", as.character(geo_code))
  ) %>%
  select(-alt_geo_code, -before_decimal, -after_decimal)

### Clean neighbourhoods names -----

ct_to_neighbourhood <- ct_to_neighbourhood %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

### Only keep CTs in Toronto proper -----

# Keep toronto itself

toronto_dimensions <- toronto_census_tracts %>%
  filter(geo_code == "535")

# Then neighbourhoods matching with CT -> Neighbourhoods file

toronto_census_tracts <- toronto_census_tracts %>%
  inner_join(ct_to_neighbourhood, by = c("geo_code" = "ct"))

# And recombine

toronto_census_tracts <- toronto_census_tracts %>%
  bind_rows(toronto_dimensions)

### Construct NAs in total / male / female -----

# total, male, and female have values:
# * ... for not available / applicable
# * x for suppressed due to small counts
# * F for unreliable

# These should all be converted to NA, and then the fields should be converted to numeric

toronto_census_tracts <- toronto_census_tracts %>%
  mutate(
    across(c(total, male, female), ~ case_when(
      .x %in% c("...", "x", "F") ~ NA_character_,
      TRUE ~ .x
    )),
    across(c(total, male, female), as.numeric)
  )

### Order variables ----
toronto_census_tracts <- toronto_census_tracts %>%
  select(dimension_full, dimension, parent_id, dimension_id, geo_code, neighbourhood, total, female, male)

### Save data ----
saveRDS(toronto_census_tracts, here::here("data-raw", "census_profiles_2016", "clean", "census_profiles_toronto_cts.rds"))
