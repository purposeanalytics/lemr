# Once the apartment_building_registry, apartment_building_evaluation data sets have been created, combine them

library(dplyr)
library(purrr)
library(sf)
library(tidyr)
devtools::load_all()

apartment_building_registry <- readRDS(here::here("data-raw", "points_layers", "apartment_building_registry", "clean", "apartment_building_registry.rds"))

apartment_building_evaluation <- readRDS(here::here("data-raw", "points_layers", "apartment_building_evaluation", "clean", "apartment_building_evaluation.rds"))

agi_applications_and_tdf <- readRDS(here::here("data-raw", "points_layers", "agi_and_tenant_defense_fund", "clean", "agi_applications_and_tdf.rds"))

# Join registry and RentSafeTO -----

buildings <- apartment_building_registry %>%
  mutate(apartment = TRUE) %>%
  # Only joining by RSN, because the registry addresses have ranges but the evaluation addresses do not - use the addresses from registry
  full_join(apartment_building_evaluation, by = "rsn", suffix = c("", "_evaluation")) %>%
  mutate(
    address = coalesce(address, address_evaluation),
    bing_address = coalesce(bing_address, bing_address_evaluation),
    year_built = coalesce(year_built, year_built_evaluation),
    neighbourhood = coalesce(neighbourhood, neighbourhood_evaluation)
  ) %>%
  select(rsn, address, bing_address, neighbourhood, apartment, property_type, year_built, year_registered, units, storeys, property_management, property_management, evaluation_completed_on, score, score_percent, score_bucket, score_colour, geometry) %>%
  mutate(landlord = ifelse(property_management == "Unknown", NA_character_, property_management))

# Get coords instead of geometry column, so we can coalesce more easily
apartment_buildings_coords <- buildings %>%
  st_coordinates() %>%
  as_tibble()

buildings <- buildings %>%
  as_tibble() %>%
  select(-geometry) %>%
  bind_cols(apartment_buildings_coords)

# Add AGI / TDF -----
# Just do AGIs that are in apartments
# For now, just take property management if it's there, and if not, use landlord

# Combine multiple AGI dates, and take the latest non-NA landlord
latest_agi_landlord <- agi_applications_and_tdf %>%
  as_tibble() %>%
  filter(!is.na(landlord)) %>%
  group_by(bing_address) %>%
  filter(date_agi_initiated == max(date_agi_initiated)) %>%
  slice(1) %>%
  distinct(bing_address, landlord) %>%
  ungroup()

agi_applications <- agi_applications_and_tdf %>%
  mutate(agi = TRUE) %>%
  as_tibble() %>%
  mutate(reduced_increase_by = case_when(
    is.na(reduced_increase_by) ~ NA_character_,
    TRUE ~ paste0(round(reduced_increase_by, 2), "%")
  )) %>%
  group_by(agi, bing_address, neighbourhood) %>%
  arrange(desc(date_agi_initiated)) %>%
  summarise(
    geometry = geometry,
    date_agi_initiated = paste(na.omit(unique(date_agi_initiated)), collapse = ", "),
    tdf_year = paste(na.omit(tdf_year), collapse = ", "),
    reduced_increase_by = paste(na.omit(reduced_increase_by), collapse = ", "),
    .groups = "drop"
  ) %>%
  distinct() %>%
  mutate(tdf = tdf_year != "") %>%
  st_as_sf(crs = 4326)

agi_applications <- agi_applications %>%
  left_join(latest_agi_landlord, by = "bing_address")

agi_applications_coords <- agi_applications %>%
  group_by(bing_address) %>%
  slice(1) %>%
  ungroup() %>%
  st_coordinates() %>%
  as_tibble()

agi_applications <- agi_applications %>%
  as_tibble() %>%
  select(-geometry) %>%
  distinct() %>%
  bind_cols(agi_applications_coords)

buildings <- buildings %>%
  as_tibble() %>%
  left_join(agi_applications, by = "bing_address", suffix = c("_apt", "_agi"))

# Fill in columns, prioritizing apt -> agi

buildings <- buildings %>%
  mutate(
    property_management_or_landlord = coalesce(landlord_apt, landlord_agi),
    X = coalesce(X_apt, X_agi),
    Y = coalesce(Y_apt, Y_agi),
    neighbourhood = coalesce(neighbourhood_apt, neighbourhood_agi),
    apartment = coalesce(apartment, FALSE),
    agi = coalesce(agi, FALSE),
    tdf = coalesce(tdf, FALSE),
    tdf_year = na_if(tdf_year, ""),
    reduced_increase_by = na_if(reduced_increase_by, "")
  )

# Select columns -----

buildings <- buildings %>%
  select(rsn, address, bing_address, X, Y, neighbourhood, apartment, property_type, year_built, year_registered, units, storeys, property_management_or_landlord, evaluation_completed_on, score, score_percent, score_bucket, score_colour, agi, date_agi_initiated, tdf, tdf_year, reduced_increase_by)

# Convert to spatial -----

buildings <- buildings %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# Generate tooltip based on information available -----

generate_tooltip <- function(data) {
  variables <- tribble(
    ~title, ~variable,
    "Built", "year_built",
    "Landlord/Management", "property_management_or_landlord",
    "Units", "units",
    "RentSafeTO Evaluation", "score_percent",
    # "Eviction Hearings", "hearings",
    "AGI Application", "date_agi_initiated",
    "Tenant Defence Fund Received", "tdf_year",
    "TDF Reduced Increase By", "reduced_increase_by"
  )

  variables_text <- purrr::map2_chr(
    variables[["title"]], variables[["variable"]],
    function(title, variable) {
      if (is.na(data[[variable]])) {
        ""
      } else {
        glue::glue("<b>{title}</b>: {data[[variable]]}<br>")
      }
    }
  ) %>%
    glue::glue_collapse()

  tooltip <- glue::glue("<b>{data$address}</b><br>{variables_text}")

  tooltip
}

tooltips <- buildings %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  split(.$id) %>%
  map(generate_tooltip) %>%
  unlist()

tooltips <- tibble(tooltip = tooltips)

buildings <- buildings %>%
  bind_cols(tooltips)

usethis::use_data(buildings, overwrite = TRUE)
