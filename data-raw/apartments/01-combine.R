# Once the apartment_building_registry, apartment_building_evaluation data sets have been created, combine them

library(dplyr)
library(purrr)
library(sf)
devtools::load_all()

# Join registry and RentSafeTO -----

apartment_buildings <- apartment_building_registry %>%
  # Only joining by RSN, because the registry addresses have ranges but the evaluation addresses do not - use the addresses from registry
  full_join(apartment_building_evaluation, by = "rsn", suffix = c("", ".y")) %>%
  mutate(
    site_address = coalesce(site_address, site_address.y),
    bing_address = coalesce(bing_address, bing_address.y),
    year_built = coalesce(year_built, year_built.y),
    neighbourhood = coalesce(neighbourhood, neighbourhood.y)
  ) %>%
  select(rsn, site_address, bing_address, neighbourhood, property_type, year_built, year_registered, confirmed_units, confirmed_storeys, property_management, property_management_clean, evaluation_completed_on, score, score_percent, results_of_score, score_colour = color, geometry)

apartment_buildings_coords <- apartment_buildings %>%
  st_coordinates() %>%
  as_tibble()

apartment_buildings <- apartment_buildings %>%
  as_tibble() %>%
  select(-geometry) %>%
  bind_cols(apartment_buildings_coords)

# Add AGI / TDF -----
# Not all AGI are apartments, but we want them in the same data set anyways
# For now, just take property management if it's there, and if not, use landlord

# Combine multiple AGI dates, and take the latest non-NA landlord
latest_agi_landlord <- lemur::agi_applications_and_tdf %>%
  as_tibble() %>%
  filter(!is.na(landlord)) %>%
  group_by(address) %>%
  filter(date_agi_initiated == max(date_agi_initiated)) %>%
  slice(1) %>%
  distinct(address, landlord) %>%
  ungroup()

agi_applications <- lemur::agi_applications_and_tdf %>%
  mutate(agi = TRUE) %>%
  as_tibble() %>%
  group_by(agi, tdf, address, bing_address) %>%
  arrange(desc(date_agi_initiated)) %>%
  summarise(
    geometry = geometry,
    date_agi_initiated = glue::glue_collapse(unique(date_agi_initiated), sep = ", "),
    .groups = "drop"
  ) %>%
  distinct() %>%
  st_as_sf(crs = 4326)

agi_applications <- agi_applications %>%
  left_join(latest_agi_landlord, by = "address")

agi_applications_coords <- agi_applications %>%
  st_coordinates() %>%
  as_tibble()

agi_applications <- agi_applications %>%
  as_tibble() %>%
  select(-geometry) %>%
  bind_cols(agi_applications_coords)

apartment_buildings <- apartment_buildings %>%
  as_tibble() %>%
  full_join(agi_applications, by = "bing_address") %>%
  mutate(
    property_management_or_landlord = case_when(
      property_management_clean == "Unknown" | is.na(property_management_clean) ~ coalesce(landlord, "Unknown"),
      TRUE ~ property_management_clean
    ),
    site_address = coalesce(site_address, bing_address),
    agi = coalesce(agi, FALSE),
    tdf = coalesce(tdf, FALSE),
    X = coalesce(X.x, X.y),
    Y = coalesce(Y.x, Y.y)
  )

apartment_buildings <- apartment_buildings %>%
  relocate(property_management_or_landlord, .before = property_management_clean) %>%
  select(-property_management_clean, -landlord, -X.x, -X.y, -Y.x, -Y.y)

# Add evictions hearings -----

apartment_buildings <- apartment_buildings %>%
  full_join(eviction_hearings %>%
    mutate(eviction_hearing = TRUE), by = "bing_address") %>%
  mutate(
    site_address = coalesce(site_address, address.y),
    agi = coalesce(agi, FALSE),
    tdf = coalesce(tdf, FALSE),
    eviction_hearing = coalesce(eviction_hearing, FALSE),
    X = coalesce(X, bing_longitude),
    Y = coalesce(Y, bing_latitude),
    number_of_hearings = hearings,
    property_management_or_landlord = coalesce(property_management_or_landlord, landlord)
  ) %>%
  select(-address.x, -address.y, -landlord, -bing_latitude, -bing_longitude, -hearings)

# Convert to spatial -----

apartment_buildings <- apartment_buildings %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# Generate tooltip based on information available -----

generate_tooltip <- function(data) {
  variables <- tribble(
    ~title, ~variable,
    "Built", "year_built",
    "Landlord/Management", "property_management_or_landlord",
    "Units", "confirmed_units",
    "RentSafeTO Evaluation", "score_percent",
    "Eviction Hearings", "number_of_hearings",
    "AGI Application", "date_agi_initiated"
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

  tooltip <- glue::glue("<b>{data$site_address}</b><br>{variables_text}")

  if (data$tdf) {
    # Tooltip has <br> at the end so no need to add
    tooltip <- glue::glue("{tooltip}Received tenant defence fund grant")
  }

  tooltip
}

tooltips <- apartment_buildings %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  split(.$id) %>%
  map(generate_tooltip) %>%
  unlist()

tooltips <- tibble(tooltip = tooltips)

apartment_buildings <- apartment_buildings %>%
  bind_cols(tooltips)

usethis::use_data(apartment_buildings, overwrite = TRUE)
