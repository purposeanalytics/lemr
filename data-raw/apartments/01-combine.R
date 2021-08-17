# Once the apartment_building_registry, apartment_building_evaluation data sets have been created, combine them

library(dplyr)
library(purrr)
library(sf)
devtools::load_all()

apartment_buildings <- apartment_building_registry %>%
  # Only joining by RSN, because the registry addresses have ranges but the evaluation addresses do not - use the addresses from registry
  full_join(apartment_building_evaluation, by = "rsn", suffix = c("", ".y")) %>%
  mutate(
    site_address = coalesce(site_address, site_address.y),
    bing_address = coalesce(bing_address, bing_address.y),
    year_built = coalesce(year_built, year_built.y),
    neighbourhood = coalesce(neighbourhood, neighbourhood.y)
  ) %>%
  select(rsn, site_address, bing_address, neighbourhood, property_type, year_built, year_registered, confirmed_units, confirmed_storeys, property_management, property_management_clean, evaluation_completed_on, score, score_percent, results_of_score, score_colour = color)

# Add AGI
# Not all AGI are apartments, but we want them in the same data set anyways
# For now, just take property management if it's there, and if not, use landlord

# Combine multiple AGI dates, and take the latest non-NA landlord
latest_agi_landlord <- lemur::agi_applications %>%
  as_tibble() %>%
  filter(!is.na(landlord)) %>%
  group_by(address) %>%
  filter(date_agi_initiated == max(date_agi_initiated)) %>%
  slice(1) %>%
  distinct(address, landlord) %>%
  ungroup()

agi_applications <- lemur::agi_applications %>%
  mutate(agi = TRUE) %>%
  as_tibble() %>%
  group_by(agi, address, bing_address) %>%
  arrange(desc(date_agi_initiated)) %>%
  summarise(
    geometry = geometry,
    date_agi_initiated = glue::glue_collapse(unique(date_agi_initiated), sep = ", "),
    .groups = "drop"
  ) %>%
  distinct()

agi_applications <- agi_applications %>%
  left_join(latest_agi_landlord, by = "address")

apartment_buildings <- apartment_buildings %>%
  as_tibble() %>%
  full_join(agi_applications, by = "bing_address") %>%
  mutate(
    geometry = coalesce(geometry.x, geometry.y),
    property_management_or_landlord = case_when(
      property_management_clean == "Unknown" | is.na(property_management_clean) ~ coalesce(landlord, "Unknown"),
      TRUE ~ property_management_clean
    ),
    agi = coalesce(agi, FALSE)
  )

apartment_buildings <- apartment_buildings %>%
  relocate(property_management_or_landlord, .before = property_management_clean) %>%
  select(-property_management_clean, -landlord, -geometry.x, -geometry.y) %>%
  st_as_sf(crs = 4326)

# Generate tooltip based on information available

generate_tooltip <- function(data) {
  variables <- tribble(
    ~title, ~variable,
    "Built", "year_built",
    "Landlord/Management", "property_management_or_landlord",
    "Units", "confirmed_units",
    "RentSafeTO Evaluation", "score_percent",
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

  glue::glue("<b>{data$site_address}</b><br>{variables_text}")
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
