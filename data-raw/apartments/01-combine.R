# Once the apartment_building_registry, apartment_building_evaluation data sets have been created, combine them

library(dplyr)
library(purrr)
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

# Generate tooltip based on information available

generate_tooltip <- function(data) {
  variables <- tribble(
    ~title, ~variable,
    "Built", "year_built",
    "Property management", "property_management",
    "Units", "confirmed_units",
    "RentSafeTO Evaluation", "score_percent"
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
