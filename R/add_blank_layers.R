#' Add blank points layers
#'
#' Add empty layers of \link{buildings}. The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}. This function adds the following layers (accessed via IDs): apartment building registry (apartment_buildings), RentSafeTO scores (apartment_evaluation), evictions hearings (evictions_hearings), Above Guideline Increase applications (agi), and tenant defense fund grants (tdf).
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' map_toronto() %>%
#'   add_blank_points_layers() %>%
#'   toggle_layer_visible("tdf")
add_blank_points_layers <- function(map) {
  blur <- 0
  radius <- 5
  radius <- 4
  opacity <- 0.8
  stroke_colour <- "#FFFFFF"
  stroke_width <- 1

  # Temporarily setting NA score_colour to "none" so we can filter the data in the RentSafeTO layer
  # I can't figure out how to filter out NA/null yet
  data <- lemur::buildings %>%
    dplyr::mutate(score_colour = dplyr::coalesce(.data$score_colour, "none"))

  map %>%
    # All points layers source data ----
    mapboxer::add_source(
      source = mapboxer::as_mapbox_source(data),
      id = "points_data_source"
    ) %>%
    # Apartment buildings ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "apartment_buildings",
      filter = list("==", "apartment", TRUE),
      circle_color = list(
        "case",
        list("==", c("get", "property_type"), "Privately owned"), layer_colours[["apartment_buildings_private"]],
        list("==", c("get", "property_type"), "Toronto Community Housing"), layer_colours[["apartment_buildings_tch"]],
        list("==", c("get", "property_type"), "Social housing"), layer_colours[["apartment_buildings_social_housing"]],
        # Defaults to 'white'
        "white"
      ),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # RentSafeTO Evaluation Scores -----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "apartment_evaluation",
      filter = list("!=", "score_colour", "none"),
      circle_color = c("get", "score_colour"),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # Above guideline increases ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "agi",
      filter = list("==", "agi", TRUE),
      circle_color = layer_colours[["agi"]],
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # Tenant Defence Fund -----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "tdf",
      filter = list("==", "tdf", TRUE),
      circle_color = layer_colours[["tdf"]],
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    )
}

#' Add a blank address layer
#'
#' Add an empty layer to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling highlighting an address, via \link{zoom_map_to_address}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_address_layer()
add_blank_address_layer <- function(map) {
  initial_data <- dplyr::tibble(longitude = "-79.384293", latitude = "43.653908") %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(initial_data), id = "address_points", circle_color = "red", circle_blur = 0.5, visibility = FALSE) %>%
    # Add tooltips with address
    mapboxer::add_tooltips(layer_id = "address_points", "{{address}}")
}

#' Add a blank amenity density layer
#'
#' Add an empty layer of \link{amenity_density} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible} (with the id "amenity_density").
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_amenity_density_layer() %>%
#'   toggle_layer_visible("amenity_density")
add_blank_amenity_density_layer <- function(map) {
  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "mapbox://purposeanalytics.15drt2xq"
    ),
    id = "amenity_density_data"
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = "amenity_density",
        "type" = "fill",
        "source" = "amenity_density_data",
        "source-layer" = "amenity_density-cxezz6",
        "layout" = list(
          "visibility" = "none"
        ),
        "paint" = list(
          "fill-color" = list(
            "case",
            list("==", c("get", "amenity_dense"), "Low"), amenity_density_colours()[["Low"]],
            list("==", c("get", "amenity_dense"), "Medium"), amenity_density_colours()[["Medium"]],
            list("==", c("get", "amenity_dense"), "High"), amenity_density_colours()[["High"]],
            # Defaults to 'white'
            "white"
          ),
          "fill-opacity" = c("get", "alpha"),
          "fill-outline-color" = "black"
        )
      )
    )
}

#' Add a blank LEM layer
#'
#' Add an empty layer of \link{total_affordable_by_neighbourhood} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible} (with the id "lem").
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_lem_layer() %>%
#'   toggle_layer_visible("lem")
add_blank_lem_layer <- function(map) {
  map %>%
    # Add the layer
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(lemur::total_affordable_by_neighbourhood), fill_color = c("get", "colour"), fill_opacity = 0.65, id = "lem")
}

#' Add a blank neighbourhood layer
#'
#' Add a blank layer of \link{neighbourhoods} to a map (created via \link{map_toronto}). The purpose of this function is to allow for zooming and highlighting to a neighbourhood from the data, via \link{zoom_map_to_neighbourhood}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_neighbourhood_layer() %>%
#'   zoom_map_to_neighbourhood("Casa Loma")
add_blank_neighbourhood_layer <- function(map) {
  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "mapbox://purposeanalytics.4juivyoh",
      promoteId = "id"
    ),
    id = "neighbourhoods"
    ) %>%
    # Outline layer
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_line",
        "type" = "line",
        "source" = "neighbourhoods",
        "source-layer" = "neighbourhoods-0jaap1",
        "paint" = list(
          "line-color" = default_line_colour,
          "line-width" = 1.5
        )
      )
    ) %>%
    # Add a "blank" layer for clicking on, that contains all neighbourhoods
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_click",
        "type" = "fill",
        "source" = "neighbourhoods",
        "source-layer" = "neighbourhoods-0jaap1",
        "paint" = list(
          "fill-color" = "white",
          "fill-opacity" = 0
        )
      )
    ) %>%
    # Add an actual layer for neighbourhoods that will be thickened
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_click_line",
        "type" = "line",
        "source" = "neighbourhoods",
        "source-layer" = "neighbourhoods-0jaap1",
        filter = list("==", "neighbourhood", "none"),
        "paint" = list(
          "line-color" = main_colour,
          "line-width" = 5
        )
      )
    ) %>%
    # Hover layer
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_hover_line",
        "type" = "line",
        "source" = "neighbourhoods",
        "source-layer" = "neighbourhoods-0jaap1",
        "paint" = list(
          "line-color" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), main_colour,
            "white"
          ),
          "line-width" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 5,
            1
          ),
          "line-opacity" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 1,
            0
          )
        )
      )
    )
}

#' Add blank rental supply layers
#'
#' Add a blank layers of \link{rental_supply_by_neighbourhood} to a map (created via \link{map_toronto}). There is one layer for primary market (rental_supply_primary), condo (rental_supply_condo), secondary (rental_supply_non_condo), and non-market (rental_supply_non_market). The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_rental_supply_layers() %>%
#'   toggle_layer_visible("rental_supply_condo")
add_blank_rental_supply_layers <- function(map) {

  colors <- dplyr::tibble(color = c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")) %>%
    dplyr::mutate(id = dplyr::row_number())

  rental_supply <- lemur::rental_supply_by_neighbourhood %>%
    dplyr::left_join(colors, by = c("prop_group" = "id")) %>%
    dplyr::select(-prop, -prop_group) %>%
    tidyr::pivot_wider(names_from = group, values_from = color) %>%
    dplyr::left_join(lemur::neighbourhoods, by = "neighbourhood") %>%
    sf::st_as_sf(crs = 4326)

  map %>%
    mapboxer::add_source(mapboxer::as_mapbox_source(rental_supply), id = "rental_supply") %>%
    # Primary
    mapboxer::add_fill_layer(source = "rental_supply", fill_color = c("get", "Primary"), fill_opacity = 0.65, id = "rental_supply_primary", visibility = FALSE) %>%
    # Condo
    mapboxer::add_fill_layer(source = "rental_supply", fill_color = c("get", "Condo"), fill_opacity = 0.65, id = "rental_supply_condo", visibility = FALSE) %>%
    # Non-condo secondary
    mapboxer::add_fill_layer(source = "rental_supply", fill_color = c("get", "Non-Condo"), fill_opacity = 0.65, id = "rental_supply_non_condo", visibility = FALSE) %>%
    # Non-market
    mapboxer::add_fill_layer(source = "rental_supply", fill_color = c("get", "Non-market"), fill_opacity = 0.65, id = "rental_supply_non_market", visibility = FALSE)
}

#' Add blank core housing need layer
#'
#' Add a blank layers of \link{core_housing_need_by_neighbourhood} to a map (created via \link{map_toronto}).The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible} (with id "core_housing_need").
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_core_housing_need_layer() %>%
#'   toggle_layer_visible("core_housing_need")
add_blank_core_housing_need_layer <- function(map) {
  colors <- dplyr::tibble(color = c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")) %>%
    dplyr::mutate(id = dplyr::row_number())

  core_housing_need <- lemur::core_housing_need_by_neighbourhood %>%
    dplyr::left_join(colors, by = c("prop_group" = "id")) %>%
    dplyr::select(-prop, -prop_group) %>%
    dplyr::left_join(lemur::neighbourhoods, by = "neighbourhood") %>%
    sf::st_as_sf(crs = 4326)

  map %>%
    mapboxer::add_source(mapboxer::as_mapbox_source(core_housing_need), id = "core_housing_need_data") %>%
    mapboxer::add_fill_layer(source = "core_housing_need_data", fill_color = c("get", "color"), fill_opacity = 0.65, id = "core_housing_need", visibility = FALSE)
}

#' Add blank evictions layer
#'
#' Add a blank layers of \link{evictions_by_neighbourhood} to a map (created via \link{map_toronto}).The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible} (with id "evictions").
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_evictions_layer() %>%
#'   toggle_layer_visible("evictions")
add_blank_evictions_layer <- function(map) {
  colors <- dplyr::tibble(color = c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")) %>%
    dplyr::mutate(id = dplyr::row_number())

  evictions <- lemur::evictions_by_neighbourhood %>%
    dplyr::left_join(colors, by = c("prop_group" = "id")) %>%
    dplyr::select(-prop, -prop_group) %>%
    dplyr::left_join(lemur::neighbourhoods, by = "neighbourhood") %>%
    sf::st_as_sf(crs = 4326)

  map %>%
    mapboxer::add_source(mapboxer::as_mapbox_source(evictions), id = "evictions_data") %>%
    mapboxer::add_fill_layer(source = "evictions_data", fill_color = c("get", "color"), fill_opacity = 0.65, id = "evictions", visibility = FALSE)
}
