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
      circle_color = layer_colours[["apartment_buildings"]],
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
    # Evictions hearings ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "evictions_hearings",
      filter = list("==", "eviction_hearing", TRUE),
      circle_color = layer_colours[["evictions_hearings"]],
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
          "fill-color" = c("get", "colour"),
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
