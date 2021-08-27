add_blank_points_layers <- function(map) {
  blur <- 0.5
  radius <- 5

  # Temporarily setting NA score_colour to "none" so we can filter the data in the RentSafeTO layer
  # I can't figure out how to filter out NA/null yet
  data <- lemur::buildings %>%
    dplyr::mutate(score_colour = dplyr::coalesce(score_colour, "none"))

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
      circle_radius = radius,
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
      circle_radius = radius,
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
      circle_radius = radius,
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
      circle_radius = radius,
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
      circle_radius = radius,
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

  # Colours
  amenity_density_colours <- tibble::tribble(
    ~amenity_dense, ~colour,
    "Low", low_colour,
    "Medium", accent_colour,
    "High", high_colour,
    "Unknown", "white"
  )

  amenity_density <- amenity_density %>%
    dplyr::left_join(amenity_density_colours, by = "amenity_dense")

  # Alphas / opacity
  n <- 20

  amenity_density_alpha <- amenity_density %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(.data$dbuid, .data$population) %>%
    dplyr::mutate(
      log_population = log(.data$population + 1),
      log_population_group = cut(.data$log_population, breaks = n)
    )

  log_population_groups <- amenity_density_alpha %>%
    dplyr::distinct(.data$log_population_group)

  alphas <- seq(0.1, 0.8, length.out = nrow(log_population_groups))

  log_population_groups <- log_population_groups %>%
    dplyr::mutate(alpha = alphas)

  amenity_density_alpha <- amenity_density_alpha %>%
    dplyr::left_join(log_population_groups, by = "log_population_group") %>%
    dplyr::select(.data$dbuid, .data$alpha)

  amenity_density <- amenity_density %>%
    dplyr::left_join(amenity_density_alpha, by = "dbuid")

  map %>%
    # Add the layer
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(amenity_density), fill_color = c("get", "colour"), fill_opacity = c("get", "alpha"), fill_outline_color = "black", id = "amenity_density", visibility = FALSE)
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
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(total_affordable_by_neighbourhood), fill_color = c("get", "colour"), fill_opacity = 0.75, id = "lem") %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "lem", "visibility", "none")
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
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), line_color = main_colour, line_width = 2, id = "neighbourhood_line") %>%
    # Add a "blank" layer for clicking on, that contains all neighbourhoods
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), fill_color = main_colour, fill_opacity = 0, id = "neighbourhood_click") %>%
    # Add an actual layer for neighbourhoods that will be thickened
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), line_color = main_colour, line_width = 5, id = "neighbourhood_click_line", filter = list("==", "neighbourhood", "none"))
}
