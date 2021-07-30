#' Add a blank amenity density layer
#'
#' Add an empty layer of \link{amenity_density} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_amenity_density_layer()
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
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(amenity_density), fill_color = c("get", "colour"), fill_opacity = c("get", "alpha"), fill_outline_color = "black", id = "amenity_density") %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "amenity_density", "visibility", "none")
}
