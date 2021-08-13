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
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(initial_data), id = "address_points", circle_color = "red", circle_blur = 0.5) %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "address_points", "visibility", "none") %>%
    # Add tooltips with address
    mapboxer::add_tooltips(layer_id = "address_points", "{{address}}")
}
