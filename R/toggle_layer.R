#' Toggle mapbox layer visible or invisible
#'
#' @param map Map e.g. from \link{map_toronto}
#' @param id ID of existing layer, added via e.g. \link{add_blank_apartment_layer}
#'
#' @export
#' @rdname toggle_layer
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_amenity_density_layer() %>%
#'   toggle_layer_visible("amenity_density")
toggle_layer_visible <- function(map, id) {
  map %>%
    # Set the visibility to visible
    mapboxer::set_layout_property(layer_id = id, "visibility", "visible")
}

#' @rdname toggle_layer
toggle_layer_invisible <- function(map, id) {
  map %>%
    # Set the visibility to "none"
    mapboxer::set_layout_property(layer_id = id, "visibility", "none")
}
