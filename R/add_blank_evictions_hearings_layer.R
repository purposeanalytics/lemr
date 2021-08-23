#' Add a blank evictions hearings layer
#'
#' Add an empty layer of \link{apartment_buildings} (with evictions hearings only) to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_evictions_hearings_layer() %>%
#'   toggle_layer_visible("evictions_hearings")
add_blank_evictions_hearings_layer <- function(map) {
  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(apartment_buildings %>%
      dplyr::filter(eviction_hearing)), circle_color = "yellow", circle_opacity = 0.75, id = "evictions_hearings", circle_blur = 0.5, circle_radius = 6) %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "evictions_hearings", "visibility", "none") %>%
    # Add tooltips
    mapboxer::add_popups(layer_id = "evictions_hearings", "{{{tooltip}}}")
}
