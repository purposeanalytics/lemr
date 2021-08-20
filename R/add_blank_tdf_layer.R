#' Add a blank Tenant Defence Fund layer
#'
#' Add an empty layer of \link{apartment_buildings} (with tenant defence fund only) to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_tdf_layer() %>%
#'   toggle_layer_visible("tdf")
add_blank_tdf_layer <- function(map) {
  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(apartment_buildings %>%
                                                                     dplyr::filter(tdf)), circle_color = "green", circle_opacity = 0.75, id = "tdf", circle_blur = 0.5, circle_radius = 6) %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "tdf", "visibility", "none") %>%
    # Add tooltips
    mapboxer::add_tooltips(layer_id = "tdf", "{{{tooltip}}}")
}
