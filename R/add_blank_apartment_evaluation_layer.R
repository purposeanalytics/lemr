#' Add a blank apartment evaluation layer
#'
#' Add an invisible layer of \link{apartment_evaluation} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the apartment building layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_apartment_evaluation_layer() %>%
#'   toggle_layer_visible("apartment_evaluation")
add_blank_apartment_evaluation_layer <- function(map) {
  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_buildings), id = "apartment_evaluation", circle_color = c("get", "score_colour"), circle_blur = 0.5, circle_radius = 6) %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "apartment_evaluation", "visibility", "none") %>%
  # Add tooltips
  mapboxer::add_tooltips(layer_id = "apartment_buildings", "{{{tooltip}}}")
}
