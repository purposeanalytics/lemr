#' Add a blank LEM layer
#'
#' Add an empty layer of \link{total_affordable_by_neighbourhood} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
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
