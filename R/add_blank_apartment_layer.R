#' Add a blank apartment layer
#'
#' Add a blank layer of \link{apartment_building_registry} to a map (created via \link{map_toronto}). The purpose of this function is to allow for zooming and highlighting to an address from the data, via \link{zoom_map_to_address}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_apartment_layer() %>%
#'   zoom_map_to_address("378 Markham St")
add_blank_apartment_layer <- function(map) {
  map %>%
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry %>% dplyr::select(.data$bing_address, .data$geometry, .data$confirmed_units)), circle_color = "red", id = "apartment_building_searched", filter = list("==", "bing_address", "none")) %>%
    mapboxer::add_tooltips(layer_id = "apartment_building_searched", "{{bing_address}}<br>Units: {{confirmed_units}}")
}
