#' Add a blank apartment layer
#'
#' Add an invisible layer of \link{apartment_building_registry} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the apartment building layer on and off.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_apartment_layer()
add_blank_apartment_layer <- function(map) {
  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry %>% dplyr::select(.data$bing_address, .data$geometry, .data$confirmed_units)), circle_color = "red", id = "apartment_buildings") %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "apartment_buildings", "visibility", "none") %>%
    # Add tooltips with address and units
    mapboxer::add_tooltips(layer_id = "apartment_buildings", "{{site_address}}<br>Units: {{confirmed_units}}")
}
