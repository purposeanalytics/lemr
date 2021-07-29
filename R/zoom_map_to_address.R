#' Zoom map of Toronto to address
#'
#' @param map Map created by \link{map_toronto}
#' @param address Address from \link{address_points()}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_apartment_layer() %>%
#'   zoom_map_to_address("378 Markham St")
zoom_map_to_address <- function(map, address) {
  browser()
  searched_address <- lemur::apartment_building_registry %>%
    dplyr::filter(.data$bing_address == address)

  map %>%
    # UPDATE: for now, don't highlight address - just zoom to it. I want to decouple the address search from the apartment layer.

    # Update the data in the "apartment_buildings" layer to be for this address, so that that point is highlighted in red
    # mapboxer::set_filter(layer_id = "apartment_buildings", list("==", "bing_address", address)) %>%
    # mapboxer::set_data(data = searched_address, source_id = "apartment_buildings") %>%
    # Set the visibility to visible
    # mapboxer::set_layout_property(layer_id = "apartment_buildings", "visibility", "visible") %>%
    # Zoom to the address
    mapboxer::fit_bounds(sf::st_bbox(searched_address), maxZoom = 15, pitch = 0, bearing = -15)
}
