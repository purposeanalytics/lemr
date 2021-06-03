#' Zoom map of Toronto to address
#'
#' @param map Map created by \link{map_toronto}
#' @param address Address from \link{apartment_building_registry}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>% zoom_map_to_address("378 Markham St")
zoom_map_to_address <- function(map, address) {
  searched_address <- lemur::apartment_building_registry %>%
    dplyr::filter(.data$bing_address == address)

  if (nrow(searched_address) == 0) {
    stop("Address not found in `apartment_building_registry`", call. = FALSE)
  }

  # TODO: maybe need to add the layer first, or check if it exists... otherwise it won't update the data shown if you search a different address

  map %>%
    # Zoom to the address
    mapboxer::fit_bounds(sf::st_bbox(searched_address), maxZoom = 15, pitch = 0, bearing = -15) %>%
    # Filter the "apartment_building_searched" layer to be for this address, so that that point is highlighted in red
    mapboxer::set_filter(layer_id = "apartment_building_searched", list("==", "bing_address", address))

}
