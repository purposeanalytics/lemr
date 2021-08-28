#' Zoom map of Toronto to address
#'
#' @param map Map created by \link{map_toronto}
#' @param address Address or sf tibble with geometry column
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_address_layer() %>%
#'   zoom_map_to_address("378 Markham St")
zoom_map_to_address <- function(map, address) {
  if (!inherits(address, "sf") & is.character(address)) {
    address <- geocode_address(glue::glue("{address} Toronto ON"), quiet = TRUE) %>%
      sf::st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)
  }

  map %>%
    # Update the data in the "address_points" layer to be for this address, so that that point shows
    mapboxer::set_data(data = address, source_id = "address_points") %>%
    # Set the visibility to visible
    toggle_layer_visible(id = "address_points") %>%
    # Zoom to the address
    mapboxer::fit_bounds(sf::st_bbox(address), maxZoom = 15, pitch = 0, bearing = -15)
}
