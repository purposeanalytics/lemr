#' Zoom map of Toronto to neighbourhood
#'
#' @param map Map created by \link{map_toronto}
#' @param neighbourhood Neighbourhood from \link{neighbourhoods}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>% add_blank_neighbourhood_layer() %>% zoom_map_to_neighbourhood("Casa Loma")
zoom_map_to_neighbourhood <- function(map, neighbourhood) {
  searched_neighbourhood <- lemur::neighbourhoods %>%
    dplyr::filter(.data$neighbourhood == !!neighbourhood)

  if (nrow(searched_neighbourhood) == 0) {
    stop("Neighbourhood not found in `neighbourhoods`", call. = FALSE)
  }

  map %>%
    # Zoom to the address
    mapboxer::fit_bounds(sf::st_bbox(searched_neighbourhood), maxZoom = 15, pitch = 0, bearing = -15) %>%
    # Filter the "neighbourhood_line" and "neighbourhood_fill" layers to be for this neighbourhood
    mapboxer::set_filter(layer_id = "neighbourhood_line", list("==", "neighbourhood", neighbourhood)) %>%
    mapboxer::set_filter(layer_id = "neighbourhood_fill", list("==", "neighbourhood", neighbourhood))
}
