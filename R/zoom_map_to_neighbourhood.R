#' Zoom map of Toronto to neighbourhood
#'
#' @param map Map created by \link{map_toronto}
#' @param neighbourhood Neighbourhood from \link{neighbourhoods}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_neighbourhood_layer() %>%
#'   zoom_map_to_neighbourhood("Casa Loma")
zoom_map_to_neighbourhood <- function(map, neighbourhood) {
  searched_neighbourhood <- lemur::neighbourhoods %>%
    dplyr::filter(.data$neighbourhood == !!neighbourhood)

  map %>%
    # Filter the "neighbourhood_click_line" layers to be for this neighbourhood
    mapboxer::set_filter(layer_id = "neighbourhood_click_line", list("==", "neighbourhood", neighbourhood)) %>%
    # Zoom to the neighbourhood
    mapboxer::fit_bounds(sf::st_bbox(searched_neighbourhood), maxZoom = 14, pitch = 0, bearing = -15)
}
