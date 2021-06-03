#' Zoom map of Toronto to neighbourhood
#'
#' @param map Map created by \link{map_toronto}
#' @param neighbourhood Neighbourhood from \link{neighbourhoods}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>% zoom_map_to_neighbourhood("Casa Loma")
zoom_map_to_neighbourhood <- function(map, neighbourhood) {
  searched_neighbourhood <- lemur::neighbourhoods %>%
    dplyr::filter(.data$neighbourhood == !!neighbourhood)

  if (nrow(searched_neighbourhood) == 0) {
    stop("Neighbourhood not found in `neighbourhoods`", call. = FALSE)
  }

  map %>%
    # Zoom to the address
    mapboxer::fit_bounds(sf::st_bbox(searched_neighbourhood), maxZoom = 15, pitch = 0, bearing = -15) %>%
    # Highlight the address
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(searched_neighbourhood), line_color = "green", line_width = 2, id = "neighbourhood_line") %>%
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(searched_neighbourhood), fill_color = "green", fill_opacity = 0.25, id = "neighbourhood_fill")
}
