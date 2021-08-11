#' Add a blank neighbourhood layer
#'
#' Add a blank layer of \link{neighbourhoods} to a map (created via \link{map_toronto}). The purpose of this function is to allow for zooming and highlighting to a neighbourhood from the data, via \link{zoom_map_to_neighbourhood}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#' map_toronto() %>%
#'   add_blank_neighbourhood_layer() %>%
#'   zoom_map_to_neighbourhood("Casa Loma")
add_blank_neighbourhood_layer <- function(map) {
  map %>%
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), line_color = main_colour, line_width = 2, id = "neighbourhood_line") %>%
    # Add a "blank" layer for clicking on, that contains all neighbourhoods
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), fill_color = main_colour, fill_opacity = 0, id = "neighbourhood_click") %>%
    # Add an actual layer for neighbourhoods that will be thickened
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), line_color = main_colour, line_width = 10, id = "neighbourhood_click_line", filter = list("==", "neighbourhood", "none"))
}
