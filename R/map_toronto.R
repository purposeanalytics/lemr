#' Create a map of Toronto
#'
#' @return A mapboxer map of Toronto
#' @export
#'
#' @examples
#' map_toronto()
map_toronto <- function() {
  lemur::toronto %>%
    mapboxer::as_mapbox_source() %>%
    mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, pitch = 0, bearing = -15) %>%
    mapboxer::fit_bounds(sf::st_bbox(toronto), pitch = 0, bearing = -15) %>%
    mapboxer::add_navigation_control() %>%
    mapboxer::add_line_layer(line_color = main_colour, line_width = 2)
}
