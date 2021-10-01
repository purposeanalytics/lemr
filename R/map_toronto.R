#' Create a map of Toronto
#'
#' @return A mapboxer map of Toronto
#' @export
#'
#' @examples
#' map_toronto()
map_toronto <- function() {
  lemr::toronto %>%
    mapboxer::as_mapbox_source() %>%
    mapboxer::mapboxer(style = "mapbox://styles/purposeanalytics/cksw80iwn155y1anq5l1f0v5n", center = c(-79.3753, 43.7173), zoom = 12, pitch = 0, bearing = bearing, minZoom = 8) %>%
    mapboxer::fit_bounds(sf::st_bbox(lemr::toronto), pitch = 0, bearing = bearing) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    mapboxer::add_line_layer(line_color = default_line_colour, line_width = 1.5)
}
