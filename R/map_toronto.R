#' Create a map of Toronto
#'
#' @return A mapboxer map of Toronto
#'
#' @examples
#' map_toronto()
map_toronto <- function() {
  lemur::toronto %>%
    mapboxer::as_mapbox_source() %>%
    mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, minZoom = 10, pitch = 0, bearing = -15) %>%
    mapboxer::add_navigation_control() %>%
    mapboxer::add_line_layer(line_color = "green", line_width = 2) %>%
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry), circle_color = "grey", id = "apartment_building") %>%
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry), circle_color = "red", id = "apartment_building_searched", filter = list("==", "bing_address", "none")) %>%
    mapboxer::add_tooltips(layer_id = "apartment_building", "{{bing_address}}<br>Units: {{confirmed_units}}")
}
