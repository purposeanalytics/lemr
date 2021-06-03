add_blank_neighbourhood_layer <- function(map) {
  map %>%
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), line_color = "green", line_width = 2, id = "neighbourhood_line", filter = list("==", "neighbourhood", "none")) %>%
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(lemur::neighbourhoods), fill_color = "green", fill_opacity = 0.25, id = "neighbourhood_fill", filter = list("==", "neighbourhood", "none")) %>%
    mapboxer::add_tooltips(layer_id = "neighbourhood_fill", "{{neighbourhood}}")
}
