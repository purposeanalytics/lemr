toggle_layer_visible <- function(map, id) {
  map %>%
    # Set the visibility to visible
    mapboxer::set_layout_property(layer_id = id, "visibility", "visible")
}

toggle_layer_invisible <- function(map, id) {
  map %>%
    # Set the visibility to "none"
    mapboxer::set_layout_property(layer_id = id, "visibility", "none")
}
