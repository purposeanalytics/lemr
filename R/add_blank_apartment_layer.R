add_blank_apartment_layer <- function(map) {
  map %>%
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry %>% dplyr::select(bing_address, geometry, confirmed_units)), circle_color = "red", id = "apartment_building_searched", filter = list("==", "bing_address", "none")) %>%
    mapboxer::add_tooltips(layer_id = "apartment_building", "{{bing_address}}<br>Units: {{confirmed_units}}")
}
