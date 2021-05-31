#' Map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mapboxer::mapboxerOutput(ns("map"))
  )
}

#' Map Server Functions
#'
#' @noRd
mod_map_server <- function(id, address) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial map
    output$map <- mapboxer::renderMapboxer({
      lemur::toronto %>%
        mapboxer::as_mapbox_source() %>%
        mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, minZoom = 10, pitch = 0, bearing = -15) %>%
        mapboxer::add_navigation_control() %>%
        mapboxer::add_line_layer(line_color = "green", line_width = 2) %>%
        mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry), circle_color = "grey", id = "apartment_building") %>%
        mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(lemur::apartment_building_registry), circle_color = "red", id = "apartment_building_searched", filter = list("==", "bing_address", "none")) %>%
        mapboxer::add_tooltips(layer_id = "apartment_building", "{{bing_address}}<br>Units: {{confirmed_units}}")
    })

    # Update zoom of map and highlighted apartment based on address search
    shiny::observeEvent(address(), {
      searched_address <- lemur::apartment_building_registry %>%
        dplyr::filter(.data$bing_address == address())

      mapboxer::mapboxer_proxy(ns("map")) %>%
        # Zoom to the address
        mapboxer::fit_bounds(sf::st_bbox(searched_address), maxZoom = 15, pitch = 0, bearing = -15) %>%
        # Filter the "apartment_building_searched" layer to be for this address, so that that point is highlighted in red
        mapboxer::set_filter(layer_id = "apartment_building_searched", list("==", "bing_address", address())) %>%
        mapboxer::update_mapboxer()
    })
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
