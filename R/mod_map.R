#' Map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    mapboxer::mapboxerOutput(ns("map"))
  )
}

#' Map Server Functions
#'
#' @noRd
mod_map_server <- function(id, address){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initial map
    output$map <- mapboxer::renderMapboxer({
      toronto %>%
        mapboxer::as_mapbox_source() %>%
        mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, minZoom = 10, pitch = 0, bearing = -15) %>%
        mapboxer::add_navigation_control() %>%
        mapboxer::add_line_layer(line_color = "green", line_width = 2) %>%
        mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(apartment_building_registry), circle_color = NA, id = "apartment_building") %>%
        mapboxer::add_tooltips(layer_id = "apartment_building", "{{bing_address}}<br>Units: {{confirmed_units}}")
    })

    # Update zoom of map and highlighted apartment based on address search
    shiny::observeEvent(address(), {
      selected_address <- apartment_building_registry %>%
        dplyr::filter(.data$bing_address == address())

      mapboxer::mapboxer_proxy(ns("map")) %>%
        # Zoom to the address
        mapboxer::fit_bounds(sf::st_bbox(selected_address), maxZoom = 15, pitch = 0, bearing = -15) %>%
        # Add a layer to it with that address highlighted
        # mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(address_lat_long), circle_color = "red", id = "searched_address") %>%
        # mapboxer::set_data(data = selected_address, source_id = "apartment_building") %>%
        mapboxer::update_mapboxer()
    })

  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
