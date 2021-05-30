#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  library(sf)

  mod_map_server("map")

  # Update zoom based on address
  shiny::observeEvent(input$address, {
    selected_address <- apartment_building_registry %>%
      dplyr::filter(.data$bing_address == input$address)

    mapboxer::mapboxer_proxy("map") %>%
      # Zoom to the address
      mapboxer::fit_bounds(sf::st_bbox(selected_address), maxZoom = 15, pitch = 0, bearing = -15) %>%
      # Add a layer to it with that address highlighted
      # mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(address_lat_long), circle_color = "red", id = "searched_address") %>%
      # mapboxer::set_data(data = selected_address, source_id = "apartment_building") %>%
      mapboxer::update_mapboxer()
  })
}
