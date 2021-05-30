#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Address
  shiny::updateSelectizeInput(session, "address", choices = apartment_building_registry[["bing_address"]], server = TRUE, options = list(allowEmptyOption = FALSE, placeholder = "SEARCH..."))

  library(sf)

  output$map <- mapboxer::renderMapboxer({
    toronto %>%
      mapboxer::as_mapbox_source() %>%
      mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, minZoom = 10, pitch = 0, bearing = -15) %>%
      mapboxer::add_navigation_control() %>%
      mapboxer::add_line_layer(line_color = "red", line_width = 2) %>%
      mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(apartment_building_registry), circle_color = NA, id = "apartment_building") %>%
      mapboxer::add_tooltips(layer_id = "apartment_building", "{{bing_address}}<br>Units: {{confirmed_units}}")
  })

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
