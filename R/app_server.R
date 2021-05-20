#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$map <- mapboxer::renderMapboxer(
    toronto %>%
      mapboxer::as_mapbox_source() %>%
      mapboxer::mapboxer(style = mapboxer::basemap_raster_style(), center = c(-79.39021, 43.72557), zoom = 11, minZoom = 10, pitch = 0, bearing = -15) %>%
      mapboxer::add_line_layer(line_color = "red", line_width = 2)
  )

  shiny::observeEvent(input$address, {
    address_options <-
      apartment_building_registry %>%
      dplyr::filter(stringr::str_starts(stringr::str_to_lower(address), stringr::str_to_lower(input$address))) %>%
      dplyr::pull(address)

    # shinyWidgets::updatePickerInput(session, inputId = "address", choices = address_options)
  })
}
