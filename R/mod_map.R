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
mod_map_server <- function(id, address, neighbourhood) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial map
    output$map <- mapboxer::renderMapboxer({
      map_toronto() %>%
        add_blank_apartment_layer()
    })

    # Update zoom of map and highlighted apartment based on address search
    shiny::observeEvent(address(), {
      mapboxer::mapboxer_proxy(ns("map")) %>%
        zoom_map_to_address(address()) %>%
        mapboxer::update_mapboxer()
    })

    # Update zoom of map and highlight neighbourhood based on neighbourhood search
    shiny::observeEvent(neighbourhood(), {
      mapboxer::mapboxer_proxy(ns("map")) %>%
        zoom_map_to_neighbourhood(neighbourhood()) %>%
        mapboxer::update_mapboxer()
    })
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
