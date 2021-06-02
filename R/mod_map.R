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
      map_toronto()
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

    # Update zoom of map and highlight neighbourhood based on neighbourhood search
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
