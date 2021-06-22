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
mod_map_server <- function(id, address_and_neighbourhood, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial map
    output$map <- mapboxer::renderMapboxer({
      map_toronto() %>%
        add_blank_apartment_layer() %>%
        add_blank_neighbourhood_layer()
    })

    # Update zoom of map and highlighted apartment and/or neighbourhood based on search
    shiny::observeEvent(
      {
        search_method()
        address_and_neighbourhood$neighbourhood
      }, ignoreNULL = FALSE, ignoreInit = TRUE,
      {

        if (search_method() == "address") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            zoom_map_to_address(address_and_neighbourhood$address) %>%
            zoom_map_to_neighbourhood(address_and_neighbourhood$neighbourhood) %>%
            mapboxer::update_mapboxer()
        } else if (search_method() == "neighbourhood") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            # Clear address
            zoom_map_to_address("none") %>%
            zoom_map_to_neighbourhood(address_and_neighbourhood$neighbourhood) %>%
            mapboxer::update_mapboxer()
        } else if (search_method() == "back") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            # Clear address
            zoom_map_to_address("none") %>%
            # Clear neighbourhood
            zoom_map_to_neighbourhood("none") %>%
            mapboxer::update_mapboxer()
        }
      }
    )
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
