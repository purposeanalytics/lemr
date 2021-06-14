#' Address Search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_address_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(
      inputId = ns("address"),
      label = "Address",
      choices = lemur::apartment_building_registry[["bing_address"]], multiple = TRUE, selected = NULL, options = shinyWidgets::pickerOptions(liveSearch = TRUE, maxOptions = 1, size = 10)
    )
  )
}

#' Address Search Server Functions
#'
#' @noRd
mod_address_search_server <- function(id, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update that the search method was via address
    shiny::observeEvent(input$address, {
      search_method("address")
    })

    # Get neighbourhood of address
    neighbourhood <- shiny::reactive({
      lemur::apartment_building_registry %>%
        dplyr::filter(.data$bing_address == input$address) %>%
        dplyr::pull(.data$neighbourhood)
    })

    # For now, just return the address and its neighbourhood to be used by other modules
    list(
      address = shiny::reactive(input$address),
      neighbourhood = shiny::reactive(neighbourhood())
    )
  })
}

## To be copied in the UI
# mod_address_search_ui("address")

## To be copied in the server
# mod_address_search_server("address")
