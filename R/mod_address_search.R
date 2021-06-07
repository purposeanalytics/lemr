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
mod_address_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # For now, just return the address to be used by other modules
    shiny::reactive(input$address)
  })
}

## To be copied in the UI
# mod_address_search_ui("address")

## To be copied in the server
# mod_address_search_server("address")
