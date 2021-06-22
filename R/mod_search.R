#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_search_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::column(
      width = 6,
      shinyWidgets::pickerInput(
        inputId = ns("address"),
        label = "Address",
        choices = lemur::apartment_building_registry[["bing_address"]],
        multiple = TRUE,
        selected = NULL,
        options = shinyWidgets::pickerOptions(liveSearch = TRUE, maxOptions = 1, size = 10)
      )
    ),
    shiny::column(
      width = 6,
      shiny::selectizeInput(
        inputId = ns("neighbourhood"),
        label = "Neighbourhood",
        choices = sort(lemur::neighbourhoods[["neighbourhood"]]),
        multiple = TRUE,
        selected = NULL,
        options = list(
          placeholder = "Search neighbourhood...",
          maxItems = 1
        )
      )
    )
  )
}

#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id, address_and_neighbourhood, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # If address is selected, store address and neighbourhood
    shiny::observeEvent(input$address, {
      address_and_neighbourhood$address <- input$address

      address_and_neighbourhood$neighbourhood <- lemur::apartment_building_registry %>%
        dplyr::filter(.data$bing_address == input$address) %>%
        dplyr::pull(.data$neighbourhood)

      # Update search method
      search_method("address")

      # Deselect neighbourhood
      shiny::updateSelectizeInput(session = session, inputId = "neighbourhood", selected = "")
    })

    # If neighbourhood is selected, store neighbourhood
    shiny::observeEvent(input$neighbourhood, {
      address_and_neighbourhood$neighbourhood <- input$neighbourhood

      # Update search method
      search_method("neighbourhood")

      # Deselect address
      shinyWidgets::updatePickerInput(session = session, inputId = "address", selected = "")
    })
  })
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# mod_search_server("search_ui_1")
