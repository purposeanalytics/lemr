#' Neighbourhood Search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_neighbourhood_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
}

#' Neighbourhood Search Server Functions
#'
#' @noRd
mod_neighbourhood_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # For now, just return the neighbourhood to be used by other modules
    shiny::reactive(input$neighbourhood)
  })
}

## To be copied in the UI
# mod_neighbourhood_search_ui("neighbourhood")

## To be copied in the server
# mod_neighbourhood_search_server("neighbourhood")
