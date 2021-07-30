#' layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_layers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h1("Data"),
    shiny::column(
      width = 6,
      shiny::checkboxGroupInput(
        inputId = ns("point_layers"),
        label = "Data points",
        choices = point_layers_choices
      ),
    ),
    shiny::column(
      width = 6,
      shiny::checkboxGroupInput(
        inputId = ns("aggregate_layers"),
        label = "Aggregated data",
        choices = aggregate_layers_choices
      )
    )
  )
}
#' layers Server Functions
#'
#' @noRd
mod_layers_server <- function(id, point_layers, aggregate_layers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$point_layers,
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        # Update reactive with value from input
        point_layers(input$point_layers)
      }
    )

    shiny::observeEvent(input$aggregate_layers,
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        # Update reactive with value from input
        aggregate_layers(input$aggregate_layers)
      }
    )
  })
}

point_layers_choices <- list("Apartment buildings" = "apartment_buildings", "RentSafeTO Evaluation Scores" = "apartment_evaluation")

aggregate_layers_choices <- list("Amenity density" = "amenity_density")

## To be copied in the UI
# mod_layers_ui("layers_ui_1")

## To be copied in the server
# mod_layers_server("layers_ui_1")
