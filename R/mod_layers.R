#' layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_layers_ui <- function(id) {
  ns <- NS(id)
  shiny::column(
    width = 12,
    shiny::h1("Data"),
    shiny::column(
      width = 6,
      bigger_padded("Point data"),
      style = "padding-left: 0px;",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("apartment_buildings"),
        choices = list("Apartment buildings" = "apartment_buildings"),
        justified = TRUE
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("apartment_evaluation"),
        choices = list("RentSafeTO Evaluation Scores" = "apartment_evaluation"),
        justified = TRUE
      )
    ),
    shiny::column(
      width = 6,
      bigger_padded("Aggregated data"),
      style = "padding-right: 0px;",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("amenity_density"),
        choices = "Amenity Density",
        justified = TRUE
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

    shiny::observeEvent(
      {
        input$apartment_buildings
        input$apartment_evaluation
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$apartment_buildings, input$apartment_evaluation)

        # Update reactive with value from input
        point_layers(active_layers)
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
