#' Point Layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_point_layer_ui <- function(id) {
  ns <- NS(id)

  label <- point_layers_choices[[id]]

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::materialSwitch(
          inputId = ns("input"),
          label = label,
          value = id == "lem", # LEM is on by default
          status = "primary",
          right = TRUE
        )
      ),
      shiny::column(
        width = 6,
        shiny::conditionalPanel("input.input == true",
          # legend,
          ns = ns
        )
      )
    ),
    shiny::conditionalPanel(
      "input.input == true",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("plot_ui")),
          shiny::htmlOutput(ns("table"))
        )
      ),
      ns = ns
    )
  )
}

#' Point Layer Server Functions
#'
#' @noRd
mod_point_layer_server <- function(id, point_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update reactive with value, mod_map handles what's shown
    shiny::observeEvent(input$input,
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        layer <- if (input$input) {id} else {NULL}
        active_layers <- c(point_layers(), layer)

        # Update reactive with value from input - then mod_map handles what's shown
        point_layers(active_layers)
      }
    )
  })
}

point_layers_choices <- list(
  apartment_buildings = "Apartment Buildings", apartment_evaluation = "RentSafeTO Evaluation Scores", evictions_hearings = "Evictions Hearings", agi = "AGI Applications",
  tdf = "Tenant Defence Fund"
)

## To be copied in the UI
# mod_point_layer_ui("point_layer_ui_1")

## To be copied in the server
# mod_point_layer_server("point_layer_ui_1")
