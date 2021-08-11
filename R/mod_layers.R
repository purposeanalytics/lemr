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
    shiny::hr(),
    bigger_padded("Display additional layers"),
    shiny::column(
      width = 6,
      # bigger_padded("Point data"),
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
      ),
      generate_layers_legend(c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "Low", "100%")
    ),
    shiny::column(
      width = 6,
      # bigger_padded("Aggregated data"),
      style = "padding-right: 0px;",
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("amenity_density"),
        choices = list("Amenity Density" = "amenity_density"),
        justified = TRUE
      ),
      generate_layers_legend(c(low_colour, mid_colour, high_colour), "Low", "High")
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

    shiny::observeEvent(
      {
        input$amenity_density
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$amenity_density)

        # Update reactive with value from input
        aggregate_layers(active_layers)
      }
    )
  })
}

point_layers_choices <- list("Apartment buildings" = "apartment_buildings", "RentSafeTO Evaluation Scores" = "apartment_evaluation")

aggregate_layers_choices <- list("Amenity density" = "amenity_density")

generate_layers_legend <- function(colors, min_text, max_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
    shiny::tags$ul(
      shiny::tags$li(class = "min", min_text),
      shiny::tags$li(class = "max", max_text),
      shiny::tags$li(
        class = "legend-colors",
        shiny::div(
          class = "colors",
          colors
        )
      )
    )
  )
}

## To be copied in the UI
# mod_layers_ui("layers_ui_1")

## To be copied in the server
# mod_layers_server("layers_ui_1")
