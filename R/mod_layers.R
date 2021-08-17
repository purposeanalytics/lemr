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
    biggest_padded("Display additional layers"),
    bigger_padded("Aggregate data"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("lem"),
          choices = list("Low-end of market rentals" = "lem"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6,
        generate_layers_legend(c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A", "#053C6B"), "0", "100")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("amenity_density"),
          choices = list("Amenity Density" = "amenity_density"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6,
        generate_low_mid_high_legends(rev(c(low_colour, mid_colour, high_colour)), "High", "Medium", "Low")
      )
    ),
    bigger_padded("Points data"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("apartment_buildings"),
          choices = list("Apartment buildings" = "apartment_buildings"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("apartment_evaluation"),
          choices = list("RentSafeTO Evaluation Scores" = "apartment_evaluation"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6,
        generate_layers_legend(c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "Low", "100%")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("agi"),
          choices = list("AGI Applications" = "agi"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6
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
        input$agi
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$apartment_buildings, input$apartment_evaluation, input$agi)

        # Update reactive with value from input
        point_layers(active_layers)
      }
    )

    shiny::observeEvent(
      {
        input$amenity_density
        input$lem
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$amenity_density, input$lem)

        # Update reactive with value from input
        aggregate_layers(active_layers)
      }
    )
  })
}

point_layers_choices <- list("Apartment buildings" = "apartment_buildings", "RentSafeTO Evaluation Scores" = "apartment_evaluation", "AGI Applications" = "agi")

aggregate_layers_choices <- list("Amenity density" = "amenity_density", "Low-end of market rentals" = "lem")

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

generate_low_mid_high_legends <- function(colors, min_text, mid_text, max_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
    shiny::div(
      class = "triple-text",
      shiny::tags$ul(
        shiny::tags$li(min_text),
        shiny::tags$li(mid_text),
        shiny::tags$li(max_text)
      )
    ),
    shiny::tags$li(
      class = "legend-colors",
      shiny::div(
        class = "colors",
        colors
      )
    )
  )
}

## To be copied in the UI
# mod_layers_ui("layers_ui_1")

## To be copied in the server
# mod_layers_server("layers_ui_1")
