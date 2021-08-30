#' legend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_legend_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("legend_ui"), class = "padded", style = "margin-top: 0.5em;")
}

#' legend Server Functions
#'
#' @noRd
mod_legend_server <- function(id, level, neighbourhood) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Created in HTML because ggplot2 legends somehow can't be flushed to the left! Incredible.
    plot_legend <- shiny::reactive({
      if (level() == "neighbourhood") {
        create_legend(neighbourhood())
      }
    })

    output$legend <- shiny::renderText({
      plot_legend()
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$legend_ui <- shiny::renderUI({
      if (level() == "neighbourhood") {
        shiny::div(
          role = "img",
          `aria-label` = glue::glue("A legend showing {neighbourhood()} (dark blue) versus City of Toronto (grey), used for plots on this page."),
          shiny::htmlOutput(ns("legend"))
        )
      }
    })
  })
}

## To be copied in the UI
# mod_legend_ui("legend_ui_1")

## To be copied in the server
# mod_legend_server("legend_ui_1")
