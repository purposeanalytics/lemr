#' full_summary_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_full_summary_modal_ui <- function(id) {
  ns <- NS(id)

  shiny::actionButton(ns("modal"), label = "Full Summary")
}

#' full_summary_modal Server Functions
#'
#' @noRd
mod_full_summary_modal_server <- function(id, header) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$modal, {
      shiny::showModal(shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h1(shiny::textOutput(ns("header"))),
            shiny::hr()
          ),
          shiny::column(width = 4),
          shiny::column(width = 4),
          shiny::column(width = 4)
        ),
        easyClose = TRUE,
        footer = NULL, size = "l"
      ))

      output$header <- shiny::renderText(header())
    })
  })
}

## To be copied in the UI
# mod_full_summary_modal_ui("full_summary")

## To be copied in the server
# mod_full_summary_modal_server("full_summary")
