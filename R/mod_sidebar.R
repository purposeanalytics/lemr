#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("header"))
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, address, neighbourhood, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    header <- shiny::eventReactive(
      search_method(),
      {
        if (search_method() == "neighbourhood") {
          shiny::h1(neighbourhood())
        } else if (search_method() == "address") {
          shiny::tagList(
            shiny::h1(address$address()),
            shiny::h1(address$neighbourhood())
          )
        }
      }
    )

    output$header <- shiny::renderUI(header())
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
