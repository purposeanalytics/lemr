#' data_and_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_and_definitions_ui <- function(id) {
  ns <- NS(id)
  shiny::includeMarkdown(system.file("app", "data_and_definitions.md", package = "lemur"))

}

#' data_and_definitions Server Functions
#'
#' @noRd
mod_data_and_definitions_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_and_definitions_ui("data_and_definitions_ui_1")

## To be copied in the server
# mod_data_and_definitions_server("data_and_definitions_ui_1")
