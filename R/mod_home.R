#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "content-page",
      shiny::includeMarkdown(app_sys("app", "home.md"))
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_home_ui("home")

## To be copied in the server
# mod_home_server("home")
