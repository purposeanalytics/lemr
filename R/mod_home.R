#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(
      style = "max-width: 1000px; margin-left: auto; margin-right: auto; font-size: var(--bigger-size); margin-top: 1em;",
      shiny::includeMarkdown(app_sys("app", "home.md"))
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home")

## To be copied in the server
# mod_home_server("home")
