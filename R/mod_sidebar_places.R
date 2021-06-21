#' sidebar_places UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_places_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sidebar_places Server Functions
#'
#' @noRd 
mod_sidebar_places_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sidebar_places_ui("sidebar_places_ui_1")
    
## To be copied in the server
# mod_sidebar_places_server("sidebar_places_ui_1")
