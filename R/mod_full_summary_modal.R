#' full_summary_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_full_summary_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' full_summary_modal Server Functions
#'
#' @noRd 
mod_full_summary_modal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_full_summary_modal_ui("full_summary_modal_ui_1")
    
## To be copied in the server
# mod_full_summary_modal_server("full_summary_modal_ui_1")
