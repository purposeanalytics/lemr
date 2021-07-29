#' layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_layers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h1("Data"),
    shinyWidgets::prettyCheckbox(
      inputId = ns("apartment_buildings"), label = "Apartment buildings", icon = shiny::icon("check")
    ),
    shinyWidgets::prettyCheckbox(
      inputId = ns("amenity_density"), label = "Amenity density", icon = shiny::icon("check")
    )
  )
}

#' layers Server Functions
#'
#' @noRd
mod_layers_server <- function(id, layer_apartment_building, layer_amenity_density) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$apartment_buildings, ignoreInit = TRUE, {
      # Update reactive with value from input
      layer_apartment_building(input$apartment_buildings)
    })

    observeEvent(input$amenity_density, ignoreInit = TRUE, {
      # Update reactive with value from input
      layer_amenity_density(input$amenity_density)
    })
  })
}

## To be copied in the UI
# mod_layers_ui("layers_ui_1")

## To be copied in the server
# mod_layers_server("layers_ui_1")
