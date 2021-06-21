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
  shiny::tagList(
    shiny::h1(shiny::uiOutput(ns("header"))),
    shiny::h2(shiny::uiOutput(ns("population"))),
    shiny::uiOutput(ns("tabs_people_places"))
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, address_neighbourhood) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(address_neighbourhood$neighbourhood, {
      neighbourhood <- address_neighbourhood$neighbourhood

      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood]]

      # Neighbourhood ----

      output$header <- shiny::renderUI(neighbourhood)

      # Population -----

      output$population <- shiny::renderUI({
        glue::glue('Population: {scales::comma(neighbourhood_profile[["population"]])} ({scales::comma(neighbourhood_profile[["households"]])} households)')
      })

      # Tabs -----

      output$tabs_people_places <- shiny::renderUI({
        shiny::tabsetPanel(
          id = "sidebar_tab",
          shiny::tabPanel(title = "People", mod_sidebar_people_ui(ns("people"))),
          shiny::tabPanel(title = "Places", mod_sidebar_places_ui(ns("places")))
        )
      })

      mod_sidebar_people_server("people", neighbourhood)
      mod_sidebar_places_server("places", neighbourhood)
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
