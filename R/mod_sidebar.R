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
mod_sidebar_server <- function(id, address, neighbourhood, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    display_neighbourhood <- shiny::eventReactive(
      {
        # address$address()
        neighbourhood()
      },
      {
        # TODO: seems like this runs the first time before search_method() has a value
        # Maybe initialize it with something? Or take an action if it's NULL based on which of address / neighbourhood is not null
        # search_method_neighbourhood <- search_method() == "neighbourhood" | (!is.null(neighbourhood()) & is.null(address$neighbourhood()))
        search_method_neighbourhood <- !is.null(neighbourhood())
        if (search_method_neighbourhood) {
          neighbourhood()
        } else {
          address$neighbourhood()
        }
      }
    )

    shiny::observeEvent(neighbourhood(), {
      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

      # Neighbourhood ----

      output$header <- shiny::renderUI(display_neighbourhood())

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
