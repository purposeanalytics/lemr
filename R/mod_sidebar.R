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
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("population")),
    shiny::uiOutput(ns("population_density")),
    shiny::plotOutput(ns("household_size"))
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, address, neighbourhood, search_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    header <- shiny::eventReactive(
      {
        address$address()
        neighbourhood()
      },
      {
        # TODO: seems like this runs the first time before search_method() has a value
        # Maybe initialize it with something? Or take an action if it's NULL based on which of address / neighbourhood is not null
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

    shiny::observeEvent(neighbourhood(), {

      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

      output$population <- shiny::renderUI({
        population <- neighbourhood_profile[["population"]] %>%
          dplyr::pull(value) %>%
          scales::comma()

        shiny::h2(glue::glue("Population: {population}"))
      })

      output$population_density <- shiny::renderUI({
        population_density <- neighbourhood_profile[["population_density"]] %>%
          dplyr::pull(value) %>%
          round() %>%
          scales::comma()

        shiny::h3(glue::glue("Population density: {population_density}"))
      })

      output$household_size <- shiny::renderPlot({
        neighbourhood_profile[["household_size"]] %>%
          plot_household_size()
      })
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
