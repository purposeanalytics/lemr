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
  shiny::uiOutput(ns("people_sidebar"))
}

#' sidebar_places Server Functions
#'
#' @noRd
mod_sidebar_places_server <- function(id, neighbourhood){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(neighbourhood(), {

      # UI ----

      # Do the whole thing as one UI rendered in so it all loads at once - better than having headers / plots / etc appear at different times!
      # Then all of the layout can be updated in one place too :)

      output$people_sidebar <- shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Structure Type"),
              shiny::plotOutput(ns("structure_type"), height = "200px")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Number of Bedrooms"),
              shiny::plotOutput(ns("bedrooms"), height = "200px")
            )
          )
        )
      })

      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

      # Structure type -----

      output$structure_type <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("structure_type")
        },
        res = 96,
        bg = "transparent"
      )

      # Bedrooms -----

      output$bedrooms <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("bedrooms")
        },
        res = 96,
        bg = "transparent"
      )
    })

  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
