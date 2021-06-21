#' sidebar_places UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_places_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("people_sidebar"))
}

#' sidebar_places Server Functions
#'
#' @noRd
mod_sidebar_places_server <- function(id, neighbourhood) {
  moduleServer(id, function(input, output, session) {
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
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::h3("Households by tenure"),
              shiny::plotOutput(ns("household_tenure"), height = "100px")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::h3("Average Shelter Cost for Renters")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              align = "center",
              shiny::h4(shiny::uiOutput(ns("shelter_cost"))),
              shiny::h4(shiny::uiOutput(ns("shelter_cost_city")))
            ),
            shiny::column(
              width = 9,
              shiny::plotOutput(ns("average_renter_shelter_cost_plot"), height = "100px")
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

      # Household tenure ----

      output$household_tenure <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("renter_owner")
        },
        res = 96,
        bg = "transparent"
      )

      # Shelter cost ----

      output$shelter_cost <- shiny::renderUI({
        glue::glue('{scales::dollar(neighbourhood_profile[["average_renter_shelter_cost"]], accuracy = 1)}')
      })

      output$shelter_cost_city <- shiny::renderUI({
        glue::glue('(City: {scales::dollar(city_profile[["average_renter_shelter_cost"]][["value"]], accuracy = 1)})')
      })

      output$average_renter_shelter_cost_plot <- shiny::renderPlot({
        neighbourhood_profile %>%
          plot_neighbourhood_profile_distribution("average_renter_shelter_cost") +
          ggplot2::scale_x_continuous(labels = scales::dollar)
      })
    })
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
