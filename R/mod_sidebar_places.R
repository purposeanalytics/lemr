#' sidebar_places UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_places_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("people_sidebar"))
}

#' sidebar_places Server Functions
#'
#' @noRd
mod_sidebar_places_server <- function(id, neighbourhood) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sidebar_level <- shiny::reactive({
      if (is.null(neighbourhood())) {
        "city"
      } else {
        "neighbourhood"
      }
    })

    compare <- shiny::reactive({
      sidebar_level() == "neighbourhood"
    })

    dataset <- shiny::reactive({
      switch(sidebar_level(),
        "city" = lemur::city_profile,
        "neighbourhood" = lemur::neighbourhood_profiles[[neighbourhood()]]
      )
    })

    # UI ----

    # Do the whole thing as one UI rendered in so it all loads at once - better than having headers / plots / etc appear at different times!
    # Then all of the layout can be updated in one place too :)

    output$people_sidebar <- shiny::renderUI({
      shiny::tagList(
        shiny::column(
          width = 12,
          shiny::h3("Structure Type"),
          shiny::htmlOutput(ns("structure_type_legend")),
          shiny::plotOutput(ns("structure_type"), height = "200px")
        ),
        shiny::column(
          width = 12,
          shiny::h3("Number of Bedrooms"),
          shiny::htmlOutput(ns("bedrooms_legend")),
          shiny::plotOutput(ns("bedrooms"), height = "200px")
        ),
        shiny::column(
          width = 12,
          shiny::h3("Households by tenure"),
          shiny::plotOutput(ns("household_tenure"), height = "120px")
        ),
        shiny::column(
          width = 12,
          shiny::h3("Average Shelter Cost for Renters"),
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

    # Legend ----

    # Created in HTML because ggplot2 legends somehow can't be flushed to the left! Incredible.
    plot_legend <- shiny::reactive({
      if (sidebar_level() == "neighbourhood") {
        create_legend(neighbourhood())
      }
    })

    # Structure type -----

    output$structure_type_legend <- shiny::renderText({
      plot_legend()
    })

    output$structure_type <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile("structure_type", compare = compare())
      },
      res = 96,
      bg = "transparent"
    )

    # Bedrooms -----

    output$bedrooms_legend <- shiny::renderText({
      plot_legend()
    })

    output$bedrooms <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile("bedrooms", compare = compare())
      },
      res = 96,
      bg = "transparent"
    )

    # Household tenure ----

    output$household_tenure <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile("household_tenure", compare = compare())
      },
      res = 96,
      bg = "transparent"
    )

    # Shelter cost ----

    output$shelter_cost <- shiny::renderUI({
      glue::glue('{scales::dollar(dataset()[["average_renter_shelter_cost"]], accuracy = 1)}')
    })

    output$shelter_cost_city <- shiny::renderUI({
      if (sidebar_level() == "neighbourhood") {
        glue::glue('(City: {scales::dollar(city_profile[["average_renter_shelter_cost"]], accuracy = 1)})')
      } else {
        NULL
      }
    })

    output$average_renter_shelter_cost_plot <- shiny::renderPlot({
      dataset() %>%
        plot_neighbourhood_profile_distribution("average_renter_shelter_cost", compare = compare(), binwidth = 50) +
        ggplot2::scale_x_continuous(labels = scales::dollar)
    })
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
