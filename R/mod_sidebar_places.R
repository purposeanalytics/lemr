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

    level <- shiny::reactive({
      if (is.null(neighbourhood())) {
        "city"
      } else {
        "neighbourhood"
      }
    })

    compare <- shiny::reactive({
      level() == "neighbourhood"
    })

    dataset <- shiny::reactive({
      determine_dataset_from_level(level(), neighbourhood())
    })

    # UI ----

    # Do the whole thing as one UI rendered in so it all loads at once - better than having headers / plots / etc appear at different times!
    # Then all of the layout can be updated in one place too :)

    output$people_sidebar <- shiny::renderUI({
      shiny::tagList(
        shiny::div(
          shiny::hr(),
          shiny::htmlOutput(ns("legend")),
          shiny::h2("Housing structure type"),
          shiny::textOutput(ns("structure_type_description")),
          shiny::plotOutput(ns("structure_type_plot"), height = "200px"),
          shiny::htmlOutput(ns("structure_type_table")),
          shiny::h2("Number of bedrooms"),
          shiny::textOutput(ns("bedrooms_description")),
          shiny::plotOutput(ns("bedrooms_plot"), height = "200px"),
          shiny::htmlOutput(ns("bedrooms_table")),
          shiny::h2("Households by tenure"),
          shiny::textOutput(ns("household_tenure_description")),
          shiny::plotOutput(ns("household_tenure_plot"), height = "120px"),
          shiny::htmlOutput(ns("household_tenure_table")),
          shiny::h2("Average shelter cost for renters"),
          bigger_padded(shiny::textOutput(ns("shelter_cost"))),
          bigger_padded(shiny::textOutput(ns("shelter_cost_city"))),
          shiny::textOutput(ns("average_renter_shelter_cost_description")),
          shiny::plotOutput(ns("average_renter_shelter_cost_plot"), height = "100px")
        )
      )
    })

    # Legend ----

    # Created in HTML because ggplot2 legends somehow can't be flushed to the left! Incredible.
    plot_legend <- shiny::reactive({
      if (level() == "neighbourhood") {
        create_legend(neighbourhood())
      }
    })

    output$legend <- shiny::renderText({
      plot_legend()
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    # Structure type -----

    output$structure_type_description <- shiny::renderText({
      structure_type_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    structure_type_alt_text <- shiny::reactive({
      structure_type_plot_alt_text(level(), neighbourhood())
    })

    output$structure_type_plot <- shiny::renderPlot(
      {
        structure_type_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = structure_type_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    output$structure_type_table <- shiny::renderText({
      generate_table(dataset(), "structure_type", compare(), "Housing Structure Type", "Percent")
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    # Bedrooms -----

    output$bedrooms_description <- shiny::renderText({
      bedrooms_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    bedrooms_alt_text <- shiny::reactive({
      bedrooms_plot_alt_text(level(), neighbourhood())
    })

    output$bedrooms_plot <- shiny::renderPlot(
      {
        bedrooms_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = bedrooms_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    output$bedrooms_table <- shiny::renderText({
      generate_table(dataset(), "bedrooms", compare(), "Number of bedrooms", "Percent")
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    # Household tenure ----

    output$household_tenure_description <- shiny::renderText({
      household_tenure_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    household_tenure_alt_text <- shiny::reactive({
      household_tenure_plot_alt_text(level(), neighbourhood())
    })

    output$household_tenure_plot <- shiny::renderPlot(
      {
        household_tenure_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = household_tenure_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    output$household_tenure_table <- shiny::renderText({
      generate_table(dataset(), "household_tenure", compare(), "Household tenure", "Percent")
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    # Shelter cost ----

    shelter_cost <- shiny::reactive({
      get_measure(dataset(), "average_renter_shelter_cost")
    })

    shelter_cost_formatted <- shiny::reactive({
      format_measure(shelter_cost(), "average_renter_shelter_cost")
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$shelter_cost <- shiny::renderText({
      shelter_cost_number(shelter_cost_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$shelter_cost_city <- shiny::renderText({
      shelter_cost_city(level())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$average_renter_shelter_cost_description <- shiny::renderText({
      average_renter_shelter_cost_description(level(), neighbourhood(), shelter_cost(), shelter_cost_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    average_renter_shelter_cost_alt_text <- shiny::reactive({
      average_renter_shelter_cost_plot_alt_text(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$average_renter_shelter_cost_plot <- shiny::renderPlot(
      {
        average_renter_shelter_cost_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = average_renter_shelter_cost_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
