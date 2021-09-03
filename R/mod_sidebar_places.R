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
          mod_legend_ui(ns("legend")),
          shiny::h2("Housing structure type"),
          shiny::textOutput(ns("structure_type_description")),
          shiny::uiOutput(ns("structure_type_plot_ui")),
          shiny::htmlOutput(ns("structure_type_table")),
          shiny::h2("Number of bedrooms"),
          shiny::textOutput(ns("bedrooms_description")),
          shiny::uiOutput(ns("bedrooms_plot_ui")),
          shiny::htmlOutput(ns("bedrooms_table")),
          shiny::h2("Households by tenure"),
          shiny::textOutput(ns("household_tenure_description")),
          shiny::uiOutput(ns("household_tenure_plot_ui")),
          shiny::htmlOutput(ns("household_tenure_table")),
          shiny::h2("Average shelter cost for renters"),
          bigger_padded(shiny::textOutput(ns("shelter_cost"))),
          bigger_padded(shiny::textOutput(ns("shelter_cost_city"))),
          shiny::textOutput(ns("average_renter_shelter_cost_description")),
          shiny::uiOutput(ns("average_renter_shelter_cost_plot_ui"))
        )
      )
    })

    # Legend ----

    mod_legend_server("legend", level, neighbourhood)

    # Structure type -----

    output$structure_type_description <- shiny::renderText({
      structure_type_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    structure_type_alt_text <- shiny::reactive({
      structure_type_plot_alt_text(level(), neighbourhood())
    })

    output$structure_type_plot <- plotly::renderPlotly({
      structure_type_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$structure_type_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = structure_type_alt_text(),
        plotly::plotlyOutput(ns("structure_type_plot"), height = "200px")
      )
    })

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

    output$bedrooms_plot <- plotly::renderPlotly({
      bedrooms_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$bedrooms_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = bedrooms_alt_text(),
        plotly::plotlyOutput(ns("bedrooms_plot"), height = "200px")
      )
    })

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

    output$household_tenure_plot <- plotly::renderPlotly({
      household_tenure_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$household_tenure_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = household_tenure_alt_text(),
        plotly::plotlyOutput(ns("household_tenure_plot"), height = "100px")
      )
    })

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

    output$average_renter_shelter_cost_plot <- plotly::renderPlotly({
      average_renter_shelter_cost_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$average_renter_shelter_cost_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = average_renter_shelter_cost_alt_text(),
        plotly::plotlyOutput(ns("average_renter_shelter_cost_plot"), height = "100px")
      )
    })
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
