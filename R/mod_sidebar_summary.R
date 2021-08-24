#' "Summary" sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("summary_sidebar"))
}

#' sidebar_places Server Functions
#'
#' @noRd
mod_sidebar_summary_server <- function(id, neighbourhood) {
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

    output$summary_sidebar <- shiny::renderUI({
      shiny::tagList(
        shiny::div(
          shiny::htmlOutput(ns("legend")),
          shiny::h2("Low-end of market rentals"),
          shiny::htmlOutput(ns("lem_table")),
          shiny::h2("Apartment buildings"),
          bigger_padded(shiny::textOutput(ns("number_of_apartments_number"))),
          shiny::textOutput(ns("number_of_apartments_description")),
          plotly::plotlyOutput(ns("number_of_apartments_plot"), height = "100px"),
          shiny::textOutput(ns("number_of_units_description")),
          plotly::plotlyOutput(ns("number_of_units_plot"), height = "100px"),
          shiny::h2("RentSafeTO evaluation scores"),
          bigger_padded(shiny::textOutput(ns("apartment_building_evaluation_number"))),
          shiny::textOutput(ns("apartment_building_evaluation_description")),
          plotly::plotlyOutput(ns("apartment_building_evaluation_plot"), height = "100px"),
          shiny::h2("Amenity density"),
          shiny::textOutput(ns("amenity_density_description")),
          shiny::plotOutput(ns("amenity_density_plot"), height = "150px"),
          shiny::htmlOutput(ns("amenity_density_table"))
        )
      )
    })

    output$test <- plotly::renderPlotly({
      plot_ly(x, x = ~x, y = ~y, type = "bar", hoverinfo = "skip") %>%
        layout(
          yaxis = list(title = NA, showline = FALSE, showgrid = FALSE, showticklabels = FALSE, fixedrange = TRUE),
          xaxis = list(title = NA, showline = FALSE, fixedrange = TRUE),
          margin = list(l = 0, r = 0, t = 0, b = 0),
          barmode = "stack"
        ) %>%
        config(displayModeBar = FALSE)
    }
    )

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

    # LEM ----

    output$lem_table <- shiny::renderText({
      dataset()[["lem"]] %>%
        kableExtra::kable() %>%
        kableExtra::kable_styling()
    })

    # Number of apartments -----

    number_of_apartments <- shiny::reactive({
      get_measure(dataset(), "number_of_apartments")
    })

    number_of_apartments_formatted <- shiny::reactive({
      format_measure(number_of_apartments(), "number_of_apartments")
    })

    number_of_units <- shiny::reactive({
      get_measure(dataset(), "number_of_units")
    })

    number_of_units_formatted <- shiny::reactive({
      format_measure(number_of_units(), "number_of_units")
    })

    output$number_of_apartments_number <- shiny::renderText({
      number_of_apartments_number(number_of_apartments_formatted(), number_of_units_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$number_of_apartments_description <- shiny::renderText({
      number_of_apartments_description(level(), neighbourhood(), number_of_apartments(), number_of_apartments_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    number_of_apartments_alt_text <- shiny::reactive({
      number_of_apartments_plot_alt_text(level(), neighbourhood())
    })

    output$number_of_apartments_plot <- plotly::renderPlotly(
      {
        number_of_apartments_plot(dataset(), compare())
      }
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    output$number_of_units_description <- shiny::renderText({
      number_of_units_description(level(), neighbourhood(), number_of_units(), number_of_units_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    number_of_units_alt_text <- shiny::reactive({
      number_of_units_plot_alt_text(level(), neighbourhood())
    })

    output$number_of_units_plot <- plotly::renderPlotly(
      {
        number_of_units_plot(dataset(), compare())
      }
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    # Apartment building evaluation scores (RentSafeTO) ----

    apartment_building_evaluation <- shiny::reactive({
      get_measure(dataset(), "apartment_building_evaluation")
    })

    apartment_building_evaluation_formatted <- shiny::reactive({
      format_measure(apartment_building_evaluation(), "apartment_building_evaluation")
    })

    output$apartment_building_evaluation_number <- shiny::renderText({
      apartment_building_evaluation_number(apartment_building_evaluation_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$apartment_building_evaluation_description <- shiny::renderText({
      apartment_building_evaluation_description(level(), neighbourhood(), apartment_building_evaluation(), apartment_building_evaluation_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    apartment_building_evaluation_alt_text <- shiny::reactive({
      apartment_building_evaluation_plot_alt_text(level(), neighbourhood())
    })

    output$apartment_building_evaluation_plot <- plotly::renderPlotly(
      {
        apartment_building_evaluation_plot(dataset(), compare())
      }
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    # Amenity density ------

    output$amenity_density_description <- shiny::renderText({
      amenity_density_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    amenity_density_alt_text <- shiny::reactive({
      amenity_density_plot_alt_text(level(), neighbourhood())
    })

    output$amenity_density_plot <- shiny::renderPlot(
      {
        amenity_density_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = amenity_density_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())

    output$amenity_density_table <- shiny::renderText({
      generate_table(dataset(), "amenity_density", compare(), "Amenity density", "Percent") %>%
        kableExtra::footnote(general = "A very small number of areas have unknown amenity density, so values may not add up to 100%.")
    }) %>%
      shiny::bindCache(level(), neighbourhood())
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
