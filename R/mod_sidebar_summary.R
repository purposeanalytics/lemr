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
          shiny::h2("Apartment buildings"),
          bigger_padded(shiny::textOutput(ns("number_of_apartments_number"))),
          shiny::textOutput(ns("number_of_apartments_number_description")),
          shiny::plotOutput(ns("number_of_apartments_number_plot"), height = "100px"),
          shiny::h2("RentSafeTO evaluation scores"),
          bigger_padded(shiny::textOutput(ns("apartment_building_evaluation")))
        )
      )
    })

    # Number of apartments -----

    number_of_apartments <- shiny::reactive({
      get_measure(dataset(), "number_of_apartments")
    })

    number_of_apartments_formatted <- shiny::reactive({
      format_measure(number_of_apartments(), "number_of_apartments")
    })

    output$number_of_apartments_number <- shiny::renderText({
      number_of_apartments_number(number_of_apartments_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$number_of_apartments_description <- shiny::renderText({
      number_of_apartments_description(level(), neighbourhood(), number_of_apartments(), number_of_apartments_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    number_of_apartments_alt_text <- shiny::reactive({
      number_of_apartments_plot_alt_text(level(), neighbourhood())
    })

    output$number_of_apartments_plot <- shiny::renderPlot(
      {
        number_of_apartments_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = number_of_apartments_alt_text
    ) %>%
      shiny::bindCache(level(), neighbourhood())


    output$apartment_building_evaluation <- shiny::renderText({
      if(level() == "neighbourhood") {
        data <- lemur::apartment_building_evaluation %>%
          dplyr::filter(.data$neighbourhood == neighbourhood())
      } else {
        data <- lemur::apartment_building_evaluation
      }

      glue::glue("Median score: {median(data[['score']], na.rm = TRUE)}/100")
    })
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
