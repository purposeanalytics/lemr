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
    # shiny::plotOutput(ns("household_size"))
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
        search_method_neighbourhood <- search_method() == "neighbourhood" | !is.null(neighbourhood())
        if (search_method_neighbourhood) {
          neighbourhood()
        } else {
          address$neighbourhood()
        }
      }
    )

    output$header <- shiny::renderUI(shiny::h1(display_neighbourhood()))

    shiny::observeEvent(display_neighbourhood(), {
      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

      output$population <- shiny::renderUI({
        shiny::h1(
          glue::glue('Population: {scales::comma(neighbourhood_profile[["population"]])} ({scales::comma(neighbourhood_profile[["households"]])} households)')
        )
      })

      output$population_density_plot <- shiny::renderPlot(
        {
          ggplot2::ggplot() +
            ggplot2::geom_density(data = city_profile[["population_density"]][["distribution"]], ggplot2::aes(x = value), fill = "grey", color = "grey") +
            ggplot2::geom_vline(ggplot2::aes(xintercept = neighbourhood_profile[["population_density"]]), color = "darkgreen") +
            theme_lemur() +
            ggplot2::scale_x_continuous(labels = scales::comma) +
            ggplot2::theme(
              axis.title = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank()
            )
        },
        res = 96,
        bg = "transparent"
      )

      output$population_density <- shiny::renderUI({
        shiny::fluidRow(
          shiny::column(
            width = 3,
            align = "center",
            shiny::h3("Population density"),
            shiny::h4(glue::glue('{scales::comma(round(neighbourhood_profile[["population_density"]]))}  people per square km'))
          ),
          shiny::column(
            width = 9,
            shiny::plotOutput(ns("population_density_plot"), height = "200px")
          )
        )
      })
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
