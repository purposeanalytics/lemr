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
  shiny::uiOutput(ns("sidebar_ui"))
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

    shiny::observeEvent(display_neighbourhood(), {
      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

      # UI ----

      # Do the whole thing as one UI rendered in so it all loads at once - better than having headers / plots / etc appear at different times!
      # Then all of the layout can be updated in one place too :)

      output$sidebar_ui <- shiny::renderUI({
        shiny::tagList(
          shiny::h1(shiny::uiOutput(ns("header"))),
          shiny::h2(shiny::uiOutput(ns("population"))),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Population Change"),
              shiny::h3(shiny::uiOutput(ns("population_change_number"))),
              shiny::plotOutput(ns("population_change_plot"), height = "100px")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Population Density"),
              shiny::h3(shiny::uiOutput(ns("population_density_number"))),
              shiny::plotOutput(ns("population_density_plot"), height = "100px")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Household size"),
              shiny::plotOutput(ns("household_size"), height = "200px")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Mean total household income"),
              shiny::plotOutput(ns("average_total_income"), height = "100px"),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  align = "center",
                  shiny::h3(shiny::uiOutput(ns("unaffordable_housing"))),
                  shiny::h3(shiny::uiOutput(ns("unaffordable_housing_city"))),
                ),
                shiny::column(
                  width = 6,
                  shiny::plotOutput(ns("unaffordable_housing_plot"), height = "100px")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
                  shiny::h3(shiny::uiOutput(ns("visible_minority"))),
              shiny::plotOutput(ns("visible_minority_plot"), height = "400px")
            )
        )
        )
      })

      # Neighbourhood ----

      output$header <- shiny::renderUI(display_neighbourhood())

      # Population -----

      output$population <- shiny::renderUI({
        glue::glue('Population: {scales::comma(neighbourhood_profile[["population"]])} ({scales::comma(neighbourhood_profile[["households"]])} households)')
      })

      # Population change -----

      output$population_change_number <- shiny::renderUI({
        pop_change <- neighbourhood_profile[["population_change"]]

        pop_change_percent <- pop_change %>%
          abs() %>%
          scales::percent(accuracy = 0.1)

        sign <- ifelse(pop_change > 0, "+", "-")

        glue::glue("2011 to 2016: {sign}{pop_change_percent}")
      })

      output$population_change_plot <- shiny::renderPlot(
          {
            ggplot2::ggplot() +
              ggplot2::geom_density(data = city_profile[["population_change"]][["distribution"]], ggplot2::aes(x = value), fill = "grey", color = "grey") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = neighbourhood_profile[["population_change"]]), color = "darkgreen") +
              theme_lemur() +
              ggplot2::scale_x_continuous(labels = scales::percent) +
              ggplot2::theme(
                axis.title = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank()
              )
          },
          res = 96,
          bg = "transparent"
        )

      # Population density -----

      output$population_density_number <- shiny::renderUI({
        glue::glue('{scales::comma(round(neighbourhood_profile[["population_density"]]))} people per square km')
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

      # Household size -----

      output$household_size <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("household_size", width = 10)
        },
        res = 96,
        bg = "transparent"
      )

      # Income and poverty ------

      ## Mean total household income ------

      output$average_total_income <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE)
        },
        res = 96,
        bg = "transparent"
      )

      ## Unaffordable housing -----

      output$unaffordable_housing <- shiny::renderUI({
        glue::glue('Unaffordable housing: {scales::percent(neighbourhood_profile[["unaffordable_housing"]], accuracy = 0.1)}')
      })

      output$unaffordable_housing_city <- shiny::renderUI({
        glue::glue('(City: {city_profile[["unaffordable_housing"]][["value"]]}%)')
      })

      output$unaffordable_housing_plot <- shiny::renderPlot(
        {
          ggplot2::ggplot() +
            ggplot2::geom_density(data = city_profile[["unaffordable_housing"]][["distribution"]], ggplot2::aes(x = value), fill = "grey", color = "grey") +
            ggplot2::geom_vline(ggplot2::aes(xintercept = neighbourhood_profile[["unaffordable_housing"]]), color = "darkgreen") +
            theme_lemur() +
            ggplot2::scale_x_continuous(labels = scales::percent) +
            ggplot2::theme(
              axis.title = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank()
            )
        },
        res = 96,
        bg = "transparent"
      )

      # Visible minority population -----

      output$visible_minority <- shiny::renderUI({
        prop <- neighbourhood_profile[["visible_minority"]] %>%
          dplyr::filter(.data$group != "Not a visible minority") %>%
          dplyr::pull(.data$prop) %>%
          sum() %>%
          scales::percent(accuracy = 0.1)

        city_prop <- city_profile[["visible_minority"]] %>%
          dplyr::filter(.data$group != "Not a visible minority") %>%
          dplyr::pull(.data$prop) %>%
          sum() %>%
          scales::percent(accuracy = 0.1)

        shiny::HTML(
          glue::glue("Visible Minority Population: {prop}<br>(City of Toronto: {city_prop})")
        )
      })

      output$visible_minority_plot <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("visible_minority", width = 20)
        },
        res = 96,
        bg = "transparent"
      )
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
