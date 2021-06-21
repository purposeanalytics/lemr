#' "People" Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_people_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("people_sidebar"))
}

#' "People" Sidebar Server Functions
#'
#' @noRd
mod_sidebar_people_server <- function(id, neighbourhood) {
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
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  align = "center",
                  shiny::h3(shiny::uiOutput(ns("poverty"))),
                  shiny::h3(shiny::uiOutput(ns("poverty_city"))),
                ),
                shiny::column(
                  width = 6,
                  shiny::plotOutput(ns("poverty_plot"), height = "100px")
                )
              ),
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

      neighbourhood_profile <- lemur::neighbourhood_profiles[[neighbourhood()]]

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
          neighbourhood_profile %>%
            plot_neighbourhood_profile_distribution("population_change") +
            ggplot2::scale_x_continuous(labels = scales::percent)
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
          neighbourhood_profile %>%
            plot_neighbourhood_profile_distribution("population_density") +
            ggplot2::scale_x_continuous(labels = scales::comma)
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

      # Mean total household income ------

      output$average_total_income <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE)
        },
        res = 96,
        bg = "transparent"
      )

      # Poverty measure -----

      output$poverty <- shiny::renderUI({
        glue::glue('Poverty (LIM-AT): {scales::percent(neighbourhood_profile[["poverty"]], accuracy = 0.1)}')
      })

      output$poverty_city <- shiny::renderUI({
        glue::glue('(City: {scales::percent(city_profile[["poverty"]][["value"]], accuracy = 0.1)}%)')
      })

      output$poverty_plot <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile_distribution("poverty") +
            ggplot2::scale_x_continuous(labels = scales::percent)
        },
        res = 96,
        bg = "transparent"
      )

      # Unaffordable housing -----

      output$unaffordable_housing <- shiny::renderUI({
        glue::glue('Unaffordable housing: {scales::percent(neighbourhood_profile[["unaffordable_housing"]], accuracy = 0.1)}')
      })

      output$unaffordable_housing_city <- shiny::renderUI({
        glue::glue('(City: {city_profile[["unaffordable_housing"]][["value"]]}%)')
      })

      output$unaffordable_housing_plot <- shiny::renderPlot(
        {
          neighbourhood_profile %>%
            plot_neighbourhood_profile_distribution("unaffordable_housing") +
            ggplot2::scale_x_continuous(labels = scales::percent)
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
# mod_sidebar_people_ui("people")

## To be copied in the server
# mod_sidebar_people_server("people")
