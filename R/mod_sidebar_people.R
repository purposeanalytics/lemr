#' "People" Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_people_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("people_sidebar"))
}

#' "People" Sidebar Server Functions
#'
#' @noRd
mod_sidebar_people_server <- function(id, neighbourhood) {
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
          shiny::htmlOutput(ns("legend")),

          shiny::h3("Population Change"),
          shiny::h3(shiny::uiOutput(ns("population_change_number"))),
          shiny::plotOutput(ns("population_change_plot"), height = "100px"),

          shiny::h3("Population Density"),
          shiny::h3(shiny::uiOutput(ns("population_density_number"))),
          shiny::plotOutput(ns("population_density_plot"), height = "100px"),

          shiny::h3("Household size"),
          shiny::plotOutput(ns("household_size"), height = "200px"),

          shiny::h3("Mean total household income"),
          shiny::plotOutput(ns("average_total_income"), height = "100px"),

          shiny::fluidRow(
          shiny::column(
            width = 6,
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
            shiny::h3(shiny::uiOutput(ns("lim_at"))),
            shiny::h3(shiny::uiOutput(ns("lim_at_city"))),
          ),
          shiny::column(
            width = 6,
            shiny::plotOutput(ns("lim_at_plot"), height = "100px")
          )
          ),

          shiny::h3(shiny::uiOutput(ns("visible_minority"))),
          shiny::plotOutput(ns("visible_minority_plot"), height = "400px")
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

    output$legend <- shiny::renderText({
      plot_legend()
    })

    # Population change -----

    output$population_change_number <- shiny::renderUI({
      pop_change <- dataset()[["population_change"]]

      pop_change_percent <- pop_change %>%
        abs() %>%
        scales::percent(accuracy = 0.1)

      sign <- ifelse(pop_change > 0, "+", "-")

      glue::glue("2011 to 2016: {sign}{pop_change_percent}")
    })

    output$population_change_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("population_change", compare = compare(), binwidth = 0.01) +
          ggplot2::scale_x_continuous(labels = scales::label_percent())
      },
      res = 96,
      bg = "transparent"
    )

    # Population density -----

    output$population_density_number <- shiny::renderUI({
      glue::glue('{scales::comma(round(dataset()[["population_density"]]))} people per square km')
    })

    output$population_density_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("population_density", compare = compare(), binwidth = 1000) +
          ggplot2::scale_x_continuous(labels = scales::comma)
      },
      res = 96,
      bg = "transparent"
    )

    # Household size -----

    output$household_size <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile("household_size", width = 10, compare = compare())
      },
      res = 96,
      bg = "transparent"
    )

    # Mean total household income ------

    output$average_total_income <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE, compare = compare())
      },
      res = 96,
      bg = "transparent"
    )

    # LIM-AT -----

    output$lim_at <- shiny::renderUI({
      glue::glue('LIM-AT: {scales::percent(dataset()[["lim_at"]], accuracy = 0.1)}')
    })

    output$lim_at_city <- shiny::renderUI({
      if (sidebar_level() == "neighbourhood") {
        glue::glue('(City: {scales::percent(lemur::city_profile[["lim_at"]], accuracy = 0.1)}%)')
      } else {
        NULL
      }
    })

    output$lim_at_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("lim_at", compare = compare(), binwidth = 0.025) +
          ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1))
      },
      res = 96,
      bg = "transparent"
    )

    # Unaffordable housing -----

    output$unaffordable_housing <- shiny::renderUI({
      glue::glue('Unaffordable housing: {scales::percent(dataset()[["unaffordable_housing"]], accuracy = 0.1)}')
    })

    output$unaffordable_housing_city <- shiny::renderUI({
      if (sidebar_level() == "neighbourhood") {
        glue::glue('(City: {scales::percent(lemur::city_profile[["unaffordable_housing"]], accuracy = 0.1)})')
      } else {
        NULL
      }
    })

    output$unaffordable_housing_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("unaffordable_housing", compare = compare(), binwidth = 0.025) +
          ggplot2::scale_x_continuous(labels = scales::label_percent())
      },
      res = 96,
      bg = "transparent"
    )

    # Visible minority population -----

    output$visible_minority <- shiny::renderUI({
      prop <- dataset()[["visible_minority"]] %>%
        dplyr::filter(.data$group != "Not a visible minority") %>%
        dplyr::pull(.data$prop) %>%
        sum() %>%
        scales::percent(accuracy = 0.1)

      city_prop <- lemur::city_profile[["visible_minority"]] %>%
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
        dataset() %>%
          plot_neighbourhood_profile("visible_minority", width = 20, compare = compare())
      },
      res = 96,
      bg = "transparent"
    )
  })
}

## To be copied in the UI
# mod_sidebar_people_ui("people")

## To be copied in the server
# mod_sidebar_people_server("people")
