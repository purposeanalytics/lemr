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
          shiny::br(),
          shiny::htmlOutput(ns("legend")),
          shiny::h3("Population Change"),
          shiny::h4(shiny::uiOutput(ns("population_change_number"))),
          shiny::textOutput(ns("population_change_description")),
          shiny::plotOutput(ns("population_change_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3("Population Density"),
          shiny::h4(shiny::uiOutput(ns("population_density_number"))),
          shiny::textOutput(ns("population_density_description")),
          shiny::plotOutput(ns("population_density_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3("Household size"),
          shiny::textOutput(ns("household_size_description")),
          shiny::plotOutput(ns("household_size_plot"), height = "200px"),
          reactable::reactableOutput(ns("household_size_table")),
          shiny::hr(),
          shiny::h3("Average total household income"),
          shiny::textOutput(ns("average_total_income_description")),
          shiny::plotOutput(ns("average_total_income_plot"), height = "100px"),
          reactable::reactableOutput(ns("average_total_income_table")),
          shiny::hr(),
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
          shiny::hr(),
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
          shiny::hr(),
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

    population_change <- shiny::reactive({
      pop_change <- dataset()[["population_change"]]
    })

    population_change_formatted <- shiny::reactive({
      pop_change_percent <- population_change() %>%
        abs() %>%
        scales::percent(accuracy = 0.1)

      sign <- ifelse(population_change() > 0, "+", "-")

      glue::glue("{sign}{pop_change_percent}")
    })

    output$population_change_number <- shiny::renderUI({
      glue::glue("2011 to 2016: {population_change_formatted()}")
    })

    output$population_change_description <- shiny::renderText({
      if (sidebar_level() == "neighbourhood") {
        value_distribution <- ecdf(city_profile[["population_change_distribution"]][["value"]])
        value_percentile <- value_distribution(population_change())
      }

      switch(sidebar_level(),
        "city" = "Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods.",
        "neighbourhood" = glue::glue("Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods. The value for {neighbourhood()}, {population_change_formatted()}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population change.")
      )
    })

    population_change_plot_alt_text <- shiny::reactive({
      values <- city_profile[["population_change_distribution"]][["value"]]

      alt_text <- glue::glue("Histogram showing the distribution of population change from 2011 to 2016 for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} population change and the distribution is heavily skewed left with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
        min = min(values),
        max = max(values),
        skew_min = quantile(values, 0.1),
        skew_max = quantile(values, 0.9)
      )

      if (sidebar_level() == "neighbourhood") {
        neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood()}'s population change is highlighted.")
        alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
      }

      alt_text
    })

    output$population_change_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("population_change", compare = compare(), binwidth = 0.01) +
          ggplot2::scale_x_continuous(labels = scales::label_percent())
      },
      res = 96,
      bg = "transparent",
      alt = population_change_plot_alt_text
    )

    # Population density -----

    population_density <- shiny::reactive({
      dataset()[["population_density"]]
    })

    population_density_formatted <- shiny::reactive({
      scales::comma(round(population_density()))
    })

    output$population_density_number <- shiny::renderUI({
      glue::glue("{population_density_formatted()} people per square kilometre")
    })

    output$population_density_description <- shiny::renderText({
      if (sidebar_level() == "neighbourhood") {
        value_distribution <- ecdf(city_profile[["population_density_distribution"]][["value"]])
        value_percentile <- value_distribution(population_density())
      }

      switch(sidebar_level(),
        "city" = "Distribution of population density for each of the City of Toronto neighbourhoods.",
        "neighbourhood" = glue::glue("Distribution of population density for each of the City of Toronto neighbourhoods. The value for {neighbourhood()}, {population_density_formatted()} people per square kilometre, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population density.")
      )
    })

    population_density_plot_alt_text <- shiny::reactive({
      values <- city_profile[["population_density_distribution"]][["value"]]

      alt_text <- glue::glue("Histogram showing the distribution of population density for each of Toronto's neighbourhoods. The values range from {round(min)} to {round(max)} people per square kilometer and the distribution is heavily skewed left with most values between {round(skew_min)} and {round(skew_max)}.",
        min = min(values),
        max = max(values),
        skew_min = quantile(values, 0.1),
        skew_max = quantile(values, 0.9)
      )

      if (sidebar_level() == "neighbourhood") {
        neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood()}'s population density is highlighted.")
        alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
      }

      alt_text
    })

    output$population_density_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("population_density", compare = compare(), binwidth = 1000) +
          ggplot2::scale_x_continuous(labels = scales::comma)
      },
      res = 96,
      bg = "transparent",
      alt = population_density_plot_alt_text
    )

    # Household size -----

    output$household_size_description <- shiny::renderText({
      switch(sidebar_level(),
        "city" = "Distribution of household sizes for all households in the City of Toronto.",
        "neighbourhood" = glue::glue("Comparison of household sizes for households in {neighbourhood()} versus all households in the City of Toronto.")
      )
    })

    household_size_plot_alt_text <- shiny::reactive({
      switch(sidebar_level(),
        "city" = "Bar chart showing distribution of household sizes for all households in the City of Toronto. The data is in the table that follows.",
        "neighbourhood" = glue::glue("Bar chart comparing household sizes for households in {neighbourhood()} versus all households in the City of Toronto. The data is in the table that follows.")
      )
    })

    output$household_size_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("household_size", width = 10, compare = compare())
      },
      res = 96,
      bg = "transparent",
      alt = household_size_plot_alt_text
    )

    output$household_size_table <- reactable::renderReactable({
      res <- dataset() %>%
        display_neighbourhood_profile("household_size", compare = compare(), type = "table")

      if (!compare()) {
        names(res) <- c("Household Size", "Percent")
      } else {
        names(res)[[1]] <- "Household Size"
      }

      res %>%
        reactable::reactable(sortable = FALSE)
    })

    # Mean total household income ------

    output$average_total_income_description <- shiny::renderText({
      switch(sidebar_level(),
        "city" = "Average total income for 1 person versus 2+ person households in the City of Toronto.",
        "neighbourhood" = glue::glue("Comparison of average total income for 1 person versus 2+ person households in {neighbourhood()} versus in the City of Toronto.")
      )
    })

    household_size_plot_alt_text <- shiny::reactive({
      switch(sidebar_level(),
        "city" = "Bar chart comparing average total income for 1 person versus 2+ person households in the City of Toronto. The data is in the table that follows.",
        "neighbourhood" = glue::glue("Bar chart comparing average total income for 1 person versus 2+ person households in {neighbourhood()} versus in the City of Toronto. The data is in the table that follows.")
      )
    })

    output$average_total_income_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE, compare = compare())
      },
      res = 96,
      bg = "transparent",
      alt = household_size_plot_alt_text
    )

    output$average_total_income_table <- reactable::renderReactable({
      res <- dataset() %>%
        display_neighbourhood_profile("average_total_income", compare = compare(), type = "table") %>%
        dplyr::mutate_at(dplyr::vars(-group), ~ scales::dollar(.x))

      if (!compare()) {
        names(res) <- c("Household Size", "Average Total Household Income")
      } else {
        names(res)[[1]] <- "Household Size"
      }

      res %>%
        reactable::reactable(sortable = FALSE)
    })

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
        glue::glue("Visible Minority Population: {prop}<br>(Toronto: {city_prop})")
      )
    })

    output$visible_minority_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("visible_minority", width = 20, compare = compare())
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
