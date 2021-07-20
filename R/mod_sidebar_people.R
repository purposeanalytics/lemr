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
        shiny::column(
          width = 12,
          shiny::br(),
          shiny::htmlOutput(ns("legend")),
          shiny::h3("Population change"),
          shiny::h4(shiny::uiOutput(ns("population_change_number"))),
          shiny::textOutput(ns("population_change_description")),
          shiny::plotOutput(ns("population_change_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3("Population density"),
          shiny::h4(shiny::uiOutput(ns("population_density_number"))),
          shiny::textOutput(ns("population_density_description")),
          shiny::plotOutput(ns("population_density_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3("Household size"),
          shiny::textOutput(ns("household_size_description")),
          shiny::plotOutput(ns("household_size_plot"), height = "200px"),
          shiny::htmlOutput(ns("household_size_table")),
          shiny::hr(),
          shiny::h3("Average total household income"),
          shiny::textOutput(ns("average_total_income_description")),
          shiny::plotOutput(ns("average_total_income_plot"), height = "100px"),
          shiny::htmlOutput(ns("average_total_income_table")),
          shiny::hr(),
          shiny::h3("Unaffordable housing"),
          shiny::h4(shiny::uiOutput(ns("unaffordable_housing"))),
          shiny::h4(shiny::uiOutput(ns("unaffordable_housing_city"))),
          shiny::textOutput(ns("unaffordable_housing_description")),
          shiny::plotOutput(ns("unaffordable_housing_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3("Low-income measure after tax"),
          shiny::h4(shiny::uiOutput(ns("lim_at"))),
          shiny::h4(shiny::uiOutput(ns("lim_at_city"))),
          shiny::textOutput(ns("lim_at_description")),
          shiny::plotOutput(ns("lim_at_plot"), height = "100px"),
          shiny::hr(),
          shiny::h3(shiny::uiOutput(ns("visible_minority"))),
          shiny::textOutput(ns("visible_minority_description")),
          shiny::plotOutput(ns("visible_minority_plot"), height = "400px"),
          shiny::htmlOutput(ns("visible_minority_table"))
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
    })

    # Population change -----

    population_change <- shiny::reactive({
      get_measure(dataset(), "population_change")
    })

    population_change_formatted <- shiny::reactive({
      format_measure(population_change(), "population_change")
    })

    output$population_change_number <- shiny::renderUI({
      population_change_number(population_change_formatted())
    })

    output$population_change_description <- shiny::renderText({population_change
      population_change_description(level(), neighbourhood(), population_change(), population_change_formatted())
    })

    population_change_alt_text <- shiny::reactive({
      population_change_plot_alt_text(level(), neighbourhood())
    })

    output$population_change_plot <- shiny::renderPlot(
      {
        population_change_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = population_change_alt_text
    )

    # Population density -----

    population_density <- shiny::reactive({
      get_measure(dataset(), "population_density")
    })

    population_density_formatted <- shiny::reactive({
      format_measure(population_density(), "population_density")
    })

    output$population_density_number <- shiny::renderUI({
      population_density_number(population_density_formatted())
    })

    output$population_density_description <- shiny::renderText({
      population_density_description(level(), neighbourhood(), population_density(), population_density_formatted)
    })

    population_density_alt_text <- shiny::reactive({
      population_density_plot_alt_text(level(), neighbourhood())
    })

    output$population_density_plot <- shiny::renderPlot(
      {
        population_density_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = population_density_alt_text
    )

    # Household size -----

    output$household_size_description <- shiny::renderText({
      household_size_description(level(), neighbourhood())
    })

    household_size_alt_text <- shiny::reactive({
      household_size_plot_alt_text(level(), neighbourhood())
    })

    output$household_size_plot <- shiny::renderPlot(
      {
        household_size_plot(dataset(),)
      },
      res = 96,
      bg = "transparent",
      alt = household_size_alt_text
    )

    output$household_size_table <- shiny::renderText({
      generate_table(dataset, household_size, compare, "Household Size", "Percent")
    })

    # Mean total household income ------

    output$average_total_income_description <- shiny::renderText({
      switch(level(),
        "city" = "Average total income for 1 person versus 2+ person households in the City of Toronto.",
        "neighbourhood" = glue::glue("Comparison of average total income for 1 person versus 2+ person households in {neighbourhood()} versus in the City of Toronto.")
      )
    })

    household_size_plot_alt_text <- shiny::reactive({
      switch(level(),
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

    output$average_total_income_table <- shiny::renderText({
      res <- dataset() %>%
        display_neighbourhood_profile("average_total_income", compare = compare(), type = "table") %>%
        dplyr::mutate_at(dplyr::vars(-.data$group), ~ scales::dollar(.x))

      if (!compare()) {
        names(res) <- c("Household Size", "Average Total Household Income")
      } else {
        names(res)[[1]] <- "Household Size"
      }

      res %>%
        kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
        kableExtra::kable_styling()
    })

    # Unaffordable housing -----

    unaffordable_housing <- shiny::reactive({
      dataset()[["unaffordable_housing"]]
    })

    unaffordable_housing_formatted <- shiny::reactive({
      scales::percent(unaffordable_housing(), accuracy = 0.1)
    })

    output$unaffordable_housing <- shiny::renderUI({
      glue::glue("Percent of tenants with unaffordable housing: {unaffordable_housing_formatted()}")
    })

    output$unaffordable_housing_city <- shiny::renderUI({
      if (level() == "neighbourhood") {
        glue::glue('(City of Toronto: {scales::percent(lemur::city_profile[["unaffordable_housing"]], accuracy = 0.1)})')
      } else {
        NULL
      }
    })

    output$unaffordable_housing_description <- shiny::renderText({
      if (level() == "neighbourhood") {
        value_distribution <- stats::ecdf(lemur::city_profile[["unaffordable_housing_distribution"]][["value"]])
        value_percentile <- value_distribution(unaffordable_housing())
      }

      switch(level(),
        "city" = "Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods.",
        "neighbourhood" = glue::glue("Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods. The value for {neighbourhood()}, {unaffordable_housing_formatted()}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' percent of tenants with unaffordable housing.")
      )
    })

    unaffordable_housing_alt_text <- shiny::reactive({
      values <- lemur::city_profile[["unaffordable_housing_distribution"]][["value"]]

      alt_text <- glue::glue("Histogram showing the distribution of percent of tenants with unaffordable housing for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} of tenants with unaffordable housing with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
        min = min(values),
        max = max(values),
        skew_min = stats::quantile(values, 0.1),
        skew_max = stats::quantile(values, 0.9)
      )

      if (level() == "neighbourhood") {
        neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood()}'s percent of tenants with unaffordable housing is highlighted.")
        alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
      }

      alt_text
    })

    output$unaffordable_housing_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("unaffordable_housing", compare = compare(), binwidth = 0.025) +
          ggplot2::scale_x_continuous(labels = scales::label_percent())
      },
      res = 96,
      bg = "transparent",
      alt = unaffordable_housing_alt_text
    )

    # LIM-AT -----

    lim_at <- shiny::reactive({
      dataset()[["lim_at"]]
    })

    lim_at_formatted <- shiny::reactive({
      scales::percent(lim_at(), accuracy = 0.1)
    })

    output$lim_at <- shiny::renderUI({
      glue::glue('Percent of people under LIM-AT: {lim_at_formatted()}')
    })

    output$lim_at_city <- shiny::renderUI({
      if (level() == "neighbourhood") {
        glue::glue('(City of Toronto: {scales::percent(lemur::city_profile[["lim_at"]], accuracy = 0.1)}%)')
      } else {
        NULL
      }
    })

    output$lim_at_description <- shiny::renderText({
      if (level() == "neighbourhood") {
        value_distribution <- stats::ecdf(lemur::city_profile[["lim_at_distribution"]][["value"]])
        value_percentile <- value_distribution(lim_at())
      }

      switch(level(),
        "city" = "Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods.",
        "neighbourhood" = glue::glue("Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods. The value for {neighbourhood()}, {lim_at_formatted()}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
      )
    })

    lim_at_alt_text <- shiny::reactive({
      values <- lemur::city_profile[["lim_at_distribution"]][["value"]]

      alt_text <- glue::glue("Histogram showing the distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
        min = min(values),
        max = max(values),
        skew_min = stats::quantile(values, 0.1),
        skew_max = stats::quantile(values, 0.9)
      )

      if (level() == "neighbourhood") {
        neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood()}'s value is highlighted.")
        alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
      }

      alt_text
    })

    output$lim_at_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("lim_at", compare = compare(), binwidth = 0.025) +
          ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1))
      },
      res = 96,
      bg = "transparent",
      alt = lim_at_alt_text
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

    output$visible_minority_description <- shiny::renderText({
      switch(level(),
        "city" = "Breakdown of visible minority groups in the City of Toronto.",
        "neighbourhood" = glue::glue("Comparison of visible minority groups in {neighbourhood()} versus in the City of Toronto.")
      )
    })

    visible_minority_alt_text <- shiny::reactive({
      switch(level(),
        "city" = "Bar chart showing the breakdown of visible minority groups in the City of Toronto. The data is in the table that follows.",
        "neighbourhood" = glue::glue("Bar chart comparing the breakdown of visible minority groups in {neighbourhood()} versus in the City of Toronto. The data is in the table that follows.")
      )
    })

    output$visible_minority_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("visible_minority", width = 20, compare = compare()) +
          ggplot2::labs(caption = 'Note: "n.i.e." = not included elsewhere')
      },
      res = 96,
      bg = "transparent",
      alt = visible_minority_alt_text
    )

    output$visible_minority_table <- shiny::renderText({
      res <- dataset() %>%
        display_neighbourhood_profile("visible_minority", compare = compare(), type = "table")

      if (!compare()) {
        names(res) <- c("Visibile Minority Group", "Percent")
      } else {
        names(res)[[1]] <- "Visibile Minority Group"
      }

      res %>%
        kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
        kableExtra::kable_styling() %>%
        kableExtra::footnote(general = '"n.i.e." = not included elsewhere')
    })
  })
}

## To be copied in the UI
# mod_sidebar_people_ui("people")

## To be copied in the server
# mod_sidebar_people_server("people")
