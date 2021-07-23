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
          shiny::h2("Population change"),
          bigger(shiny::textOutput(ns("population_change_number"))),
          shiny::textOutput(ns("population_change_description")),
          shiny::plotOutput(ns("population_change_plot"), height = "100px"),
          shiny::hr(),
          shiny::h2("Population density"),
          bigger(shiny::uiOutput(ns("population_density_number"))),
          shiny::textOutput(ns("population_density_description")),
          shiny::plotOutput(ns("population_density_plot"), height = "100px"),
          shiny::hr(),
          shiny::h2("Household size"),
          shiny::textOutput(ns("household_size_description")),
          shiny::plotOutput(ns("household_size_plot"), height = "200px"),
          shiny::htmlOutput(ns("household_size_table")),
          shiny::hr(),
          shiny::h2("Average total household income"),
          shiny::textOutput(ns("average_total_household_income_description")),
          shiny::plotOutput(ns("average_total_household_income_plot"), height = "100px"),
          shiny::htmlOutput(ns("average_total_household_income_table")),
          shiny::hr(),
          shiny::h2("Unaffordable housing"),
          shiny::h4(shiny::uiOutput(ns("unaffordable_housing"))),
          shiny::h4(shiny::uiOutput(ns("unaffordable_housing_city"))),
          shiny::textOutput(ns("unaffordable_housing_description")),
          shiny::plotOutput(ns("unaffordable_housing_plot"), height = "100px"),
          shiny::hr(),
          shiny::h2("Low-income measure after tax"),
          shiny::h4(shiny::uiOutput(ns("lim_at"))),
          shiny::h4(shiny::uiOutput(ns("lim_at_city"))),
          shiny::textOutput(ns("lim_at_description")),
          shiny::plotOutput(ns("lim_at_plot"), height = "100px"),
          shiny::hr(),
          shiny::h2(shiny::uiOutput(ns("visible_minority"))),
          shiny::h4(shiny::uiOutput(ns("visible_minority_city"))),
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

    output$population_change_number <- shiny::renderText({
      population_change_number(population_change_formatted())
    })

    output$population_change_description <- shiny::renderText({
      population_change
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
      population_density_description(level(), neighbourhood(), population_density(), population_density_formatted())
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
        household_size_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = household_size_alt_text
    )

    output$household_size_table <- shiny::renderText({
      generate_table(dataset(), "household_size", compare(), "Household Size", "Percent")
    })

    # Mean total household income ------

    output$average_total_household_income_description <- shiny::renderText({
      average_total_household_income_description(level(), neighbourhood())
    })

    average_total_household_income_alt_text <- shiny::reactive({
      average_total_household_income_plot_alt_text(level(), neighbourhood())
    })

    output$average_total_household_income_plot <- shiny::renderPlot(
      {
        average_total_household_income_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = average_total_household_income_alt_text
    )

    output$average_total_household_income_table <- shiny::renderText({
      generate_table(dataset(), "average_total_income", compare(), "Household Size", "Average Total Household", format = "dollar")
    })

    # Unaffordable housing -----

    unaffordable_housing <- shiny::reactive({
      get_measure(dataset(), "unaffordable_housing")
    })

    unaffordable_housing_formatted <- shiny::reactive({
      format_measure(unaffordable_housing(), "unaffordable_housing")
    })

    output$unaffordable_housing <- shiny::renderUI({
      unaffordable_housing_number(unaffordable_housing_formatted())
    })

    output$unaffordable_housing_city <- shiny::renderUI({
      unaffordable_housing_city(level())
    })

    output$unaffordable_housing_description <- shiny::renderText({
      unaffordable_housing_description(level(), neighbourhood(), unaffordable_housing(), unaffordable_housing_formatted())
    })

    unaffordable_housing_alt_text <- shiny::reactive({
      unaffordable_housing_plot_alt_text(level(), neighbourhood())
    })

    output$unaffordable_housing_plot <- shiny::renderPlot(
      {
        lemur:::unaffordable_housing_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = unaffordable_housing_alt_text
    )

    # LIM-AT -----

    lim_at <- shiny::reactive({
      get_measure(dataset(), "lim_at")
    })

    lim_at_formatted <- shiny::reactive({
      format_measure(lim_at(), "lim_at")
    })

    output$lim_at <- shiny::renderUI({
      lim_at_number(lim_at_formatted())
    })

    output$lim_at_city <- shiny::renderUI({
      lim_at_city(level())
    })

    output$lim_at_description <- shiny::renderText({
      lim_at_description(level(), neighbourhood(), lim_at(), lim_at_formatted())
    })

    lim_at_alt_text <- shiny::reactive({
      lim_at_plot_alt_text(level(), neighbourhood())
    })

    output$lim_at_plot <- shiny::renderPlot(
      {
        lim_at_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = lim_at_alt_text
    )


    # Visible minority population -----

    output$visible_minority <- shiny::renderUI({
      visible_minority_number(dataset())
    })

    output$visible_minority_city <- shiny::renderUI({
      visible_minority_city(level())
    })

    output$visible_minority_description <- shiny::renderText({
      visible_minority_description(level(), neighbourhood())
    })

    visible_minority_alt_text <- shiny::reactive({
      visible_minority_plot_alt_text(level(), neighbourhood())
    })

    output$visible_minority_plot <- shiny::renderPlot(
      {
        visible_minority_plot(dataset(), compare())
      },
      res = 96,
      bg = "transparent",
      alt = visible_minority_alt_text
    )

    output$visible_minority_table <- shiny::renderText({
      generate_table(dataset(), "visible_minority", compare(), "Visible Minority Group", "Percent") %>%
        kableExtra::footnote(general = '"n.i.e." = not included elsewhere')
    })
  })
}

## To be copied in the UI
# mod_sidebar_people_ui("people")

## To be copied in the server
# mod_sidebar_people_server("people")
