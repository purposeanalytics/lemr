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
        shiny::div(
          mod_legend_ui(ns("legend")),
          shiny::h2("Population change"),
          bigger_padded(shiny::textOutput(ns("population_change_number"))),
          shiny::textOutput(ns("population_change_description")),
          shiny::uiOutput(ns("population_change_plot_ui")),
          shiny::hr(),
          shiny::h2("Population density"),
          bigger_padded(shiny::textOutput(ns("population_density_number"))),
          shiny::textOutput(ns("population_density_description")),
          shiny::uiOutput(ns("population_density_plot_ui")),
          shiny::hr(),
          shiny::h2("Household size"),
          shiny::textOutput(ns("household_size_description")),
          shiny::uiOutput(ns("household_size_plot_ui")),
          shiny::htmlOutput(ns("household_size_table")),
          shiny::hr(),
          shiny::h2("Average total household income"),
          shiny::textOutput(ns("average_total_household_income_description")),
          shiny::uiOutput(ns("average_total_household_income_plot_ui")),
          shiny::htmlOutput(ns("average_total_household_income_table")),
          shiny::hr(),
          shiny::h2("Unaffordable housing"),
          bigger_padded(shiny::textOutput(ns("unaffordable_housing"))),
          bigger_padded(shiny::textOutput(ns("unaffordable_housing_city"))),
          shiny::textOutput(ns("unaffordable_housing_description")),
          shiny::uiOutput(ns("unaffordable_housing_plot_ui")),
          shiny::hr(),
          shiny::h2("Low-income measure after tax"),
          bigger_padded(shiny::textOutput(ns("lim_at"))),
          bigger_padded(shiny::textOutput(ns("lim_at_city"))),
          shiny::textOutput(ns("lim_at_description")),
          shiny::uiOutput(ns("lim_at_plot_ui")),
          shiny::hr(),
          shiny::h2("Visible minority population"),
          bigger_padded(shiny::textOutput(ns("visible_minority"))),
          bigger_padded(shiny::textOutput(ns("visible_minority_city"))),
          shiny::textOutput(ns("visible_minority_description")),
          shiny::uiOutput(ns("visible_minority_plot_ui")),
          shiny::htmlOutput(ns("visible_minority_table"))
        )
      )
    })

    # Legend ----

    mod_legend_server("legend", level, neighbourhood)

    # Population change -----

    population_change <- shiny::reactive({
      get_measure(dataset(), "population_change")
    })

    population_change_formatted <- shiny::reactive({
      format_measure(population_change(), "population_change")
    })

    output$population_change_number <- shiny::renderText({
      population_change_number(population_change_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$population_change_description <- shiny::renderText({
      population_change_description(level(), neighbourhood(), population_change(), population_change_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    population_change_alt_text <- shiny::reactive({
      population_change_plot_alt_text(level(), neighbourhood())
    })

    output$population_change_plot <- plotly::renderPlotly({
      population_change_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$population_change_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = population_change_alt_text(),
        plotly::plotlyOutput(ns("population_change_plot"), height = "100px")
      )
    })

    # Population density -----

    population_density <- shiny::reactive({
      get_measure(dataset(), "population_density")
    })

    population_density_formatted <- shiny::reactive({
      format_measure(population_density(), "population_density")
    })

    output$population_density_number <- shiny::renderText({
      population_density_number(population_density_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$population_density_description <- shiny::renderText({
      population_density_description(level(), neighbourhood(), population_density(), population_density_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    population_density_alt_text <- shiny::reactive({
      population_density_plot_alt_text(level(), neighbourhood())
    })

    output$population_density_plot <- plotly::renderPlotly({
      population_density_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$population_density_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = population_density_alt_text(),
        plotly::plotlyOutput(ns("population_density_plot"), height = "100px")
      )
    })

    # Household size -----

    output$household_size_description <- shiny::renderText({
      household_size_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    household_size_alt_text <- shiny::reactive({
      household_size_plot_alt_text(level(), neighbourhood())
    })

    output$household_size_plot <- plotly::renderPlotly({
      household_size_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$household_size_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = household_size_alt_text(),
        plotly::plotlyOutput(ns("household_size_plot"), height = "200px")
      )
    })

    output$household_size_table <- shiny::renderText({
      generate_table(dataset(), "household_size", compare(), "Household Size", "Percent")
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    # Mean total household income ------

    output$average_total_household_income_description <- shiny::renderText({
      average_total_household_income_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    average_total_household_income_alt_text <- shiny::reactive({
      average_total_household_income_plot_alt_text(level(), neighbourhood())
    })

    output$average_total_household_income_plot <- plotly::renderPlotly({
      average_total_household_income_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$average_total_household_income_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = average_total_household_income_alt_text(),
        plotly::plotlyOutput(ns("average_total_household_income_plot"), height = "100px")
      )
    })

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

    output$unaffordable_housing <- shiny::renderText({
      unaffordable_housing_number(unaffordable_housing_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$unaffordable_housing_city <- shiny::renderText({
      unaffordable_housing_city(level())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$unaffordable_housing_description <- shiny::renderText({
      unaffordable_housing_description(level(), neighbourhood(), unaffordable_housing(), unaffordable_housing_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    unaffordable_housing_alt_text <- shiny::reactive({
      unaffordable_housing_plot_alt_text(level(), neighbourhood())
    })

    output$unaffordable_housing_plot <- plotly::renderPlotly({
      unaffordable_housing_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$unaffordable_housing_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = unaffordable_housing_alt_text(),
        plotly::plotlyOutput(ns("unaffordable_housing_plot"), height = "100px")
      )
    })

    # LIM-AT -----

    lim_at <- shiny::reactive({
      get_measure(dataset(), "lim_at")
    })

    lim_at_formatted <- shiny::reactive({
      format_measure(lim_at(), "lim_at")
    })

    output$lim_at <- shiny::renderText({
      lim_at_number(lim_at_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$lim_at_city <- shiny::renderText({
      lim_at_city(level())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$lim_at_description <- shiny::renderText({
      lim_at_description(level(), neighbourhood(), lim_at(), lim_at_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    lim_at_alt_text <- shiny::reactive({
      lim_at_plot_alt_text(level(), neighbourhood())
    })

    output$lim_at_plot <- plotly::renderPlotly({
      lim_at_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$lim_at_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = lim_at_alt_text(),
        plotly::plotlyOutput(ns("lim_at_plot"), height = "100px")
      )
    })

    # Visible minority population -----

    output$visible_minority <- shiny::renderText({
      visible_minority_number(dataset())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$visible_minority_city <- shiny::renderText({
      visible_minority_city(level())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$visible_minority_description <- shiny::renderText({
      visible_minority_description(level(), neighbourhood())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    visible_minority_alt_text <- shiny::reactive({
      visible_minority_plot_alt_text(level(), neighbourhood())
    })

    output$visible_minority_plot <- plotly::renderPlotly({
      visible_minority_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$visible_minority_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = visible_minority_alt_text(),
        plotly::plotlyOutput(ns("visible_minority_plot"), height = "400px")
      )
    })

    output$visible_minority_table <- shiny::renderText({
      generate_table(dataset(), "visible_minority", compare(), "Visible Minority Group", "Percent") %>%
        kableExtra::footnote(general = '"n.i.e." = not included elsewhere')
    }) %>%
      shiny::bindCache(level(), neighbourhood())
  })
}

## To be copied in the UI
# mod_sidebar_people_ui("people")

## To be copied in the server
# mod_sidebar_people_server("people")
