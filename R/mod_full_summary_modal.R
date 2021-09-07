#' full_summary_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_full_summary_modal_ui <- function(id) {
  ns <- NS(id)

  shiny::actionButton(ns("modal"), label = "Full Summary")
}

#' full_summary_modal Server Functions
#'
#' @noRd
mod_full_summary_modal_server <- function(id, level, neighbourhood, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$modal, {
      shiny::showModal(shiny::modalDialog(
        style = "margin-top: 2em;
margin-left: 3em;
margin-right: 3em;",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h1(shiny::textOutput(ns("header"))),
            shiny::hr()
          ),
          shiny::column(
            width = 4,
            shiny::h2("Summary statistics"),
            shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::htmlOutput(ns("households")),
              shiny::htmlOutput(ns("renters")),
              shiny::htmlOutput(ns("core_housing_need")),
            ),
            shiny::column(
              width = 6,
              shiny::htmlOutput(ns("population"))
            ),
            shiny::column(
              width = 12,
              shiny::h3("Estimated rental supply"),
              shiny::tags$i("Coming soon!"),
              shiny::h3("Estimated annual availability of low-end of market rental"),
              shiny::htmlOutput(ns("lem_table")),
              shiny::h3("Apartment buildings"),
              bigger_padded(shiny::textOutput(ns("number_of_apartments_number"))),
              shiny::textOutput(ns("number_of_apartments_description")),
              shiny::uiOutput(ns("number_of_apartments_plot_ui")),
              shiny::h3("RentSafeTO evaluation scores"),
              bigger_padded(shiny::textOutput(ns("apartment_building_evaluation_number"))),
              shiny::textOutput(ns("apartment_building_evaluation_description")),
              shiny::uiOutput(ns("apartment_building_evaluation_plot_ui")),
              shiny::h3("Amenity density"),
              shiny::textOutput(ns("amenity_density_description")),
              shiny::uiOutput(ns("amenity_density_plot_ui")),
              shiny::htmlOutput(ns("amenity_density_table"))
            )
            )
          ),
          shiny::column(
            width = 4,
            shiny::h2("Housing characteristics"),
            shiny::h3("Apartment units"),
            shiny::textOutput(ns("number_of_units_description")),
            shiny::uiOutput(ns("number_of_units_plot_ui")),
            shiny::h3("Housing structure type"),
            shiny::textOutput(ns("structure_type_description")),
            shiny::uiOutput(ns("structure_type_plot_ui")),
            shiny::htmlOutput(ns("structure_type_table")),
            shiny::h3("Average shelter cost for renters"),
            bigger_padded(shiny::textOutput(ns("shelter_cost"))),
            bigger_padded(shiny::textOutput(ns("shelter_cost_city"))),
            shiny::textOutput(ns("average_renter_shelter_cost_description")),
            shiny::uiOutput(ns("average_renter_shelter_cost_plot_ui")),
            shiny::h3("Unaffordable housing"),
            bigger_padded(shiny::textOutput(ns("unaffordable_housing"))),
            bigger_padded(shiny::textOutput(ns("unaffordable_housing_city"))),
            shiny::textOutput(ns("unaffordable_housing_description")),
            shiny::uiOutput(ns("unaffordable_housing_plot_ui")),
            shiny::hr(),
            shiny::h3("Households by tenure"),
            shiny::textOutput(ns("household_tenure_description")),
            shiny::uiOutput(ns("household_tenure_plot_ui")),
            shiny::htmlOutput(ns("household_tenure_table")),
            shiny::h3("Number of bedrooms"),
            shiny::textOutput(ns("bedrooms_description")),
            shiny::uiOutput(ns("bedrooms_plot_ui")),
            shiny::htmlOutput(ns("bedrooms_table"))
          ),
          shiny::column(
            width = 4,
            shiny::h2("Sociodemographic characteristics"),
            shiny::h3("Population density"),
            bigger_padded(shiny::textOutput(ns("population_density_number"))),
            shiny::textOutput(ns("population_density_description")),
            shiny::uiOutput(ns("population_density_plot_ui")),
            shiny::hr(),
            shiny::h3("Population change"),
            bigger_padded(shiny::textOutput(ns("population_change_number"))),
            shiny::textOutput(ns("population_change_description")),
            shiny::uiOutput(ns("population_change_plot_ui")),
            shiny::hr(),
            shiny::h3("Average total household income"),
            shiny::textOutput(ns("average_total_household_income_description")),
            shiny::uiOutput(ns("average_total_household_income_plot_ui")),
            shiny::htmlOutput(ns("average_total_household_income_table")),
            shiny::hr(),
            shiny::h3("Low-income measure after tax"),
            bigger_padded(shiny::textOutput(ns("lim_at"))),
            bigger_padded(shiny::textOutput(ns("lim_at_city"))),
            shiny::textOutput(ns("lim_at_description")),
            shiny::uiOutput(ns("lim_at_plot_ui")),
            shiny::hr(),
            shiny::h3("Household size"),
            shiny::textOutput(ns("household_size_description")),
            shiny::uiOutput(ns("household_size_plot_ui")),
            shiny::htmlOutput(ns("household_size_table")),
            shiny::hr(),
            shiny::h3("Visible minority population"),
            bigger_padded(shiny::textOutput(ns("visible_minority"))),
            bigger_padded(shiny::textOutput(ns("visible_minority_city"))),
            shiny::textOutput(ns("visible_minority_description")),
            shiny::uiOutput(ns("visible_minority_plot_ui")),
            shiny::htmlOutput(ns("visible_minority_table"))
          )
        ),
        easyClose = TRUE,
        footer = NULL, size = "l"
      ))

      compare <- shiny::reactive({
        level() == "neighbourhood"
      })

      output$header <- shiny::renderText({
        switch(level(),
          city = "Toronto",
          neighbourhood = neighbourhood()
        )
      })

      ## Summary statistics -----

      output$households <- shiny::renderText({
        glue::glue('Total households: <span style = "float: right;">{scales::comma(dataset()[["households"]])}</span>')
      })

      output$renters <- shiny::renderText({
        glue::glue('Proportion renters: <span style = "float: right;">{scales::percent(renters, accuracy = 0.1)}</span>', renters = dataset()[["household_tenure"]] %>%
          dplyr::filter(group == "Renter") %>%
          dplyr::pull(prop))
      })

      output$core_housing_need <- shiny::renderText({
        glue::glue('In core housing need: <span style = "float: right;"><i>Coming soon!</i></span>')
      })

      output$population <- shiny::renderText({
        glue::glue('Total population: <span style = "float: right;">{scales::comma(dataset()[["population"]])}</span>')
      })

      output$lem_table <- shiny::renderText({
        dataset()[["lem"]] %>%
          kableExtra::kable() %>%
          kableExtra::kable_styling(bootstrap_options = "condensed", full_width = FALSE, position = "left") %>%
          kableExtra::column_spec(1, width = "30%") %>%
          kableExtra::column_spec(2:4, width = "20%")
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

      output$number_of_apartments_plot <- plotly::renderPlotly({
        number_of_apartments_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$number_of_apartments_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = number_of_apartments_alt_text(),
          plotly::plotlyOutput(ns("number_of_apartments_plot"), height = "100px")
        )
      })

      output$number_of_units_description <- shiny::renderText({
        number_of_units_description(level(), neighbourhood(), number_of_units(), number_of_units_formatted())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      number_of_units_alt_text <- shiny::reactive({
        number_of_units_plot_alt_text(level(), neighbourhood())
      })

      output$number_of_units_plot <- plotly::renderPlotly({
        number_of_units_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$number_of_units_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = number_of_units_alt_text(),
          plotly::plotlyOutput(ns("number_of_units_plot"), height = "100px")
        )
      })

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

      output$apartment_building_evaluation_plot <- plotly::renderPlotly({
        apartment_building_evaluation_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$apartment_building_evaluation_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = apartment_building_evaluation_alt_text(),
          plotly::plotlyOutput(ns("apartment_building_evaluation_plot"), height = "100px")
        )
      })

      # Amenity density ------

      output$amenity_density_description <- shiny::renderText({
        amenity_density_description(level(), neighbourhood())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      amenity_density_alt_text <- shiny::reactive({
        amenity_density_plot_alt_text(level(), neighbourhood())
      })

      output$amenity_density_plot <- plotly::renderPlotly({
        amenity_density_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$amenity_density_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = amenity_density_alt_text(),
          plotly::plotlyOutput(ns("amenity_density_plot"), height = "150px")
        )
      })

      output$amenity_density_table <- shiny::renderText({
        generate_table(dataset(), "amenity_density", compare(), "Amenity density", "Percent") %>%
          kableExtra::footnote(general = "A very small number of areas have unknown amenity density, so values may not add up to 100%.")
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      # Housing ----

      output$structure_type_description <- shiny::renderText({
        structure_type_description(level(), neighbourhood())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      structure_type_alt_text <- shiny::reactive({
        structure_type_plot_alt_text(level(), neighbourhood())
      })

      output$structure_type_plot <- plotly::renderPlotly({
        structure_type_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$structure_type_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = structure_type_alt_text(),
          plotly::plotlyOutput(ns("structure_type_plot"), height = "200px")
        )
      })

      output$structure_type_table <- shiny::renderText({
        generate_table(dataset(), "structure_type", compare(), "Housing Structure Type", "Percent")
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      # Bedrooms -----

      output$bedrooms_description <- shiny::renderText({
        bedrooms_description(level(), neighbourhood())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      bedrooms_alt_text <- shiny::reactive({
        bedrooms_plot_alt_text(level(), neighbourhood())
      })

      output$bedrooms_plot <- plotly::renderPlotly({
        bedrooms_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$bedrooms_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = bedrooms_alt_text(),
          plotly::plotlyOutput(ns("bedrooms_plot"), height = "200px")
        )
      })

      output$bedrooms_table <- shiny::renderText({
        generate_table(dataset(), "bedrooms", compare(), "Number of bedrooms", "Percent")
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      # Household tenure ----

      output$household_tenure_description <- shiny::renderText({
        household_tenure_description(level(), neighbourhood())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      household_tenure_alt_text <- shiny::reactive({
        household_tenure_plot_alt_text(level(), neighbourhood())
      })

      output$household_tenure_plot <- plotly::renderPlotly({
        household_tenure_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$household_tenure_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = household_tenure_alt_text(),
          plotly::plotlyOutput(ns("household_tenure_plot"), height = "100px")
        )
      })

      output$household_tenure_table <- shiny::renderText({
        generate_table(dataset(), "household_tenure", compare(), "Household tenure", "Percent")
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      # Shelter cost ----

      shelter_cost <- shiny::reactive({
        get_measure(dataset(), "average_renter_shelter_cost")
      })

      shelter_cost_formatted <- shiny::reactive({
        format_measure(shelter_cost(), "average_renter_shelter_cost")
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$shelter_cost <- shiny::renderText({
        shelter_cost_number(shelter_cost_formatted())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$shelter_cost_city <- shiny::renderText({
        shelter_cost_city(level())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$average_renter_shelter_cost_description <- shiny::renderText({
        average_renter_shelter_cost_description(level(), neighbourhood(), shelter_cost(), shelter_cost_formatted())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      average_renter_shelter_cost_alt_text <- shiny::reactive({
        average_renter_shelter_cost_plot_alt_text(level(), neighbourhood())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$average_renter_shelter_cost_plot <- plotly::renderPlotly({
        average_renter_shelter_cost_plot(dataset(), compare())
      }) %>%
        shiny::bindCache(level(), neighbourhood())

      output$average_renter_shelter_cost_plot_ui <- shiny::renderUI({
        shiny::div(
          role = "img",
          `aria-label` = average_renter_shelter_cost_alt_text(),
          plotly::plotlyOutput(ns("average_renter_shelter_cost_plot"), height = "100px")
        )
      })

      # Sociodemographic ----

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
  })
}

## To be copied in the UI
# mod_full_summary_modal_ui("full_summary")

## To be copied in the server
# mod_full_summary_modal_server("full_summary")
