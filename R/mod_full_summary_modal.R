#' full_summary_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_full_summary_modal_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::showModal(
    shiny::div(
      class = "full-summary-modal",
      shiny::modalDialog(
        style = "margin-top: 2em; margin-left: 3em; margin-right: 3em;",
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::modalButton("Close"),
            shiny::h1(shiny::textOutput(ns("header")))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::hr(),
            shiny::div(
              class = "modal-col",
              mod_legend_ui(ns("legend")),
              shiny::h2("Summary statistics"),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  class = "modal-summary-statistics",
                  shiny::htmlOutput(ns("summary_statistics"))
                ),
                shiny::column(
                  width = 12,
                  shiny::hr(),
                  shiny::h2("Estimated rental supply"),
                  shiny::uiOutput(ns("rental_supply_plot_ui"))
                ),
                shiny::column(
                  width = 6,
                  class = "modal-summary-statistics",
                  shiny::uiOutput(ns("rental_supply_primary_table"))
                ),
                shiny::column(
                  width = 6,
                  class = "modal-summary-statistics",
                  shiny::uiOutput(ns("rental_supply_secondary_table")),
                  shiny::uiOutput(ns("rental_supply_non_market_table"))
                ),
                shiny::column(
                  width = 12,
                  shiny::hr(),
                  shiny::h3("Estimated annual availability of low-end of market rental"),
                  shiny::htmlOutput(ns("lem_table")),
                  shiny::hr(),
                  # Units ---
                  shiny::h3(shiny::textOutput(ns("number_of_units_number"))),
                  padded(shiny::textOutput(ns("number_of_units_breakdown"))),
                  shiny::textOutput(ns("number_of_units_description")),
                  shiny::uiOutput(ns("number_of_units_plot_ui")),
                  shiny::hr(),
                  # RentSafTO ----
                  shiny::h3(shiny::textOutput(ns("apartment_building_evaluation_number"))),
                  padded(shiny::textOutput(ns("apartment_building_evaluation_none"))),
                  shiny::textOutput(ns("apartment_building_evaluation_description")),
                  shiny::uiOutput(ns("apartment_building_evaluation_plot_ui")),
                  shiny::hr(),
                  shiny::h3("Rooming houses"),
                  shiny::htmlOutput(ns("rooming_houses_table")),
                  shiny::hr(),
                  shiny::h3("Above Guideline Increase applications and Tenant Defence Fund grants"),
                  shiny::htmlOutput(ns("agi_tdf_apartments_description")),
                  shiny::htmlOutput(ns("agi_tdf_apartments_table")),
                  shiny::textOutput(ns("agi_non_apartments")),
                  shiny::hr(),
                  # Core housing need ----
                  shiny::h3(shiny::textOutput(ns("core_housing_need_number"))),
                  shiny::textOutput(ns("core_housing_need_description")),
                  shiny::uiOutput(ns("core_housing_need_plot_ui")),
                  shiny::hr(),
                  # Evictions ----
                  shiny::h3(shiny::textOutput(ns("evictions_number"))),
                  shiny::textOutput(ns("evictions_description")),
                  shiny::uiOutput(ns("evictions_plot_ui")),
                  shiny::hr(),
                  # Vacancy rate ---
                  shiny::h3(shiny::textOutput(ns("vacancy_rate_number"))),
                  shiny::textOutput(ns("vacancy_rate_description")),
                  shiny::uiOutput(ns("vacancy_rate_plot_ui")),
                  shiny::hr(),
                  shiny::h3("Proximity to services"),
                  shiny::textOutput(ns("amenity_density_description")),
                  shiny::uiOutput(ns("amenity_density_plot_ui")),
                  shiny::htmlOutput(ns("amenity_density_table")),
                  shiny::hr()
                )
              )
            ),
            shiny::div(
              class = "modal-col middle",
              shiny::h2("Housing characteristics"),
              # Apartment buildings ----
              shiny::h3(shiny::textOutput(ns("number_of_apartments_number"))),
              padded(shiny::textOutput(ns("number_of_apartments_breakdown"))),
              shiny::textOutput(ns("number_of_apartments_description")),
              shiny::uiOutput(ns("number_of_apartments_plot_ui")),
              shiny::hr(),
              shiny::h3("Housing structure type"),
              shiny::textOutput(ns("structure_type_description")),
              shiny::uiOutput(ns("structure_type_plot_ui")),
              shiny::htmlOutput(ns("structure_type_table")),
              shiny::hr(),
              # Average shelter cost for renters ----
              shiny::h3(shiny::textOutput(ns("shelter_cost"))),
              shiny::textOutput(ns("average_renter_shelter_cost_description")),
              shiny::uiOutput(ns("average_renter_shelter_cost_plot_ui")),
              shiny::hr(),
              # Unaffordable housing ----
              shiny::h3(shiny::textOutput(ns("unaffordable_housing"))),
              shiny::textOutput(ns("unaffordable_housing_description")),
              shiny::uiOutput(ns("unaffordable_housing_plot_ui")),
              shiny::hr(),
              shiny::h3("Households by tenure"),
              shiny::textOutput(ns("household_tenure_description")),
              shiny::uiOutput(ns("household_tenure_plot_ui")),
              shiny::htmlOutput(ns("household_tenure_table")),
              shiny::hr(),
              shiny::h3("Number of bedrooms"),
              shiny::textOutput(ns("bedrooms_description")),
              shiny::uiOutput(ns("bedrooms_plot_ui")),
              shiny::htmlOutput(ns("bedrooms_table")),
              shiny::hr()
            ),
            shiny::div(
              class = "modal-col right",
              shiny::h2("Sociodemographic characteristics"),
              # Population density ----
              shiny::h3(shiny::textOutput(ns("population_density_number"))),
              shiny::textOutput(ns("population_density_description")),
              shiny::uiOutput(ns("population_density_plot_ui")),
              shiny::hr(),
              shiny::h3(shiny::textOutput(ns("population_change_number"))),
              shiny::textOutput(ns("population_change_description")),
              shiny::uiOutput(ns("population_change_plot_ui")),
              shiny::hr(),
              shiny::h3("Average total household income"),
              shiny::textOutput(ns("average_total_household_income_description")),
              shiny::uiOutput(ns("average_total_household_income_plot_ui")),
              shiny::htmlOutput(ns("average_total_household_income_table")),
              shiny::hr(),
              shiny::h3(shiny::textOutput(ns("lim_at"))),
              shiny::textOutput(ns("lim_at_description")),
              shiny::uiOutput(ns("lim_at_plot_ui")),
              shiny::hr(),
              shiny::h3("Household size"),
              shiny::textOutput(ns("household_size_description")),
              shiny::uiOutput(ns("household_size_plot_ui")),
              shiny::htmlOutput(ns("household_size_table")),
              shiny::hr(),
              shiny::h3(shiny::textOutput(ns("visible_minority"))),
              shiny::textOutput(ns("visible_minority_description")),
              shiny::uiOutput(ns("visible_minority_plot_ui")),
              shiny::htmlOutput(ns("visible_minority_table"))
            )
          )
        )
      )
    )
  )
}

#' full_summary_modal Server Functions
#'
#' @noRd
mod_full_summary_modal_server <- function(id, level, neighbourhood, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    compare <- shiny::reactive({
      level() == "neighbourhood"
    })

    output$header <- shiny::renderText({
      switch(level(),
        city = "Toronto",
        neighbourhood = neighbourhood()
      )
    })


    mod_legend_server("legend", level, neighbourhood)

    ## Summary statistics -----

    output$summary_statistics <- shiny::renderText({
      summary_statistics_table(dataset())
    })

    output$rental_supply_plot <- plotly::renderPlotly({
      rental_supply_plot(dataset())
    })

    output$rental_supply_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = rental_supply_plot_alt_text(level(), neighbourhood()),
        plotly::plotlyOutput(ns("rental_supply_plot"), height = "50px")
      )
    })

    output$rental_supply_primary_table <- shiny::renderText({
      rental_supply_primary_table(dataset())
    })

    output$rental_supply_secondary_table <- shiny::renderText({
      rental_supply_secondary_table(dataset())
    })

    output$rental_supply_non_market_table <- shiny::renderText({
      rental_supply_non_market_table(dataset())
    })

    output$lem_table <- shiny::renderText({
      dataset()[["lem"]] %>%
        dplyr::mutate(dplyr::across(-Bedrooms, scales::comma)) %>%
        kableExtra::kable(align = "lrrr") %>%
        kableExtra::kable_styling(bootstrap_options = "condensed", full_width = FALSE, position = "left") %>%
        kableExtra::column_spec(1, width = "30%") %>%
        kableExtra::column_spec(2:4, width = "20%")
    })

    # AGIs and TDFs -----

    output$agi_tdf_apartments_description <- shiny::renderText({
      agi_tdf_description(level(), neighbourhood())
    })

    output$agi_tdf_apartments_table <- shiny::renderText({
      display_agi_tdf_buildings(dataset(), compare = compare())
    })

    output$agi_non_apartments <- shiny::renderText({
      agi_non_apartments(dataset(), level(), neighbourhood())
    })

    # Core housing need -----

    core_housing_need <- shiny::reactive({
      get_measure(dataset(), "core_housing_need")
    })

    core_housing_need_formatted <- shiny::reactive({
      format_measure(core_housing_need(), "core_housing_need")
    })

    output$core_housing_need_number <- shiny::renderText({
      core_housing_need_number(core_housing_need_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$core_housing_need_description <- shiny::renderText({
      core_housing_need_description(level(), neighbourhood(), core_housing_need(), core_housing_need_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    core_housing_need_alt_text <- shiny::reactive({
      core_housing_need_plot_alt_text(level(), neighbourhood())
    })

    output$core_housing_need_plot <- plotly::renderPlotly({
      core_housing_need_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$core_housing_need_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = core_housing_need_alt_text(),
        plotly::plotlyOutput(ns("core_housing_need_plot"), height = "100px")
      )
    })

    # Evictions -----

    evictions <- shiny::reactive({
      get_measure(dataset(), "evictions")
    })

    evictions_formatted <- shiny::reactive({
      format_measure(evictions(), "evictions")
    })

    output$evictions_number <- shiny::renderText({
      evictions_number(evictions_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$evictions_description <- shiny::renderText({
      evictions_description(level(), neighbourhood(), evictions(), evictions_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    evictions_alt_text <- shiny::reactive({
      evictions_plot_alt_text(level(), neighbourhood())
    })

    output$evictions_plot <- plotly::renderPlotly({
      evictions_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$evictions_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = evictions_alt_text(),
        plotly::plotlyOutput(ns("evictions_plot"), height = "100px")
      )
    })

    # Vacancy rate ----

    vacancy_rate <- shiny::reactive({
      get_measure(dataset(), "vacancy_rate_2020")
    })

    vacancy_rate_formatted <- shiny::reactive({
      format_measure(vacancy_rate(), "vacancy_rate")
    })

    output$vacancy_rate_number <- shiny::renderText({
      vacancy_rate_number(vacancy_rate_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$vacancy_rate_description <- shiny::renderText({
      vacancy_rate_description(level(), neighbourhood(), vacancy_rate(), vacancy_rate_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    vacancy_rate_alt_text <- shiny::reactive({
      vacancy_rate_plot_alt_text(level(), neighbourhood())
    })

    output$vacancy_rate_plot <- plotly::renderPlotly({
      vacancy_rate_plot(dataset(), compare())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$vacancy_rate_plot_ui <- shiny::renderUI({
      shiny::div(
        role = "img",
        `aria-label` = vacancy_rate_alt_text(),
        plotly::plotlyOutput(ns("vacancy_rate_plot"), height = "100px")
      )
    })

    # Rooming houses -----

    output$rooming_houses_table <- shiny::renderText({
      display_rooming_houses(dataset(), compare = compare())
    })

    # Number of apartments -----

    number_of_apartments <- shiny::reactive({
      get_measure(dataset(), "number_of_buildings")
    })

    number_of_apartments_formatted <- shiny::reactive({
      format_measure(number_of_apartments(), "number_of_buildings")
    })


    output$number_of_apartments_number <- shiny::renderText({
      number_of_apartments_number(number_of_apartments_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$number_of_apartments_breakdown <- shiny::renderText({
      number_of_apartments_breakdown(dataset())
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

    # Number of apartment units ----

    number_of_units <- shiny::reactive({
      get_measure(dataset(), "number_of_units")
    })

    number_of_units_formatted <- shiny::reactive({
      format_measure(number_of_units(), "number_of_units")
    })

    output$number_of_units_number <- shiny::renderText({
      number_of_units_number(number_of_units_formatted())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$number_of_units_breakdown <- shiny::renderText({
      number_of_units_breakdown(dataset())
    }) %>%
      shiny::bindCache(level(), neighbourhood())

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

    output$apartment_building_evaluation_none <- shiny::renderText({
      apartment_building_evaluation_none(apartment_building_evaluation_formatted())
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
      generate_table(dataset(), "amenity_density", compare(), "Proximity to services", "Percent") %>%
        kableExtra::footnote(general = "A very small number of areas have unknown proximity to services, so values may not add up to 100%.")
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
      shelter_cost_number(shelter_cost_formatted(), level())
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
        plotly::plotlyOutput(ns("average_total_household_income_plot"), height = "125px")
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
      unaffordable_housing_number(unaffordable_housing_formatted(), level())
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
      lim_at_number(lim_at_formatted(), level())
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
      visible_minority_number(dataset(), level())
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
# mod_full_summary_modal_ui("full_summary")

## To be copied in the server
# mod_full_summary_modal_server("full_summary")
