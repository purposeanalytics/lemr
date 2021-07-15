#' sidebar_places UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_places_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("people_sidebar"))
}

#' sidebar_places Server Functions
#'
#' @noRd
mod_sidebar_places_server <- function(id, neighbourhood) {
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
          shiny::h3("Housing structure type"),
          shiny::textOutput(ns("structure_type_description")),
          shiny::plotOutput(ns("structure_type_plot"), height = "200px"),
          shiny::htmlOutput(ns("structure_type_table")),
          shiny::h3("Number of bedrooms"),
          shiny::textOutput(ns("bedrooms_description")),
          shiny::plotOutput(ns("bedrooms_plot"), height = "200px"),
          shiny::htmlOutput(ns("bedrooms_table")),
          shiny::h3("Households by tenure"),
          shiny::textOutput(ns("household_tenure_description")),
          shiny::plotOutput(ns("household_tenure_plot"), height = "120px"),
          shiny::htmlOutput(ns("household_tenure_table")),
          shiny::h3("Average shelter cost for renters"),
          shiny::h4(shiny::uiOutput(ns("shelter_cost"))),
          shiny::h4(shiny::uiOutput(ns("shelter_cost_city"))),
          shiny::textOutput(ns("average_renter_shelter_cost_description")),
          shiny::plotOutput(ns("average_renter_shelter_cost_plot"), height = "100px")
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

    # Structure type -----

    output$structure_type_description <- shiny::renderText({
      generate_bar_chart_description(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "structure type")
    })

    structure_type_plot_alt_text <- shiny::reactive({
      generate_bar_chart_alt_text(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "housing structure type")
    })

    output$structure_type_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("structure_type", compare = compare())
      },
      res = 96,
      bg = "transparent",
      alt = structure_type_plot_alt_text
    )

    output$structure_type_table <- shiny::renderText({
      res <- dataset() %>%
        display_neighbourhood_profile("structure_type", compare = compare(), type = "table")

      if (!compare()) {
        names(res) <- c("Housing Structure Type", "Percent")
      } else {
        names(res)[[1]] <- "Housing Structure Type"
      }

      res %>%
        kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
        kableExtra::kable_styling()
    })

    # Bedrooms -----

    output$bedrooms_description <- shiny::renderText({
      generate_bar_chart_description(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "number of bedrooms")
    })

    bedrooms_plot_alt_text <- shiny::reactive({
      generate_bar_chart_alt_text(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "number of bedrooms")
    })

    output$bedrooms_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("bedrooms", compare = compare())
      },
      res = 96,
      bg = "transparent",
      alt = bedrooms_plot_alt_text
    )

    output$bedrooms_table <- shiny::renderText({
      res <- dataset() %>%
        display_neighbourhood_profile("bedrooms", compare = compare(), type = "table")

      if (!compare()) {
        names(res) <- c("Number of bedrooms", "Percent")
      } else {
        names(res)[[1]] <- "Number of bedrooms"
      }

      res %>%
        kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
        kableExtra::kable_styling()
    })

    # Household tenure ----

    output$household_tenure_description <- shiny::renderText({
      generate_bar_chart_description(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "household tenure (renter versus owner)")
    })

    household_tenure_plot_alt_text <- shiny::reactive({
      generate_bar_chart_alt_text(sidebar_level = sidebar_level(), neighbourhood = neighbourhood(), text = "household tenure (renter versus owner)")
    })

    output$household_tenure_plot <- shiny::renderPlot(
      {
        dataset() %>%
          display_neighbourhood_profile("household_tenure", compare = compare(), width = 10)
      },
      res = 96,
      bg = "transparent",
      alt = household_tenure_plot_alt_text
    )

    output$household_tenure_table <- shiny::renderText({
      res <- dataset() %>%
        display_neighbourhood_profile("household_tenure", compare = compare(), type = "table")

      if (!compare()) {
        names(res) <- c("Household tenure", "Percent")
      } else {
        names(res)[[1]] <- "Household tenure"
      }

      res %>%
        kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
        kableExtra::kable_styling()
    })

    # Shelter cost ----

    shelter_cost <- shiny::reactive({
      dataset()[["average_renter_shelter_cost"]]
    })

    shelter_cost_formatted <- shiny::reactive({
      scales::dollar(shelter_cost(), accuracy = 1)
    })

    output$shelter_cost <- shiny::renderUI({
      glue::glue("Average monthly rent: {shelter_cost_formatted()}")
    })

    output$shelter_cost_city <- shiny::renderUI({
      if (sidebar_level() == "neighbourhood") {
        glue::glue('(City of Toronto: {scales::dollar(lemur::city_profile[["average_renter_shelter_cost"]], accuracy = 1)})')
      } else {
        NULL
      }
    })

    output$average_renter_shelter_cost_description <- shiny::renderText({
      if (sidebar_level() == "neighbourhood") {
        value_distribution <- stats::ecdf(lemur::city_profile[["average_renter_shelter_cost_distribution"]][["value"]])
        value_percentile <- value_distribution(shelter_cost())
      }

      switch(sidebar_level(),
        "city" = "Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods.",
        "neighbourhood" = glue::glue("Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods. The value for {neighbourhood()}, {shelter_cost_formatted()} per month, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' average rent.")
      )
    })

    average_renter_shelter_cost_plot_alt_text <- shiny::reactive({
      values <- lemur::city_profile[["average_renter_shelter_cost_distribution"]][["value"]]

      alt_text <- glue::glue("Histogram showing the distribution of average renter shelter cost for each of Toronto's neighbourhoods. The values range from {scales::dollar(min, accuracy = 1)} to {scales::dollar(max, accuracy = 1)} with most values between {scales::dollar(skew_min, accuracy = 1)} and {scales::dollar(skew_max, accuracy = 1)}.",
        min = min(values),
        max = max(values),
        skew_min = stats::quantile(values, 0.1),
        skew_max = stats::quantile(values, 0.9)
      )

      if (sidebar_level() == "neighbourhood") {
        neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood()}'s average monthly rent is highlighted.")
        alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
      }

      alt_text
    })

    output$average_renter_shelter_cost_plot <- shiny::renderPlot(
      {
        dataset() %>%
          plot_neighbourhood_profile_distribution("average_renter_shelter_cost", compare = compare(), binwidth = 50) +
          ggplot2::scale_x_continuous(labels = scales::dollar)
      },
      res = 96,
      bg = "transparent",
      alt = average_renter_shelter_cost_plot_alt_text
    )
  })
}

## To be copied in the UI
# mod_sidebar_places_ui("places")

## To be copied in the server
# mod_sidebar_places_server("places")
