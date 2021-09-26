#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_header_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 8,
      shiny::h1(shiny::textOutput(ns("header")))
    ),
    shiny::column(
      width = 4, align = "right",
      shiny::actionButton(ns("modal"), label = "Full Summary")
    ),
    shiny::column(
      width = 12,
      shiny::h2("Summary statistics")
    ),
    shiny::column(
      width = 6,
      class = "summary-statistics",
      shiny::htmlOutput(ns("summary_statistics"))
    ),
    shiny::column(
      width = 12,
      shiny::h2("Estimated rental supply"),
      shiny::uiOutput(ns("rental_supply_plot_ui"))
    ),
    shiny::column(
      width = 6,
      class = "summary-statistics padded",
      shiny::uiOutput(ns("rental_supply_primary_table"))
    ),
    shiny::column(
      width = 6,
      class = "summary-statistics padded",
      shiny::uiOutput(ns("rental_supply_secondary_table")),
      shiny::uiOutput(ns("rental_supply_non_market_table"))
    ),
    shiny::column(
      width = 12,
      shiny::h2("Estimated annual availability of low-end of market rental"),
      shiny::htmlOutput(ns("lem_table"))
    )
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_sidebar_header_server <- function(id, address_and_neighbourhood, search_method) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Trigger modal
    shiny::observeEvent(input$modal, {
      mod_full_summary_modal_ui(ns("full_summary"))
    })

    mod_full_summary_modal_server("full_summary", level, neighbourhood, dataset)

    neighbourhood <- shiny::reactive({
      address_and_neighbourhood$neighbourhood
    })

    level <- shiny::reactive({
      if (is.null(neighbourhood())) {
        "city"
      } else {
        "neighbourhood"
      }
    })

    header <- shiny::reactive({
      switch(level(),
        city = "Toronto",
        neighbourhood = neighbourhood()
      )
    })

    output$header <- shiny::renderText({
      header()
    })

    dataset <- shiny::reactive({
      switch(level(),
        city = lemur::city_aggregate,
        neighbourhood = lemur::neighbourhood_aggregate[[neighbourhood()]]
      )
    })

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

    output$back_to_city <- shiny::renderUI({
      if (!is.null(address_and_neighbourhood$neighbourhood)) {
        shiny::actionLink(ns("back"), label = "Back to City of Toronto view", class = "padded")
      }
    })

    # Observe the back button to reset the inputs and map
    shiny::observeEvent(input$back, {
      address_and_neighbourhood$address <- NULL
      address_and_neighbourhood$neighbourhood <- NULL

      search_method("back")
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
