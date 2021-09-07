#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_header_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 8,
      shiny::h1(shiny::textOutput(ns("header")))
    ),
    shiny::column(
      width = 4, align = "right",
      mod_full_summary_modal_ui(ns("full_summary"))
    ),
    shiny::column(
      width = 12,
      shiny::h2("Summary statistics")
    ),
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
      shiny::h2("Estimated rental supply"),
      shiny::tags$i("Coming soon!"),
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
        city = lemur::city_profile,
        neighbourhood = lemur::neighbourhood_profiles[[neighbourhood()]]
      )
    })

    mod_full_summary_modal_server("full_summary", header)

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
