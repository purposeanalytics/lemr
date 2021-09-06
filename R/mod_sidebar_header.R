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
      width = 12,
      shiny::h1(shiny::textOutput(ns("header"))),
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
    shiny::h2("Estimated annual availability of low-end of market rental")
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

    output$header <- shiny::renderText({
      switch(level(),
        city = "Toronto",
        neighbourhood = neighbourhood()
      )
    })

    dataset <- shiny::reactive({
      switch(level(),
        city = lemur::city_profile,
        neighbourhood = lemur::neighbourhood_profiles[[neighbourhood()]]
      )
    })

    output$households <- shiny::renderText({
      glue::glue('Total households: <span style = "float: right;">{scales::comma(dataset()[["households"]])}</span>')
    })

    output$renters <- shiny::renderText({
      glue::glue('Proportion renters: <span style = "float: right;">{scales::percent(renters, accuracy = 0.1)}</span>', renters = dataset()[["household_tenure"]] %>%
        dplyr::filter(group == "Renter") %>%
        dplyr::pull(prop))
    })

    output$core_housing_need <- shiny::renderText({
      glue::glue('In core housing need: <span style = "float: right;"></span>')
    })

    output$population <- shiny::renderText({
      glue::glue('Total population: <span style = "float: right;">{scales::comma(dataset()[["population"]])}</span>')
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

    download_filename <- shiny::reactive({
      if (is.null(neighbourhood())) {
        file_start <- "City of Toronto"
      } else {
        file_start <- neighbourhood()
      }

      glue::glue("{file_start} 2016 Census Profile")
    })

    output$download_html <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.html")
      },
      content = function(file) {
        shinybusy::show_modal_spinner(color = accent_colour, text = "Generating report...")
        on.exit(shinybusy::remove_modal_spinner())

        generate_report(level(), neighbourhood(), format = "html", filename = file)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.pdf")
      },
      content = function(file) {
        name <- switch(level(),
          city = "Toronto",
          neighbourhood = neighbourhood()
        )
        file.copy(system.file(glue::glue("templates/pdf/{name}.pdf"), package = "lemur"), file)
      }
    )
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
