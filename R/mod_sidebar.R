#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1(shiny::textOutput(ns("header"))),
    shiny::uiOutput(ns("population"), class = "padded"),
    shinyWidgets::dropdownButton(
      inputId = "download-button",
      circle = FALSE,
      label = "Download report",
      shiny::downloadButton(ns("download_pdf"), "PDF", style = "width: 100%"),
      shiny::downloadButton(ns("download_html"), "HTML", style = "width: 100%")
    ),
    shiny::uiOutput(ns("back_to_city"), class = "padded"),
    shiny::uiOutput(ns("tabs_people_places"))
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, address_and_neighbourhood, search_method) {
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

    output$population <- shiny::renderText({
      dataset <- switch(level(),
        city = lemur::city_profile,
        neighbourhood = lemur::neighbourhood_profiles[[neighbourhood()]]
      )
      glue::glue('Population: {scales::comma(dataset[["population"]])} ({scales::comma(dataset[["households"]])} households)')
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

    output$tabs_people_places <- shiny::renderUI({
      shiny::tabsetPanel(
        id = "sidebar_tab",
        shiny::tabPanel(title = "People", mod_sidebar_people_ui(ns("people"))),
        shiny::tabPanel(title = "Places", mod_sidebar_places_ui(ns("places")))
      )
    })

    mod_sidebar_people_server("people", neighbourhood)
    mod_sidebar_places_server("places", neighbourhood)

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

        id <- shiny::showNotification(
          "Generating report...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(shiny::removeNotification(id), add = TRUE)

        generate_report(level(), neighbourhood(), format = "html", filename = file)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.pdf")
      },
      content = function(file) {

        id <- shiny::showNotification(
          "Generating report...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(shiny::removeNotification(id), add = TRUE)

        generate_report(level(), neighbourhood(), format = "pdf", filename = file)
      }
    )
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar")

## To be copied in the server
# mod_sidebar_server("sidebar")
