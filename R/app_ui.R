#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::navbarPage(
      shiny::img(class = "navbar-img", src = fs::path("www", "lemr-logo", ext = "png"), title = "Low-end of market rental monitor"),
      selected = "Map",
      shiny::tabPanel(
        "About"
      ),
      shiny::tabPanel(
        "Analysis"
      ),
      shiny::tabPanel(
        "Map",
        shiny::div(
          class = "map-col",
          mod_map_ui("map")
        ),
        shiny::div(
          class = "sidebar-col",
          shiny::wellPanel(
            id = "sidebar",
            style = "overflow: auto;",
            mod_search_ui("search"),
            mod_layers_ui("layers"),
            mod_sidebar_ui("sidebar")
          )
        )
      ),
      shiny::tabPanel(
        "Data and Definitions",
        mod_data_and_definitions_ui("data_and_definitions")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www", app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Low-end of Market Rental Monitor"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
