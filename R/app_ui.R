#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::navbarPage(
      "LEMUR",
      shiny::tabPanel(
        "Map",
        shiny::column(
          width = 9,
          mod_map_ui("map")
        ),
        shiny::column(
          width = 3,
          shiny::wellPanel(
            mod_search_ui("search")
          ),
          shiny::wellPanel(
            mod_layers_ui("layers")
          ),
          shiny::wellPanel(
            style = "height: calc(100vh - 325px); overflow: auto;",
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
      app_title = "lemur"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
