#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shiny::tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}"),
    golem_add_external_resources(),
    navbarPage(
      "LEMUR",
      tabPanel(
        "Map",
        sidebarLayout(
          mainPanel(
            mod_map_ui("map")
          ),
          sidebarPanel(
            mod_address_search_ui("address")
          )
        ),
        tabPanel("Portal"),
        tabPanel("About")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lemur"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
