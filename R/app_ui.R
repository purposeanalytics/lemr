#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::tags$script(shiny::HTML(
      'function link(page) {
      Shiny.onInputChange("page_link", page)
      }'
    )),
    shiny::navbarPage(
      shiny::img(class = "navbar-img", src = fs::path("www", "lemr-logo", ext = "png"), title = "Low-end of market rental monitor"),
      collapsible = TRUE,
      id = "page",
      selected = "Home",
      shiny::tabPanel(
        "Home",
        mod_home_ui("home")
      ),
      shiny::tabPanel(
        "Map",
        mod_page_map_ui("map")
      ),
      shiny::tabPanel(
        "Data & Definitions",
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
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Low-end of Market Rental Monitor"
    ),
    cicerone::use_cicerone()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
