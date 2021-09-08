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
      collapsible = TRUE,
      selected = "Home",
      cicerone::use_cicerone(),
      shiny::tabPanel(
        "Home"
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
            style = "margin-left: 15px; padding-right: 30px;",
            mod_search_ui("search"),
            shiny::hr(),
            mod_aggregate_layer_ui("aggregate"),
            shiny::h2("Select point(s) layers"),
            mod_point_layer_ui("apartment_buildings"),
            mod_point_layer_ui("apartment_evaluation"),
            # mod_point_layer_ui("evictions_hearings"),
            mod_point_layer_ui("agi"),
            mod_point_layer_ui("tdf"),
            shiny::hr(),
            mod_sidebar_header_ui("header")
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
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Low-end of Market Rental Monitor"
    ),
    cicerone::use_cicerone()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
