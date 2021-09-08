#' data_and_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_and_definitions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "max-width: 1000px; margin-left: auto; margin-right: auto",
    shiny::h1("Data and Definitions"),
    collapse_definitions(
      title = "Low-end of market",
      content = NULL
    ),
    collapse_definitions(
      title = "Amenity Density",
      content = NULL
    ),
    collapse_definitions(
      title = "Apartment Buildings",
      content = NULL
    ),
    collapse_definitions(
      title = "RentSafeTO Evaluation Scores",
      content = NULL
    ),
    collapse_definitions(
      title = "Eviction Hearings",
      content = NULL
    ),
    collapse_definitions(
      title = "Above Guideline Increase applications",
      content = NULL
    ),
    collapse_definitions(
      title = "Tenant Defense Fund grants",
      content = NULL
    ),
    collapse_definitions(
      title = "Neighbourhood Profiles",
      content = shiny::includeMarkdown(system.file("app", "data_and_definitions", "neighbourhood_profiles.md", package = "lemur"))
    )
  )
}

#' data_and_definitions Server Functions
#'
#' @noRd
mod_data_and_definitions_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

collapse_definitions <- function(title, content) {
  id <- janitor::make_clean_names(title)

  shiny::tagList(
    shiny::h2(shiny::tags$b(title), shiny::icon("chevron-down")) %>%
      bsplus::bs_attach_collapse(id),
    bsplus::bs_collapse(
      id = id,
      content = shiny::tagList(
        content
      )
    ),
    shiny::hr()
  )
}
