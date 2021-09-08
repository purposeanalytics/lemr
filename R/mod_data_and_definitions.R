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
    class = "content-page",
    shiny::h1("Data & Definitions"),
    collapse_definitions(
      title = "Low-end of market",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Amenity Density",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Apartment Buildings",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "RentSafeTO Evaluation Scores",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Eviction Hearings",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Above Guideline Increase applications",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Tenant Defense Fund grants",
      content = shiny::tags$i("Coming soon")
    ),
    collapse_definitions(
      title = "Neighbourhood Profiles",
      show = FALSE,
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

collapse_definitions <- function(title, content, show = TRUE) {
  id <- janitor::make_clean_names(title)

  shiny::tagList(
    shiny::h2(shiny::tags$b(title), shiny::icon("chevron-down")) %>%
      bsplus::bs_attach_collapse(id),
    bsplus::bs_collapse(
      id = id,
      content = shiny::tagList(
        content
      ),
      show = show
    ),
    shiny::hr()
  )
}
