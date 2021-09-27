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
    id = "data-and-definitions-page",
    shiny::h1("Data & Definitions"),
    # I wish I could explain why this is necessary, but removing it makes the tooltips everywhere go away
    # I guess some different JS is getting attached. I don't know!
    shiny::div(
      style = "display: none;",
      shiny::tagList(
        shiny::icon("chevron-down") %>%
          bsplus::bs_attach_collapse("title"),
        bsplus::bs_collapse(
          id = "title",
          content = shiny::tagList(
            "test"
          ),
          show = TRUE
        )
      )
    ),
    shiny::tagList(
      lemur::data_and_definitions %>%
        dplyr::mutate(definition_full = purrr::pmap(list(.data$name, .data$description, .data$data_source_prefix, .data$data_source_suffix, .data$data_source, .data$data_source_link), format_definition)) %>%
        dplyr::pull(.data$definition_full)
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

format_definition <- function(name, description, data_source_prefix, data_source_suffix, data_source, data_source_link) {
  includes_data_source <- !is.na(data_source)

  if (includes_data_source) {
    data_source_is_link <- !is.na(data_source_link)

    if (data_source_is_link) {
      data_source <- glue::glue("<a href = '{data_source_link}' target = '_blank'>{data_source}</a>")
    }

    data_source_full <- glue::glue("{data_source_prefix} {data_source} {data_source_suffix}",
      data_source_prefix = dplyr::coalesce(data_source_prefix, ""),
      data_source_suffix = dplyr::coalesce(data_source_suffix, "")
    ) %>%
      stringr::str_squish() %>%
      shiny::HTML()

    data_source_full <- shiny::p(shiny::tags$i("Data Source:"), data_source_full)
  } else {
    data_source_full <- NULL
  }

  data_source <-
    definition <- shiny::tagList(
      bigger_padded(shiny::tags$b(name)),
      shiny::p(shiny::tags$i("Description:"), shiny::HTML(description)),
      data_source_full,
      shiny::hr()
    )

  definition
}
