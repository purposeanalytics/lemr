download_report <- function(id, output_types = c("PDF", "HTML")) {
  ns <- shiny::NS(id)

  button_dropdown <- purrr::map(output_types, ~ shiny::tags$li(
    shiny::downloadLink(ns(.x), label = .x)
  )) %>%
    shiny::tagList()

  shiny::span(
    class = "btn-group",
    shinyBS::bsButton(
      inputId = ns("download_report"),
      label = "Download report",
      icon = shiny::icon("file-download", lib = "font-awesome"),
      type = "action",
      `data-toggle` = "dropdown",
      `aria-expanded` = "false"
    ),
    shiny::tags$ul(
      class = "dropdown-menu",
      id = ns("dropdown"),
      button_dropdown
    )
  )
}
