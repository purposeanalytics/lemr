#' report_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_download_ui <- function(id) {
  ns <- NS(id)

  shinyWidgets::dropdownButton(
    label = "Download",
    circle = FALSE,
    inline = TRUE,
    shiny::downloadButton(ns("download_pdf"), "PDF",
      icon = NULL,
      width = "100%", style = "margin-bottom: 0.5em;"
    ),
    shiny::downloadButton(ns("download_html"), "HTML",
      icon = NULL,
      width = "100%"
    )
  )
}

#' report_download Server Functions
#'
#' @noRd
mod_report_download_server <- function(id, neighbourhood) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    download_filename <- shiny::reactive({
      if (is.null(neighbourhood())) {
        file <- "Toronto"
      } else {
        file <- neighbourhood()
      }

      fs::path_sanitize(file)
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.pdf")
      },
      content = function(file) {
        file.copy(app_sys(glue::glue("reports/pdf/{download_filename()}.pdf")), file)
      }
    )

    output$download_html <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.html")
      },
      content = function(file) {
        file.copy(app_sys(glue::glue("reports/html/{download_filename()}.html")), file)
      }
    )
  })
}

## To be copied in the UI
# mod_report_download_ui("report_download_ui_1")

## To be copied in the server
# mod_report_download_server("report_download_ui_1")
