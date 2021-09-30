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

  shiny::tagList(
    shinyjs::disabled(shinyWidgets::dropdownButton(
      inputId = ns("download_dropdown"),
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
    )),
    shinybusy::use_busy_spinner(spin = "fading-circle")
  )
}

#' report_download Server Functions
#'
#' @noRd
mod_report_download_server <- function(id, level, neighbourhood) {
  shiny::moduleServer(id, function(input, output, session) {
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
        shiny::req(level())

        existing_file <- app_sys(glue::glue("reports/pdf/{download_filename()}.pdf"))

        # If file doesn't exist, render it!
        # Might happen if someone clones repo, if file isn't on server, etc
        # Better to just render than fail :)
        if (existing_file == "") {
          shinybusy::show_spinner()

          html_file <- generate_report(level(), neighbourhood(),
            format = "html",
            filename = glue::glue("{download_filename()}.html")
          )

          pagedown::chrome_print(
            html_file,
            output = file,
            extra_args = chrome_extra_args(),
            verbose = 1,
            async = TRUE # returns a promise
          )$finally(
            shinybusy::hide_spinner
          )
        } else {
          file.copy(existing_file, file)
        }
      }
    )

    output$download_html <- shiny::downloadHandler(
      filename = function() {
        glue::glue("{download_filename()}.html")
      },
      content = function(file) {
        Sys.sleep(2)

        existing_file <- app_sys(glue::glue("reports/html/{download_filename()}.html"))

        if (existing_file == "") {
          shinybusy::show_spinner()
          on.exit(shinybusy::hide_spinner())

          generate_report(level(), neighbourhood(), format = "html", filename = file)
        } else {
          file.copy(existing_file, file)
        }
      }
    )
  })
}

# Via: https://github.com/RLesur/chrome_print_shiny
#' Return Chrome CLI arguments
#'
#' This is a helper function which returns arguments to be passed to Chrome.
#' This function tests whether the code is running on shinyapps and returns the
#' appropriate Chrome extra arguments.
#'
#' @param default_args Arguments to be used in any circumstances.
#'
#' @return A character vector with CLI arguments to be passed to Chrome.
#' @noRd
chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- default_args
  # Test whether we are in a shinyapps container
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(
      args,
      "--no-sandbox", # required because we are in a container
      "--disable-dev-shm-usage"
    ) # in case of low available memory
  }
  args
}
