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
    shiny::h1("Data Downloads"),
    shinybusy::use_busy_spinner(spin = "fading-circle"),
    shiny::fluidRow(
      shiny::column(width = 12,
        shiny::p("The data used in the Low-end of Market Rental Monitor is available in two datasets and a set of reports.")
      ),
      shiny::column(
        width = 8,
        shiny::h2("LEMR Aggregate Layers"),
        shiny::p("This dataset includes the data used in each of the aggregate layers in this application, as well as all values shown in the full summary view, for each neighbourhood and the City of Toronto.")
      ),
      shiny::column(
        width = 4,
        align = "right",
        shiny::downloadButton(ns("aggregate_layers"), label = shiny::HTML("<b>Download</b> (.csv)"), icon = shiny::icon("download"))
      ),
      shiny::column(
        width = 8,
        shiny::h2("LEMR Point Layers"),
        shiny::p("This dataset includes the data used in each of the point layers in this application. It contains addresses and spatial coordinates along with the layer values.")
      ),
      shiny::column(
        width = 4,
        align = "right",
        shiny::downloadButton(ns("point_layers"), label = shiny::HTML("<b>Download</b> (.csv)"), icon = shiny::icon("download"))
      ),
      shiny::column(
        width = 8,
        shiny::h2("LEMR Reports"),
        shiny::p("These files include the Full Summary reports for each neighbourhood and the City of Toronto with charts and tables summarising the aggregate and point layers.")
      ),
      shiny::column(
        width = 4,
        align = "right",
        shiny::downloadButton(ns("reports"), label = shiny::HTML("<b>Download</b> (.zip)"), icon = shiny::icon("download"))
      )
    ),
    shiny::div(class = "divider-line"),
    shiny::h1("Definitions"),
    shiny::tagList(
      lemr::data_and_definitions %>%
        dplyr::mutate(definition_full = purrr::pmap(list(.data$name, .data$definition, .data$dataset, .data$dataset_link, .data$data_owner, .data$years, .data$published), format_definition)) %>%
        dplyr::pull(.data$definition_full)
    ),
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
    )
  )
}

#' data_and_definitions Server Functions
#'
#' @noRd
mod_data_and_definitions_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$aggregate_layers <- shiny::downloadHandler(
      filename = function() {
        "Aggregate Layers.csv"
      },
      content = function(file) {
        file.copy(app_sys("extdata/aggregate_data.csv"), file)
      }
    )

    output$point_layers <- shiny::downloadHandler(
      filename = function() {
        "Point Layers.csv"
      },
      content = function(file) {
        file.copy(app_sys("extdata/points_data.csv"), file)
      }
    )

    output$reports <- shiny::downloadHandler(
      filename = function() {
        "LEMR Reports.zip"
      },
      content = function(file) {
        shinybusy::show_spinner()
        on.exit(shinybusy::hide_spinner())
        utils::zip(file, c(fs::dir_ls(app_sys("reports/pdf")), fs::dir_ls(app_sys("reports/html"))), extras = "-j")
      },
      contentType = "application/zip"
    )
  })
}

format_definition <- function(name, definition, dataset, dataset_link, data_owner, years, published) {

    definition_only <- is.na(data_owner)
    dataset_is_link <- !is.na(dataset_link)

    if (definition_only) {

      details <- NULL

    } else {

      if (dataset_is_link) {
        dataset <- glue::glue("<a href = '{dataset_link}' target = '_blank'>{dataset}</a>")
      }

      dataset <- dataset %>%
        stringr::str_squish() %>%
        shiny::HTML()

      details <- shiny::p(shiny::tags$b("Related Datset:"), dataset,
                          shiny::tags$br(),
                          shiny::tags$b("Data Owner:"), shiny::HTML(data_owner),
                          shiny::tags$br(),
                          shiny::tags$b("Years Included:"), shiny::HTML(years),
                          shiny::tags$br(),
                          shiny::tags$b("Publication Year:"), shiny::HTML(published))

    }



  data_details <-
    definition <- shiny::tagList(
      bigger_padded(shiny::tags$b(name)),
      shiny::p(shiny::tags$b("Definition:"), shiny::HTML(definition)),
      details,
      shiny::hr()
    )

  definition
}
