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






    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::p("The data used in the Low-end of Market Rental Monitor is available in two datasets and in a set of reports."),
        shiny::downloadLink(ns("aggregate_layers"), label = shiny::tagList(shiny::h2("Low-end of Market Rental Monitor, Aggregate Layers", shiny::icon("download")))),
          shiny::p("This dataset includes the data used in each of the aggregate layers in this tool, as well as all values shown in the full summary view, for each neighbourhood and the City of Toronto."),

        shiny::downloadLink(ns("aggregate_layers"), label = shiny::tagList(shiny::h2("Low-end of Market Rental Monitor, Point Layers", shiny::icon("download")))),
          shiny::p("This dataset includes the data used in each of the point layers in the tool. It contains all addresses and spatial information along with the layer values."),

        shiny::downloadLink(ns("aggregate_layers"), label = shiny::tagList(shiny::h2("Low-end of Market Rental Monitor, Reports", shiny::icon("download")))),
          shiny::p("These files include the Full Summary report for each neighbourhood and the City of Toronto, summarising data from the aggregate and point layers.")
      )
    ),



    # shiny::fluidRow(
    #   shiny::column(
    #     width = 12,
    #     shiny::p("The data used in the Low-end of Market Rental Monitor is available in two datasets and in a set of reports.")
    #   ),
    # ),
    # shiny::fluidRow(
    #   shiny::div(
    #     class = "vertical-align",
    #     shiny::column(
    #       width = 9,
    #       shiny::h2("Low-end of Market Rental Monitor, Aggregate Layers"),
    #       shiny::p("This dataset includes the data used in each of the aggregate layers in this tool, as well as all values shown in the full summary view, for each neighbourhood and the City of Toronto.")
    #     ),
    #     shiny::column(
    #       width = 3, align = "center",
    #       shiny::downloadButton(
    #         ns("aggregate_layers"),
    #         shiny::HTML("<h3>Download<br/><i>Aggregate Layers</i></h3>"),
    #         icon = NULL
    #       )
    #     )
    #   )
    # ),
    # shiny::fluidRow(
    #   shiny::div(
    #     class = "vertical-align",
    #     shiny::column(
    #       width = 9,
    #       shiny::h2("Low-end of Market Rental Monitor, Point Layers"),
    #       shiny::p("This dataset includes the data used in each of the point layers in the tool. It contains all addresses and spatial information along with the layer values.")
    #     ),
    #     shiny::column(
    #       width = 3, align = "center",
    #       shiny::downloadButton(
    #         ns("aggregate_layers"),
    #         shiny::HTML("<h3>Download<br/><i>Point Layers</i></h3>"),
    #         icon = NULL
    #       )
    #     )
    #   )
    # ),
    # shiny::fluidRow(
    #   shiny::div(
    #     class = "vertical-align",
    #     shiny::column(
    #       width = 8,
    #       shiny::h2("Low-end of Market Rental Monitor, Reports"),
    #       shiny::p("These files include the Full Summary report for each neighbourhood and the City of Toronto, summarising data from the aggregate and point layers.")
    #     ),
    #     shiny::column(
    #       width = 4, align = "center",
    #       shiny::downloadButton(
    #         ns("aggregate_layers"),
    #         shiny::HTML("<h3>Download<br/><i>Reports</i></h3>"),
    #         icon = NULL
    #       )
    #     )
    #   )
    # ),
    shiny::div(class = "divider-line"),
    shiny::tagList(
      lemr::data_and_definitions %>%
        dplyr::mutate(definition_full = purrr::pmap(list(.data$name, .data$description, .data$data_source_prefix, .data$data_source_suffix, .data$data_source, .data$data_source_link), format_definition)) %>%
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
