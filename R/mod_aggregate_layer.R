#' Aggregate Layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_aggregate_layer_ui <- function(id) {
  ns <- NS(id)

  label <- aggregate_layers_choices[[id]]

  lem_legend <- generate_layers_legend(c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A"), "0", "100", alt_text = "A legend showing values for low-end of market rentals, from 0 (white) to 100 (dark blue).")
  amenity_density_legend <- generate_low_mid_high_legends(c(low_colour, mid_colour, high_colour), "Low", "Medium", "High", alt_text = "A legend showing possible values for amenity density: low (green), medium (yellow), and high (purple).")

  lem_tooltip <- create_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab.")
  amenity_density_tooltip <- create_popover(title = "Proximity to amenities", content = "This layer shows the proximity to amenities of each census block. An area has low proximity to amenities (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It has medium proximity (yellow) if it has access to all eight, and high proximity (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population.")

  shiny::tagList(
    bsplus::use_bs_popover(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::conditionalPanel(
          "input.layer == 'lem'",
          shiny::h2(
            "Select aggregate layer",
            lem_tooltip
          ),
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'amenity_density'",
          shiny::h2(
            "Select aggregate layer",
            amenity_density_tooltip
          ),
          ns = ns
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectInput(inputId = ns("layer"), label = NULL, choices = stats::setNames(names(aggregate_layers_choices), unname(aggregate_layers_choices)), selected = "lem", multiple = FALSE)
      )
    ),
    shiny::fluidRow(
      style = "position: relative;",
      shiny::column(
        width = 4,
        class = "summary-legend very-padded",
        shiny::conditionalPanel(
          "input.layer == 'lem'",
          lem_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'amenity_density'",
          amenity_density_legend,
          ns = ns
        )
      ),
      shiny::column(
        width = 7,
        offset = 5,
        align = "right",
        style = "position: absolute; bottom: 0; right: 0;",
        shiny::textOutput(ns("layer_summary"))
      )
    )
  )
}

#' Aggregate Layer Server Functions
#'
#' @noRd
mod_aggregate_layer_server <- function(id, address_and_neighbourhood, aggregate_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update aggregate_layers() reactive with layer selected so that mod_map updates it
    shiny::observe(
      aggregate_layers(input$layer)
    )

    # Content ----

    neighbourhood <- shiny::reactive({
      address_and_neighbourhood$neighbourhood
    })

    level <- shiny::reactive({
      if (is.null(neighbourhood())) {
        "city"
      } else {
        "neighbourhood"
      }
    })

    dataset <- shiny::reactive({
      determine_dataset_from_level(level(), neighbourhood())
    })

    output$layer_summary <- shiny::renderText({
      switch(input$layer,
        lem = glue::glue("Estimated LEM Units: {units}",
          units = scales::comma(dataset()[["lem"]][["Total"]] %>% sum())
        ),
        amenity_density = dataset()[["amenity_density"]] %>%
          dplyr::filter(.data$group != "Unknown") %>%
          dplyr::mutate(res = glue::glue("{group}: {scales::percent(prop)}")) %>%
          dplyr::pull(res) %>%
          glue::glue_collapse(sep = "; ")
      )
    })
  })
}

generate_layers_legend <- function(colors, min_text, max_text, alt_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
    role = "img",
    `aria-label` = alt_text,
    shiny::tags$ul(
      shiny::tags$li(class = "min", min_text),
      shiny::tags$li(class = "max", max_text),
      shiny::tags$li(
        class = "legend-colors",
        shiny::div(
          class = "colors",
          colors
        )
      )
    )
  )
}

generate_low_mid_high_legends <- function(colors, min_text, mid_text, max_text, alt_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
    role = "img",
    `aria-label` = alt_text,
    shiny::div(
      class = "triple-text",
      shiny::tags$ul(
        shiny::tags$li(min_text),
        shiny::tags$li(mid_text),
        shiny::tags$li(max_text)
      )
    ),
    shiny::tags$li(
      class = "legend-colors",
      shiny::div(
        class = "colors",
        colors
      )
    )
  )
}

aggregate_layers_choices <- list(lem = "Low-end of market rentals", amenity_density = "Proximity to amenities")

popup_icon <- shiny::tags$i(class = "far fa-question-circle", role = "presentation", `aria-label` = "question-circle icon", style = "color: var(--grey-color);")

create_popover <- function(icon = popup_icon, title, content) {
  icon %>%
    bsplus::bs_embed_popover(title = title, content = content, placement = "right", container = "body", trigger = "hover")
}

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
