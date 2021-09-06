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

  legend <- switch(id,
    lem = generate_layers_legend(c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A"), "0", "100", alt_text = "A legend showing values for low-end of market rentals, from 0 (white) to 100 (dark blue)."),
    amenity_density = generate_low_mid_high_legends(c(low_colour, mid_colour, high_colour), "Low", "Medium", "High", alt_text = "A legend showing possible values for amenity density: low (green), medium (yellow), and high (purple).")
  )

  tooltip <- switch(id,
    lem = create_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab."),
    amenity_density = create_popover(title = "Amenity Density", content = "This layer shows the amenity density of census block. An area is low amenity dense (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It is medium amenity dense (yellow) if it has access to all eight, and high amenity dense (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population.")
  )

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bsplus::use_bs_popover(),
        shinyWidgets::materialSwitch(
          inputId = ns("input"),
          label = label,
          value = id == "lem", # LEM is on by default
          status = "primary",
          inline = TRUE, # Ensures tooltip appears beside, since elements are inline
          right = TRUE
        ),
        tooltip
      )
    ),
    shiny::conditionalPanel("input.input == true",
      shiny::fluidRow(
        shiny::column(
          width = 6,
          style = "padding-left: 60px;",
          legend
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("plot_ui")),
          shiny::htmlOutput(ns("table"))
        )
      ),
      shiny::hr(),
      ns = ns
    )
  )
}

#' Aggregate Layer Server Functions
#'
#' @noRd
mod_aggregate_layer_server <- function(id, address_and_neighbourhood, aggregate_layers, latest_aggregate_layer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update latest layer selected to deselect the other layer
    shiny::observeEvent(input$input, ignoreInit = FALSE, ignoreNULL = FALSE, {
      input <- if (input$input) {
        id
      } else {
        NULL
      }
      if (is.null(input) & identical(latest_aggregate_layer(), id)) {
        # Only send NULL value when it was also the latest selected, otherwise the update input below creates circular logic
        latest_aggregate_layer(input)
      } else if (!is.null(input)) {
        latest_aggregate_layer(input)
      }
    })

    # Now deselect the other one!
    shiny::observeEvent(
      {
        latest_aggregate_layer()
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      {
        # Only allow one aggregate layer on at a time - deselect the others -----
        diff_layers <- setdiff(names(aggregate_layers_choices), latest_aggregate_layer())

        # If the one to be deselected is "this one", deselect it!

        if (id %in% diff_layers) {
          shinyWidgets::updateMaterialSwitch(session, "input", value = FALSE)
        }

        # Update reactive with latest input - then mod_map handles what's shown
        aggregate_layers(latest_aggregate_layer())
      }
    )

    # Content ----

    neighbourhood <- shiny::reactive({
      address_and_neighbourhood$neighbourhood
    })

    show_content <- shiny::reactive({
      if (input$input) {
        id
      } else {
        "none"
      }
    })

    level <- shiny::reactive({
      if (is.null(neighbourhood())) {
        "city"
      } else {
        "neighbourhood"
      }
    })

    compare <- shiny::reactive({
      level() == "neighbourhood"
    })

    dataset <- shiny::reactive({
      determine_dataset_from_level(level(), neighbourhood())
    })

    output$description <- shiny::renderText({
      switch(show_content(),
        lem = NULL,
        amenity_density =
          amenity_density_description(level(), neighbourhood())
      )
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    alt_text <- shiny::reactive({
      switch(show_content(),
        lem = NULL,
        amenity_density =
          amenity_density_plot_alt_text(level(), neighbourhood())
      )
    })

    output$plot <- plotly::renderPlotly({
      switch(show_content(),
        lem = NULL,
        amenity_density =
          amenity_density_plot(dataset(), compare())
      )
    }) %>%
      shiny::bindCache(level(), neighbourhood())

    output$plot_ui <- shiny::renderUI({
      switch(show_content(),
        lem = NULL,
        amenity_density =
          shiny::div(
            role = "img",
            `aria-label` = alt_text(),
            plotly::plotlyOutput(ns("plot"), height = "150px")
          )
      )
    })

    output$table <- shiny::renderText({
      switch(show_content(),
        lem = NULL,
        amenity_density =
          generate_table(dataset(), "amenity_density", compare(), "Amenity density", "Percent") %>%
            kableExtra::footnote(general = "A very small number of areas have unknown amenity density, so values may not add up to 100%.")
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

aggregate_layers_choices <- list(amenity_density = "Amenity Density", lem = "Low-end of Market Rentals")


popup_icon <- shiny::tags$i(class = "far fa-question-circle", role = "presentation", `aria-label` = "question-circle icon")

create_popover <- function(icon = popup_icon, title, content) {
  icon %>%
    bsplus::bs_embed_popover(title = title, content = content, placement = "right", container = "body", trigger = "hover")
}

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
