#' layer_lem UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_aggregate_layer_ui <- function(id) {
  ns <- NS(id)

  label <- switch(id,
    lem = "Low-end of market rentals",
    amenity_density = "Amenity density"
  )

  shiny::column(
    width = 6,
    shinyWidgets::materialSwitch(
      inputId = ns("input"),
      label = label,
      value = id == "lem", # LEM is on by default
      status = "primary",
      right = TRUE
    )
  )
}

#' layer_lem Server Functions
#'
#' @noRd
mod_aggregate_layer_server <- function(id, aggregate_layers, latest_aggregate_layer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update latest layer selected to deselect the other layer
    shiny::observeEvent(input$input, ignoreInit = FALSE, ignoreNULL = FALSE, {
      input <- if(input$input) {id} else {NULL}
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
        diff_layers <- setdiff(aggregate_layers_choices, latest_aggregate_layer())

        # If the one to be deselected is "this one", deselect it!

        if (id %in% diff_layers) {
          shinyWidgets::updateMaterialSwitch(session, "input", value = FALSE)
        }

        # Update reactive with latest input - then mod_map handles what's shown
        aggregate_layers(latest_aggregate_layer())
      }
    )
  })
}

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
