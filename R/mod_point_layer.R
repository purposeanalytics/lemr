#' Point Layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_point_layer_ui <- function(id) {
  ns <- NS(id)

  tooltip <- switch(id,
    apartment_buildings = create_popover(title = "Apartment Buildings", content = "This layer shows the location of all apartment buildings with at least three storeys and at least ten units in the City of Toronto. Each point contains information on the year built, number of units, landlord or property management, RentSafeTO evaluation scores, and above guideline increase applications, as relevant."),
    apartment_evaluation = create_popover(title = "RentSafeTO Evaluation Scores", content = "This layer shows the latest evaluation scores for buildings registered with RentSafeTO. Buildings must undergo evaluation at least once every three years. Scores range from 0% to 100%. Light yellow indicates a failing score (50% or lower) while dark red indicates 100%. Apartments that fail the evaluation by scoring less than 50% must undergo an audit."),
    evictions_hearings = create_popover(title = "Evictions Hearings", content = "This layer shows the locations of eviction hearings scheduled by the Landlord Tenant Board between November 2, 2020 to January 31, 2021."),
    agi = create_popover(title = "Above Guideline Increase Applications", content = "This layer shows the locations of rentals whose landlords applied for an Above Guideline Increase (AGI) in the rent."),
    tdf = create_popover(title = "Tenant Defence Fund Grants", content = "This layer shows the locations of rentals who received a Tenant Defence Fund grant for the above guideline increases their landlords requested.")
  )

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bsplus::use_bs_popover(),
        shinyWidgets::prettyCheckbox(
          inputId = ns("layer"),
          label = point_layers_choices[[id]],
          value = FALSE,
          status = "primary",
          inline = TRUE # Ensures tooltip appears beside, since elements are inline
        ),
        tooltip
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "summary-legend",
        shiny::uiOutput(ns("layer_summary"))
      )
    ),
    shiny::hr()
  )
}

#' Point Layer Server Functions
#'
#' @noRd
mod_point_layer_server <- function(id, address_and_neighbourhood, point_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update reactive with value, mod_map handles what's shown
    shiny::observeEvent(input$layer, ignoreInit = TRUE,
      {
        if (!input$layer) {
          active_layers <- setdiff(point_layers(), id)
        } else {
          active_layers <- c(point_layers(), id)
        }

        # Update reactive with value from input - then mod_map handles what's shown
        point_layers(active_layers)
      }
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

    output$layer_summary <- shiny::renderUI({
      switch(id,
        apartment_buildings = create_circle_legend(layer_colours[["apartment_buildings"]], glue::glue("{scales::comma(units)} units ({scales::comma(buildings)} apartment buildings)", units = dataset()[["number_of_units"]], buildings = dataset()[["number_of_apartments"]]), alt_text = "A legend showing the colour of the points of apartment buildings - a dark blue."),
        apartment_evaluation = shiny::column(width = 6, style = "padding-left: 5px;", generate_layers_legend(c("#FFFFCC", "#FED976", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "50%", "100%", alt_text = "A legend showing values for RentSafeTO evaluation scores, from 50% (light yellow) to 100% (dark red).")),
        evictions_hearings = create_circle_legend(layer_colours[["evictions_hearings"]], "Location of evictions hearings schedules November 2020 to January 2021", alt_text = "A legend showing the yellow colour of the points of eviction hearings."),
        agi = create_circle_legend(layer_colours[["agi"]], "Locations of buildings with above guideline increase applications", alt_text = "A legend showing the green colour of the points of above guideline increase applications."),
        tdf = create_circle_legend(layer_colours[["tdf"]], "Locations of buildings that received Tenant Defense Fund grants", alt_text = "A legend showing the purple colour of the points of tenant defense fund grants.")
      )
    })
  })
}

point_layers_choices <- list(
  apartment_buildings = "Apartment Buildings", apartment_evaluation = "RentSafeTO Evaluation Scores", evictions_hearings = "Evictions Hearings", agi = "Above guideline increase applications",
  tdf = "Tenant Defence Fund grants"
)

## To be copied in the UI
# mod_point_layer_ui("apartment_buildings")

## To be copied in the server
# mod_point_layer_server("apartment_buildings")
