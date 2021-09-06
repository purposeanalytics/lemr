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

  label <- point_layers_choices[[id]]

  tooltip <- switch(id,
    apartment_buildings = create_popover(title = "Apartment Buildings", content = "This layer shows the location of all apartment buildings with at least three storeys and at least ten units in the City of Toronto. Each point contains information on the year built, number of units, landlord or property management, RentSafeTO evaluation scores, and above guideline increase applications, as relevant."),
    apartment_evaluation = create_popover(title = "RentSafeTO Evaluation Scores", content = "This layer shows the latest evaluation scores for buildings registered with RentSafeTO. Buildings must undergo evaluation at least once every three years. Scores range from 0% to 100%. Light yellow indicates a failing score (50% or lower) while dark red indicates 100%. Apartments that fail the evaluation by scoring less than 50% must undergo an audit."),
    evictions_hearings = create_popover(title = "Evictions Hearings", content = "This layer shows the locations of eviction hearings scheduled by the Landlord Tenant Board between November 2, 2020 to January 31, 2021."),
    agi = create_popover(title = "Above Guideline Increase Applications", content = "This layer shows the locations of rentals whose landlords applied for an Above Guideline Increase (AGI) in the rent."),
    tdf = create_popover(title = "Tenant Defence Fund Grants", content = "This layer shows the locations of rentals who received a Tenant Defence Fund grant for the above guideline increases their landlords requested.")
  )

  legend <- switch(id,
    apartment_buildings = create_circle_legend(layer_colours[["apartment_buildings"]], "Locations of apartment buildings", alt_text = "A legend showing the colour of the points of apartment buildings - a dark blue."),
    apartment_evaluation = shiny::column(width = 6, style = "padding-left: 5px;", generate_layers_legend(c("#FFFFCC", "#FED976", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "50%", "100%", alt_text = "A legend showing values for RentSafeTO evaluation scores, from 50% (light yellow) to 100% (dark red).")),
    evictions_hearings = create_circle_legend(layer_colours[["evictions_hearings"]], "Location of evictions hearings schedules November 2020 to January 2021", alt_text = "A legend showing the yellow colour of the points of eviction hearings."),
    agi = create_circle_legend(layer_colours[["agi"]], "Locations of buildings with above guideline increase applications", alt_text = "A legend showing the green colour of the points of above guideline increase applications."),
    tdf = create_circle_legend(layer_colours[["tdf"]], "Locations of buildings that received Tenant Defense Fund grants", alt_text = "A legend showing the purple colour of the points of tenant defense fund grants.")
  )

  shiny::tagList(
    shiny::fluidRow(
      style = "margin-left: 15px;",
      bsplus::use_bs_popover(),
      shinyWidgets::materialSwitch(
        inputId = ns("input"),
        label = label,
        value = FALSE,
        status = "primary",
        inline = TRUE, # Ensures tooltip appears beside, since elements are inline
        right = TRUE
      ),
      tooltip
    ),
    shiny::conditionalPanel("input.input == true",
      shiny::div(
        style = "padding-left: 60px;",
        legend
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

#' Point Layer Server Functions
#'
#' @noRd
mod_point_layer_server <- function(id, point_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update reactive with value, mod_map handles what's shown
    shiny::observeEvent(input$input,
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        layer <- if (input$input) {
          id
        } else {
          NULL
        }
        active_layers <- c(point_layers(), layer)

        # Update reactive with value from input - then mod_map handles what's shown
        point_layers(active_layers)
      }
    )
  })
}

point_layers_choices <- list(
  apartment_buildings = "Apartment Buildings", apartment_evaluation = "RentSafeTO Evaluation Scores", evictions_hearings = "Evictions Hearings", agi = "AGI Applications",
  tdf = "Tenant Defence Fund"
)

## To be copied in the UI
# mod_point_layer_ui("point_layer_ui_1")

## To be copied in the server
# mod_point_layer_server("point_layer_ui_1")
