#' layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_layers_ui <- function(id) {
  ns <- NS(id)
  shiny::column(
    width = 12,
    bsplus::use_bs_popover(),
    shiny::hr(),
    bigger_padded(shiny::tags$b("Display layers"), shiny::icon("chevron-down")) %>%
      bsplus::bs_attach_collapse(ns("layers")),
    bsplus::bs_collapse(
      id = ns("layers"),
      content = shiny::tagList(
        padded("Select one aggregate data layer:"),
        # LEM ----
        create_full_legend(
          icon = create_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("lem"),
            choices = list("Low-end of Market Rentals" = "lem"),
            justified = TRUE
          ),
          legend = generate_layers_legend(c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A"), "0", "100")
        ),
        # Amenity Density ------
        create_full_legend(
          icon = create_popover(title = "Amenity Density", content = "This layer shows the amenity density of census block. An area is low amenity dense (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It is medium amenity dense (yellow) if it has access to all eight, and high amenity dense (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("amenity_density"),
            choices = list("Amenity Density" = "amenity_density"),
            justified = TRUE
          ),
          legend = generate_low_mid_high_legends(rev(c(low_colour, mid_colour, high_colour)), "Low", "Medium", "High")
        ),
        padded("Select one or more points data layers:"),
        # Apartment Buildings -----
        create_full_legend(
          icon = create_popover(title = "Apartment Buildings", content = "This layer shows the location of all apartment buildings with at least three storeys and at least ten units in the City of Toronto. Each point contains information on the year built, number of units, landlord or property management, RentSafeTO evaluation scores, and above guideline increase applications, as relevant."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("apartment_buildings"),
            choices = list("Apartment Buildings" = "apartment_buildings"),
            justified = TRUE
          ),
          legend = NULL
        ),
        # RentSafeTO -----
        create_full_legend(
          icon = create_popover(title = "RentSafeTO Evaluation Scores", content = "This layer shows the latest evaluation scores for buildings registered with RentSafeTO. Buildings must undergo evaluation at least once every three years. Scores range from 0% to 100%. Light yellow indicates a failing score (50% or lower) while dark red indicates 100%. Apartments that fail the evaluation by scoring less than 50% must undergo an audit."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("apartment_evaluation"),
            choices = list("RentSafeTO Evaluation Scores" = "apartment_evaluation"),
            justified = TRUE
          ),
          legend = generate_layers_legend(c("#FFFFCC", "#FED976", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "50%", "100%")
        ),
        # AGI Applications -----
        create_full_legend(
          icon = create_popover(title = "Above Guideline Increase Applications", content = "This layer shows the locations of rentals whose landlords applied for an Above Guideline Increase (AGI) in the rent."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("agi"),
            choices = list("AGI Applications" = "agi"),
            justified = TRUE
          ),
          legend = NULL
        ),
        # Tenant Defence Fund -----
        create_full_legend(
          icon = create_popover(title = "Tenant Defence Fund Grants", content = "This layer shows the locations of rentals who received a Tenant Defence Fund grant for the above guideline increases their landlords requested."),
          button = shinyWidgets::checkboxGroupButtons(
            inputId = ns("tdf"),
            choices = list("Tenant Defence Fund Grants" = "tdf"),
            justified = TRUE
          ),
          legend = NULL
        )
      )
    )
  )
}
#' layers Server Functions
#'
#' @noRd
mod_layers_server <- function(id, point_layers, aggregate_layers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(
      {
        input$apartment_buildings
        input$apartment_evaluation
        input$agi
        input$tdf
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$apartment_buildings, input$apartment_evaluation, input$agi, input$tdf)

        # Update reactive with value from input
        point_layers(active_layers)
      }
    )

    shiny::observeEvent(
      {
        input$amenity_density
        input$lem
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$amenity_density, input$lem)

        # Update reactive with value from input
        aggregate_layers(active_layers)
      }
    )
  })
}

popup_icon <- htmltools::tags$i(class = "far fa-question-circle", role = "presentation", `aria-label` = "question-circle icon")

create_popover <- function(icon = popup_icon, title, content) {
  icon %>%
    bsplus::bs_embed_popover(title = title, content = content, placement = "right", container = "body", trigger = "hover")
}

create_full_legend <- function(icon, button, legend) {
  shiny::fluidRow(
    shiny::column(
      width = 1,
      class = "layer-popup",
      icon
    ),
    shiny::column(
      width = 6,
      style = "padding-left: 0",
      button
    ),
    shiny::column(
      width = 5,
      legend
    )
  )
}

point_layers_choices <- list("Apartment Buildings" = "apartment_buildings", "RentSafeTO Evaluation Scores" = "apartment_evaluation", "AGI Applications" = "agi", "Tenant Defence Fund" = "tdf")

aggregate_layers_choices <- list("Amenity Density" = "amenity_density", "Low-end of Market Rentals" = "lem")

generate_layers_legend <- function(colors, min_text, max_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
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

generate_low_mid_high_legends <- function(colors, min_text, mid_text, max_text) {
  colors <- purrr::map(colors, function(x) {
    shiny::div(class = "legend-element", style = glue::glue("background-color: {x};"))
  })

  shiny::div(
    class = "legend",
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

## To be copied in the UI
# mod_layers_ui("layers_ui_1")

## To be copied in the server
# mod_layers_server("layers_ui_1")
