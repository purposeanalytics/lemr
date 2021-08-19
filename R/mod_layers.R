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
    shiny::hr(),
    biggest_padded("Display additional layers"),
    bigger_padded("Select one aggregate data layer:"),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        class = "layer-popup",
        popup_icon %>%
          bsplus::bs_embed_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab.", placement = "right", container = "body", trigger = "hover")
      ),
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("lem"),
          choices = list("Low-end of Market Rentals" = "lem"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6,
        generate_layers_legend(c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A", "#053C6B"), "0", "100")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        class = "layer-popup",
        popup_icon %>%
          bsplus::bs_embed_popover(title = "Amenity Density", content = "This layer shows the amenity density of census block. An area is low amenity dense (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It is medium amenity dense (yellow) if it has access to all eight, and high amenity dense (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population.", placement = "right", container = "body", trigger = "hover")
      ),
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("amenity_density"),
          choices = list("Amenity Density" = "amenity_density"),
          justified = TRUE
        )
      ),
      shiny::column(
<<<<<<< HEAD
        width = 6,
        generate_low_mid_high_legends(rev(c(low_colour, mid_colour, high_colour)), "High", "Medium", "Low")
=======
        width = 5,
        generate_low_mid_high_legends(rev(c(low_colour, mid_colour, high_colour)), "Low", "Medium", "High")
>>>>>>> Edit popover text, show on hover
      )
    ),
    bigger_padded("Select one or more points data layers:"),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        class = "layer-popup",
        popup_icon %>%
          bsplus::bs_embed_popover(title = "Apartment Buildings", content = "This layer shows the location of all apartment buildings with at least three storeys and at least ten units in the City of Toronto. Each point contains information on the year built, number of units, landlord or property management, RentSafeTO evaluation scores, and above guideline increase applications, as relevant.", placement = "right", container = "body", trigger = "hover")
      ),
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("apartment_buildings"),
          choices = list("Apartment Buildings" = "apartment_buildings"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        class = "layer-popup",
        popup_icon %>%
          bsplus::bs_embed_popover(title = "RentSafeTO Evaluation Scores", content = "This layer shows the latest evaluation scores for buildings registered with RentSafeTO. Buildings must undergo evaluation at least once every three years. Scores range from 0% to 100%. Light yellow indicates a failing score (50% or lower) while dark red indicates 100%. Apartments that fail the evaluation by scoring less than 50% must undergo an audit.", placement = "right", container = "body", trigger = "hover")
      ),
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("apartment_evaluation"),
          choices = list("RentSafeTO Evaluation Scores" = "apartment_evaluation"),
          justified = TRUE
        )
      ),
      shiny::column(
<<<<<<< HEAD
        width = 6,
        generate_layers_legend(c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "Low", "100%")
=======
        width = 5,
        generate_layers_legend(c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126"), "50%", "100%")
>>>>>>> Edit popover text, show on hover
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        class = "layer-popup",
        popup_icon %>%
          bsplus::bs_embed_popover(title = "Above Guideline Increase Applications", content = "This layer shows the locations of rentals whose landlords applied for an Above Guideline Increase (AGI) in the rent.", placement = "right", container = "body", trigger = "hover")
      ),
      shiny::column(
        width = 6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("agi"),
          choices = list("AGI Applications" = "agi"),
          justified = TRUE
        )
      ),
      shiny::column(
        width = 6
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
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      {
        active_layers <- c(input$apartment_buildings, input$apartment_evaluation, input$agi)

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

popup_icon <- htmltools::tags$i(class = "fa fa-question-circle", role = "presentation", `aria-label` = "question-circle icon")

point_layers_choices <- list("Apartment Buildings" = "apartment_buildings", "RentSafeTO Evaluation Scores" = "apartment_evaluation", "AGI Applications" = "agi")

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
