#' Aggregate Layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_aggregate_layer_ui <- function(id) {
  ns <- shiny::NS(id)

  label <- aggregate_layers_choices[[id]]

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
          "input.layer == 'rental_supply_apartment'",
          shiny::h2(
            "Select aggregate layer",
            rental_supply_apartment_tooltip
          ),
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'rental_supply_condo'",
          shiny::h2(
            "Select aggregate layer",
            rental_supply_condo_tooltip
          ),
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'rental_supply_non_condo'",
          shiny::h2(
            "Select aggregate layer",
            rental_supply_non_condo_tooltip
          ),
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'core_housing_need'",
          shiny::h2(
            "Select aggregate layer",
            core_housing_need_tooltip
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
      shiny::column(
        width = 6,
        class = "summary-legend padded",
        shiny::conditionalPanel(
          "input.layer == 'lem'",
          lem_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'rental_supply_apartment'",
          rental_supply_apartment_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'rental_supply_condo'",
          rental_supply_condo_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'rental_supply_non_condo'",
          rental_supply_non_condo_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'core_housing_need'",
          core_housing_need_legend,
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.layer == 'amenity_density'",
          amenity_density_legend(),
          ns = ns
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "summary-legend very-padded",
        shiny::textOutput(ns("layer_summary"))
      )
    )
  )
}

#' Aggregate Layer Server Functions
#'
#' @noRd
mod_aggregate_layer_server <- function(id, address_and_neighbourhood, aggregate_layers) {
  shiny::moduleServer(id, function(input, output, session) {
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
          units = scales::comma(dataset()[["lem"]] %>% dplyr::filter(Bedrooms == "Total") %>% dplyr::pull(Total))
        ),
        amenity_density = dataset()[["amenity_density"]] %>%
          dplyr::filter(.data$group != "Unknown") %>%
          dplyr::mutate(res = glue::glue("{group}: {scales::percent(prop)}")) %>%
          dplyr::pull(res) %>%
          glue::glue_collapse(sep = "; "),
        rental_supply_apartment = glue::glue("Apartment households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(group == "Apartment") %>% dplyr::pull(prop) %>% scales::percent(accuracy = 0.1)),
        rental_supply_condo = glue::glue("Condominium households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(group == "Condo") %>% dplyr::pull(prop) %>% scales::percent(accuracy = 0.1)),
        rental_supply_non_condo = glue::glue("Secondary market non-condominium households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(group == "Non-Condo") %>% dplyr::pull(prop) %>% scales::percent(accuracy = 0.1)),
        core_housing_need = glue::glue("Core housing need: {percent}",
          percent = dataset()[["core_housing_need"]][["prop"]] %>%
            scales::percent(accuracy = 0.1)
        )
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

aggregate_layers_choices <- list(lem = "Low-end of market rentals", rental_supply_apartment = "Rental supply: Apartment rentals", rental_supply_condo = "Rental supply: Condominium rentals", rental_supply_non_condo = "Rental supply: Non-condominium secondary market rentals", core_housing_need = "Core housing need", amenity_density = "Proximity to amenities")

popup_icon <- shiny::tags$i(class = "far fa-question-circle", role = "presentation", `aria-label` = "question-circle icon", style = "color: var(--grey-color);")

create_popover <- function(icon = popup_icon, title, content) {
  icon %>%
    bsplus::bs_embed_popover(title = title, content = content, placement = "right", container = "body", trigger = "hover")
}

amenity_density_legend <- function() {
  create_square_legend(c(lemur:::low_colour, lemur:::mid_colour, lemur:::high_colour), c("Low", "Medium", "High"), alt_text = "A legend showing possible values for amenity density: low (green), medium (yellow), and high (purple).")
}

low_high_legend_colors <- c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")

lem_legend <- generate_layers_legend(low_high_legend_colors, "0", "1,500", alt_text = "A legend showing values for low-end of market rentals, from 0 (white) to 1500 (dark blue).")

rental_supply_apartment_legend <- generate_layers_legend(low_high_legend_colors, "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "primary market apartment"))

rental_supply_condo_legend <- generate_layers_legend(low_high_legend_colors, "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "secondary market condominium"))

rental_supply_non_condo_legend <- generate_layers_legend(low_high_legend_colors, "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "secondary market non-condominium"))

core_housing_need_legend <- generate_layers_legend(low_high_legend_colors, "0%", "100%", alt_text = glue::glue("A legend showing the proportion of renters in core housing need, from 0% (white) to 100% (dark blue)."))

lem_tooltip <- create_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab.")

amenity_density_tooltip <- create_popover(title = "Proximity to amenities", content = "This layer shows the proximity to amenities of each census block. An area has low proximity to amenities (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It has medium proximity (yellow) if it has access to all eight, and high proximity (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population.")

rental_supply_apartment_tooltip <- create_popover(title = "Rental supply: Apartment rentals", content = NULL)

rental_supply_condo_tooltip <- create_popover(title = "Rental supply: Condominium rentals", content = NULL)

rental_supply_non_condo_tooltip <- create_popover(title = "Rental supply: Secondary market non-condominium rentals", content = NULL)

core_housing_need_tooltip <- create_popover(title = "Core housing need", content = NULL)

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
