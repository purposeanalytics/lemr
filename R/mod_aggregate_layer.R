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
    shiny::fluidRow(
      shiny::column(
        width = 12,
        id = "aggregate_layer_div",
        shiny::tagList(
          purrr::map(
            names(aggregate_layers_choices),
            ~ generate_conditional_tooltip(.x, ns = ns)
          )
        ),
        shinyWidgets::pickerInput(inputId = ns("layer"), label = NULL, choices = aggregate_layers_choices_grouped, selected = "lem", multiple = FALSE)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        class = "summary-legend padded",
        shiny::tagList(
          purrr::map(
            names(aggregate_layers_choices),
            ~ generate_conditional_legend(.x, ns = ns)
          )
        ),
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
          units = scales::comma(dataset()[["lem"]] %>% dplyr::filter(.data$Bedrooms == "Total") %>% dplyr::pull(.data$Total))
        ),
        amenity_density = dataset()[["amenity_density"]] %>%
          dplyr::filter(.data$group != "Unknown") %>%
          dplyr::mutate(res = glue::glue("{group}: {scales::percent(prop)}")) %>%
          dplyr::pull(.data$res) %>%
          glue::glue_collapse(sep = "; ") %>%
          paste0(" of population"),
        rental_supply_primary = glue::glue("Primary market households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$market == "Primary") %>% dplyr::pull(.data$prop) %>% sum() %>% scales::percent(accuracy = 0.1)),
        rental_supply_condo = glue::glue("Condominium households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$group == "Condo") %>% dplyr::pull(.data$prop) %>% scales::percent(accuracy = 0.1)),
        rental_supply_non_condo = glue::glue("Secondary market non-condominium households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$group == "Non-Condo") %>% dplyr::pull(.data$prop) %>% scales::percent(accuracy = 0.1)),
        core_housing_need = glue::glue("Core housing need: {percent}",
          percent = dataset()[["core_housing_need"]] %>%
            scales::percent(accuracy = 0.1)
        ),
        rental_supply_non_market = glue::glue("Non-market rental households: {percent} of renter households", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$market == "Non-market") %>% dplyr::pull(.data$prop) %>% sum() %>% scales::percent(accuracy = 0.1)),
        eviction_rate = glue::glue("Eviction rate: {percent}",
          percent = format_measure(dataset()[["evictions"]], "evictions")
        ),
        vacancy_rate = glue::glue("Vacancy rate: {percent}",
          percent = format_measure(dataset()[["vacancy_rate_2020"]], "vacancy_rate")
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

# List of layers -----

aggregate_layers_choices <- list(lem = "Low-end of market rentals", rental_supply_primary = "Primary market", rental_supply_condo = "Condos", rental_supply_non_condo = "Non-condo secondary market", rental_supply_non_market = "Non-market", core_housing_need = "Core housing need", eviction_rate = "Eviction rate", amenity_density = "Proximity to services", vacancy_rate = "Vacancy rate")

rental_supply_layers <- c("rental_supply_primary", "rental_supply_condo", "rental_supply_non_condo", "rental_supply_non_market")

rental_supply_layers_list <- aggregate_layers_choices[names(aggregate_layers_choices) %in% rental_supply_layers]

rental_supply_layers_list <- stats::setNames(names(rental_supply_layers_list), unname(rental_supply_layers_list))

rental_supply_layers_list <- list("Rental supply" = rental_supply_layers_list)

non_rental_supply_layers_list <- aggregate_layers_choices[!names(aggregate_layers_choices) %in% rental_supply_layers]

non_rental_supply_layers_list <- stats::setNames(names(non_rental_supply_layers_list), unname(non_rental_supply_layers_list))

aggregate_layers_choices_grouped <- append(non_rental_supply_layers_list, rental_supply_layers_list)

popup_icon <- shiny::tags$i(class = "far fa-question-circle", role = "presentation", `aria-label` = "question-circle icon", style = "color: var(--grey-color);")

create_popover <- function(icon = popup_icon, title, content) {
  icon %>%
    bsplus::bs_embed_popover(title = title, content = content, placement = "right", container = "body", trigger = "hover")
}

# Tooltips ----

generate_conditional_tooltip <- function(layer, ns) {
  shiny::conditionalPanel(
    glue::glue("input.layer == '{layer}'"),
    shiny::h2(
      "Select aggregate layer",
      get(paste0(layer, "_tooltip"))
    ),
    ns = ns
  )
}

lem_tooltip <- create_popover(title = "Low-end of Market Rentals", content = "This layer shows the number of rentals that are either \"deeply affordable\" or \"very affordable\" by neighbourhood. Darker blue indicates more rentals in the low-end, while a lighter blue indicates less. For definitions of \"deeply\" and \"very\" affordable and for methodology, please visit the \"Data and Definitions\" tab.")

amenity_density_tooltip <- create_popover(title = "Proximity to services", content = "This layer shows the proximity to services of each census block. An area has low proximity to services (green) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It has medium proximity (yellow) if it has access to all eight, and high proximity (purple) if its proximity to the eight is in the top third. Darker colours indicate higher population, while lighter colours indicate lower population.")

rental_supply_primary_tooltip <- create_popover(title = "Rental supply: Primary market rentals", content = NULL)

rental_supply_condo_tooltip <- create_popover(title = "Rental supply: Condominium rentals", content = NULL)

rental_supply_non_condo_tooltip <- create_popover(title = "Rental supply: Secondary market non-condominium rentals", content = NULL)

rental_supply_non_market_tooltip <- create_popover(title = "Rental supply: Non market rentals", content = NULL)

core_housing_need_tooltip <- create_popover(title = "Core housing need", content = NULL)

eviction_rate_tooltip <- create_popover(title = "Eviction rate", content = NULL)

vacancy_rate_tooltip <- create_popover(title = "Vacancy rate", content = NULL)

# Legends ----

generate_conditional_legend <- function(layer, ns) {
  shiny::conditionalPanel(
    glue::glue("input.layer == '{layer}'"),
    rlang::exec(paste0(layer, "_legend")),
    ns = ns
  )
}

amenity_density_legend <- function() {
  create_square_legend(amenity_density_colours(), c("Low", "Medium", "High"), alt_text = "A legend showing possible values for proximity to services: low (green), medium (yellow), and high (purple).")
}

lem_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0", "1,500", alt_text = "A legend showing values for low-end of market rentals, from 0 (white) to 1500 (dark blue).")
}

rental_supply_primary_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "primary market"))
}

rental_supply_condo_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "secondary market condominium"))
}

rental_supply_non_condo_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "secondary market non-condominium"))
}

rental_supply_non_market_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rentals, from 0% (white) to 100% (dark blue).", market = "non-market"))
}

core_housing_need_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of renters in core housing need, from 0% (white) to 100% (dark blue)."))
}

eviction_rate_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "20%", alt_text = glue::glue("A legend showing the eviction rate, from 0% (white) to 20% (dark blue)."))
}

vacancy_rate_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "11%", alt_text = "A legend showing the primary market vacancy rate, from 0% (white) to 11% (dark blue).")
}

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
