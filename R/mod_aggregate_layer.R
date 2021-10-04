#' Aggregate Layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_aggregate_layer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        id = "aggregate_layer_div",
        shiny::tagList(
          purrr::map(
            aggregate_layers_choices,
            ~ generate_conditional_tooltip(.x, ns = ns)
          )
        ),
        shinyWidgets::pickerInput(inputId = ns("layer"), label = NULL, choices = aggregate_layers_choices_grouped, selected = "lem_percent", multiple = FALSE)
      )
    ),
    shiny::fluidRow(
      shiny::tagList(
        purrr::map(
          aggregate_layers_choices,
          ~ generate_conditional_legend(.x, ns = ns)
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
        lem_percent = glue::glue("Estimated proportion of private low-end of market units (dummy data): {percent}",
          percent = scales::percent(dataset()[["lem_percent"]][["prop"]] %>% sum(), accuracy = 0.1)
        ),
        amenity_density = dataset()[["amenity_density"]] %>%
          dplyr::filter(.data$group != "Unknown") %>%
          dplyr::mutate(res = glue::glue("{group}: {scales::percent(prop)}")) %>%
          dplyr::pull(.data$res) %>%
          glue::glue_collapse(sep = "; ") %>%
          paste0(" of population"),
        rental_supply_primary = glue::glue("Estimated proportion of primary market units (2016): {percent}", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$market == "Primary") %>% dplyr::pull(.data$prop) %>% sum() %>% scales::percent(accuracy = 0.1)),
        rental_supply_condo = glue::glue("Estimated proportion of secondary market condo units (2016): {percent}", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$group == "Condo") %>% dplyr::pull(.data$prop) %>% scales::percent(accuracy = 0.1)),
        rental_supply_non_condo = glue::glue("Estimated proportion of secondary market non-condo units (2016): {percent}", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$group == "Non-Condo") %>% dplyr::pull(.data$prop) %>% scales::percent(accuracy = 0.1)),
        core_housing_need = glue::glue("Renter households in core housing need (2016): {percent}",
          percent = dataset()[["core_housing_need"]] %>%
            scales::percent(accuracy = 0.1)
        ),
        rental_supply_non_market = glue::glue("Estimated proportion of non-market units (2016): {percent}", percent = dataset()[["rental_supply"]] %>% dplyr::filter(.data$market == "Non-market") %>% dplyr::pull(.data$prop) %>% sum() %>% scales::percent(accuracy = 0.1)),
        eviction_rate = glue::glue("Eviction filings per renter households (2016): {percent}",
          percent = format_measure(dataset()[["evictions"]], "evictions")
        ),
        vacancy_rate = glue::glue("Primary market vacancy rate (2020): {percent}",
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

aggregate_layers_choices_grouped <- list(
  `Estimated availability of low-end of market rental units` = "lem_percent",
  `Estimated rental stock` = c(
    `Primary market units` = "rental_supply_primary",
    `Secondary market condo units` = "rental_supply_condo",
    `Secondary market non-condo units` = "rental_supply_non_condo",
    `Non-market units` = "rental_supply_non_market"
  ),
  `Primary market vacancy rate` = "vacancy_rate",
  `Eviction filings` = "eviction_rate",
  `Core housing need` = "core_housing_need",
  `Proximity to services` = "amenity_density"
)

aggregate_layers_choices <- aggregate_layers_choices_grouped %>%
  unlist() %>%
  unname()

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

lem_percent_tooltip <- create_popover(title = "Estimated availability of low-end of market rental units", content = "This layer shows the approximate percent of rental stock that has been defined as \"deeply\" or \"very\" affordable housing. Percentages are calculated by the number of low-end units by the total estimated rental stock. Darker colours indicate a higher percentage.")

amenity_density_tooltip <- create_popover(title = "Proximity to services", content = "This layer shows the proximity to services for each dissemination block. An area has low proximity to services (light blue) if it does not have access to all of the following: grocery store, pharmacy, health care facility, child care facility, primary school, library, public transit stop, and source of employment. It has medium proximity (mid-toned blue) if it has access to all eight, and high proximity (dark blue) if its proximity to the eight is in the top third.")

rental_supply_primary_tooltip <- create_popover(title = "Estimated rental stock: Primary market units", content = "This layer shows the percent of estimated rental stock available through the primary market. Percentages are calculated by the number of primary rental units by the total estimated rental stock. Darker colours indicate a higher percentage.")

rental_supply_condo_tooltip <- create_popover(title = "Estimated rental stock: Secondary market condo units", content = "This layer shows the percent of estimated rental stock within condominiums. Percentages are calculated by the number of rented condo units by the total estimated rental stock. Darker colours indicate a higher percentage.")

rental_supply_non_condo_tooltip <- create_popover(title = "Estimated rental stock: Secondary market non-condo units", content = "This layer shows the percent of estimated rental stock available through the secondary market but is not a condominium. Percentages are calculated by the number of non-condo secondary market units by the total estimated rental stock. Darker colours indicate a higher percentage.")

rental_supply_non_market_tooltip <- create_popover(title = "Estimated rental stock: Non-market units", content = "This layer shows the percent of estimated rental stock available through non-market means, such as Toronto Community Housing, rooming houses, and other non-market housing. Percentages are calculated by the number of non-market units by the total estimated rental stock. Darker colours indicate a higher proportion of units.")

core_housing_need_tooltip <- create_popover(title = "Core housing need", content = "This layer shows the percentage of households meeting the criteria to be in core housing need. According to Statistics Canada, ahousehold is in core housing need if its housing falls below at least one of the adequacy, affordability or suitability standards and if it would have to spend 30% or more of its total before-tax income to pay the median rent of alternative local housing that <i>is</i> acceptable (meets all three housing standards). Darker colours indicate a higher proportion of households.")

eviction_rate_tooltip <- create_popover(title = "Eviction filings", content = "This layer shows the percentage of eviction filings per rental household. Darker colours indicate a higher proportion of eviction filings.")

vacancy_rate_tooltip <- create_popover(title = "Primary market vacancy rate", content = "This layer shows the percent of primary market rental units that sit unoccupied. Darker colours indicate a higher proportion of vacant units.")

# Legends ----

generate_conditional_legend <- function(layer, ns) {
  shiny::conditionalPanel(
    glue::glue("input.layer == '{layer}'"),
    shiny::column(
      width = ifelse(layer == "amenity_density", 12, 6),
      class = "summary-legend padded",
      rlang::exec(paste0(layer, "_legend"))
    ),
    ns = ns
  )
}

amenity_density_legend <- function() {
  create_square_legend(amenity_density_colours(), c("Low", "Medium", "High"), alt_text = "A legend showing possible values for proximity to services: low (light blue), medium (mid toned blue), and high (dark blue).")
}

lem_percent_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "75%", alt_text = "A legend showing the percent of rental supply that is low-end, from 0% (white) to 75% (dark blue).")
}

rental_supply_primary_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rental units, from 0% (white) to 100% (dark blue).", market = "primary market"))
}

rental_supply_condo_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rental units, from 0% (white) to 100% (dark blue).", market = "secondary market condominium"))
}

rental_supply_non_condo_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rental units, from 0% (white) to 100% (dark blue).", market = "secondary market non-condominium"))
}

rental_supply_non_market_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "100%", alt_text = glue::glue("A legend showing the proportion of {market} rental units, from 0% (white) to 100% (dark blue).", market = "non-market"))
}

core_housing_need_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "60%", alt_text = glue::glue("A legend showing the proportion of renters in core housing need, from 0% (white) to 60% (dark blue)."))
}

eviction_rate_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "15%", alt_text = glue::glue("A legend showing the eviction filings rate, from 0% (white) to 15% (dark blue)."))
}

vacancy_rate_legend <- function() {
  generate_layers_legend(low_high_legend_colors(), "0%", "11%", alt_text = "A legend showing the primary market vacancy rate, from 0% (white) to 11% (dark blue).")
}

## To be copied in the UI
# mod_aggregate_layer_ui("layer_lem_ui_1")

## To be copied in the server
# mod_aggregate_layer_server("layer_lem_ui_1")
