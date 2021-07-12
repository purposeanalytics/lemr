#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_search_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 6,
      dqshiny::autocomplete_input(
        id = ns("address"),
        label = "Address",
        placeholder = "Search address...",
        options = lemur::address_points_just_address,
        max_options = 10,
        contains = TRUE
      )
    ),
    shiny::column(
      width = 6,
      dqshiny::autocomplete_input(
        id = ns("neighbourhood"),
        label = "Neighbourhood",
        placeholder = "Search neighbourhood...",
        options = sort(lemur::neighbourhoods[["neighbourhood"]]),
        max_options = 10,
        contains = TRUE
      )
    )
  )
}

#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id, lemur_db, address_and_neighbourhood, search_method) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # If address is selected, store address and neighbourhood
    shiny::observeEvent(input$address, ignoreInit = TRUE, {
      shiny::req(input$address != "")

      address_and_neighbourhood$address <- input$address

      # Get neighbourhood of address
      address_point <- address_points() %>%
        dplyr::filter(.data$address == local(input$address)) %>%
        utils::head(1) %>%
        dplyr::collect()

      address_and_neighbourhood$neighbourhood <- address_point %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_intersection(lemur::neighbourhoods) %>%
        dplyr::pull(.data$neighbourhood)

      # Update search method
      search_method("address")

      # Deselect neighbourhood
      dqshiny::update_autocomplete_input(session = session, id = "neighbourhood", value = "")
    })

    # If neighbourhood is selected, store neighbourhood
    shiny::observeEvent(input$neighbourhood, ignoreInit = TRUE, {
      shiny::req(input$neighbourhood != "")

      address_and_neighbourhood$neighbourhood <- input$neighbourhood

      # Update search method
      search_method("neighbourhood")

      # Deselect address
      dqshiny::update_autocomplete_input(session = session, id = "address", value = "")
    })

    # If the address_and_neighbourhood are cleared, reset both
    shiny::observeEvent(
      {
        address_and_neighbourhood$address
        address_and_neighbourhood$neighbourhood
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE,
      {
        if (is.null(address_and_neighbourhood$address) & is.null(address_and_neighbourhood$neighbourhood)) {
          dqshiny::update_autocomplete_input(session = session, id = "neighbourhood", value = "")
          dqshiny::update_autocomplete_input(session = session, id = "address", value = "")
        }
      }
    )
  })
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# mod_search_server("search_ui_1")
