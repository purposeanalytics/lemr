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
  shiny::fluidRow(
    id = ns("text"),
    shiny::column(
      width = 6,
      shiny::textInput(inputId = ns("address"), label = "Address", placeholder = "Search address..."),
      # This exposes the key in the HTML, but apparently that's fine and what everyone does??? Oki
      # It's restricted to only work from https://sharlag.shinyapps.io/lemur/
      # And localhost / 127.0.0.1
      shiny::HTML(paste0("
                 <script src='https://maps.googleapis.com/maps/api/js?key=", Sys.getenv("GOOGLE_CLOUD_TOKEN"), "&libraries=places&callback=initAutocomplete' async defer></script>"))
    ),
    shiny::column(
      width = 6,
      shinyWidgets::pickerInput(
        ns("neighbourhood"),
        "Neighbourhood",
        choices = sort(lemur::neighbourhoods[["neighbourhood"]]),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE, size = 10,
          maxOptions = 1,
          noneSelectedText = "Search neighbourhood..."
        )
      )
    ),
    shiny::column(
      width = 12,
      shiny::uiOutput(ns("back_to_city"), class = "padded")
    )
  )
}

#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id, address_and_neighbourhood, search_method) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate that the address is in Toronto
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("address", function(x) {
      # If it's empty, don't error
      if (x == "") {
        NULL
      } else if (is.null(address_and_neighbourhood$address)) {
        # If the autocomplete has not yet been populated, don't error
        NULL
      } else if (address_and_neighbourhood$address_error) {
        # If the neighbourhood is NULL, it means that the address isn't in Toronto - so error!
        "Address must be in the city of Toronto."
      } else {
        NULL
      }
    })
    iv$enable()

    # If address is selected, store coords and neighbourhood
    shiny::observeEvent(input$jsValueCoords, ignoreInit = TRUE, {
      address_res <- input$jsValueCoords %>%
        dplyr::as_tibble() %>%
        sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)

      address_res$address <- stringr::str_split(input$jsValuePretty, ", Toronto", simplify = TRUE)[[1]]

      address_and_neighbourhood$address <- address_res

      # Get neighbourhood of address
      neighbourhood <- address_and_neighbourhood$address %>%
        sf::st_join(lemur::neighbourhoods) %>%
        dplyr::pull(.data$neighbourhood)

      address_and_neighbourhood$address_error <- length(neighbourhood) == 0

      if (!address_and_neighbourhood$address_error) {
        address_and_neighbourhood$neighbourhood <- neighbourhood
        # Update search method
        search_method("address")

        # Update neighbourhood
        shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "neighbourhood", selected = neighbourhood))
      }
    })

    # If neighbourhood is selected, store neighbourhood
    shiny::observeEvent(input$neighbourhood, ignoreInit = TRUE, ignoreNULL = FALSE, {

      # If it's deselected, treat the same as "back" and go to city view
      if (is.null(input$neighbourhood)) {
        address_and_neighbourhood$address <- NULL
        address_and_neighbourhood$neighbourhood <- NULL

        search_method("back")
      } else {
        # If the input is already stored, it was probably selected via address search - so don't update the search method
        if (identical(address_and_neighbourhood$neighbourhood, input$neighbourhood)) {

        } else {
          address_and_neighbourhood$neighbourhood <- input$neighbourhood
          # Update search method
          search_method("neighbourhood")

          # Deselect address
          shiny::updateTextInput(session = session, inputId = "address", value = "")
        }
      }
    })

    shiny::observeEvent(
      {
        address_and_neighbourhood$address
        address_and_neighbourhood$neighbourhood
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE,
      {
        # If the address_and_neighbourhood are cleared, reset both
        if (is.null(address_and_neighbourhood$address) & is.null(address_and_neighbourhood$neighbourhood)) {
          shinyWidgets::updatePickerInput(session = session, inputId = "neighbourhood", selected = character(0))
          shiny::updateTextInput(session = session, inputId = "address", value = "")
        } else if (!is.null(address_and_neighbourhood$neighbourhood)) {
          # If the neighbourhood is updated (i.e. via click), update the input to be that too
          shinyWidgets::updatePickerInput(session = session, inputId = "neighbourhood", selected = address_and_neighbourhood$neighbourhood)

          # Clear the address
          if (is.null(address_and_neighbourhood$address)) {
            shiny::updateTextInput(session = session, inputId = "address", value = "")
          }
        }
      }
    )

    # Back to city view
    output$back_to_city <- shiny::renderUI({
      if (!is.null(input$neighbourhood)) {
        shiny::actionLink(ns("back"), label = "< Back to City of Toronto view", class = "padded")
      }
    })

    # Observe the back button to reset the inputs and map
    shiny::observeEvent(input$back, {
      address_and_neighbourhood$address <- NULL
      address_and_neighbourhood$neighbourhood <- NULL

      search_method("back")
    })
  })
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# mod_search_server("search_ui_1")
