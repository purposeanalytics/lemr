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
      shiny::textInput(inputId = ns("address"), label = "Address", placeholder = "Search address..."),
      HTML(paste0(" <script>
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('search-address'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
                 address = [
                 (place.address_components[0] && place.address_components[0].short_name || ''),
                 (place.address_components[1] && place.address_components[1].short_name || ''),
                 (place.address_components[2] && place.address_components[2].short_name || ''),
                 (place.address_components[3] && place.address_components[3].short_name || ''),
                 (place.address_components[4] && place.address_components[4].short_name || ''),
                 (place.address_components[5] && place.address_components[5].short_name || ''),
                 (place.address_components[6] && place.address_components[6].short_name || ''),
                 (place.address_components[7] && place.address_components[7].short_name || '')
                 ].join(' ');
                 }
                 var address_number =''
                 address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                 var coords = place.geometry.location;
                 //console.log(address);
                 Shiny.onInputChange('search-jsValue', address);
                 Shiny.onInputChange('search-jsValueAddressNumber', address_number);
                 Shiny.onInputChange('search-jsValuePretty', addressPretty);
                 Shiny.onInputChange('search-jsValueCoords', coords);});}
                 </script>
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
    )
  )
}

#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id, lemur_db, address_and_neighbourhood, search_method) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # If address is selected, store coords and neighbourhood
    shiny::observeEvent(input$jsValueCoords, ignoreInit = TRUE, {

      address_res <- input$jsValueCoords %>%
        dplyr::as_tibble() %>%
        sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)

      address_res$address <- input$jsValuePretty

      address_and_neighbourhood$address <- address_res

      # Get neighbourhood of address
      address_and_neighbourhood$neighbourhood <- address_and_neighbourhood$address %>%
        sf::st_intersection(lemur::neighbourhoods) %>%
        dplyr::pull(.data$neighbourhood)

      # Update search method
      search_method("address")

      # Deselect neighbourhood
      shinyWidgets::updatePickerInput(session = session, inputId = "neighbourhood", selected = character(0))
    })

    # If neighbourhood is selected, store neighbourhood
    shiny::observeEvent(input$neighbourhood, ignoreInit = TRUE, {
      shiny::req(input$neighbourhood != "")

      address_and_neighbourhood$neighbourhood <- input$neighbourhood

      # Update search method
      search_method("neighbourhood")

      # Deselect address
      shiny::updateTextInput(session = session, inputId = "address", value = NULL)
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
          shinyWidgets::updatePickerInput(session = session, inputId = "neighbourhood", selected = character(0))
          shiny::updateTextInput(session = session, inputId = "address", value = NULL)
        }
      }
    )
  })
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# mod_search_server("search_ui_1")
