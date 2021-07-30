#' Map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mapboxer::mapboxerOutput(ns("map"))
  )
}

#' Map Server Functions
#'
#' @noRd
mod_map_server <- function(id, address_and_neighbourhood, search_method, layer_apartment_building, layer_amenity_density) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial map ----
    output$map <- mapboxer::renderMapboxer({
      map_toronto() %>%
        add_blank_amenity_density_layer() %>%
        add_blank_apartment_layer() %>%
        add_blank_address_layer() %>%
        add_blank_neighbourhood_layer() %>%
        htmlwidgets::onRender("function() {
      var map = mapboxer._widget['map-map'].map;
      map.on('zoomend', function () {
      var mapZoom = map.getZoom();
      Shiny.onInputChange('mapZoom', mapZoom);
      });
        }")
    })

    # Observe clicking on map ----
    # Note that there is not actually an input called "map_onclick" - it comes built in with renderMapboxer
    shiny::observeEvent(
      input$map_onclick,
      {
        # Clear inputs
        # TODO - not working yet to actually trigger resetting of searches in  mod_search
        address_and_neighbourhood$address <- NULL
        address_and_neighbourhood$neighbourhood <- NULL

        # Update search method and neighbourhood, let the next observe handle actually updating the map :)
        search_method("neighbourhood")
        address_and_neighbourhood$neighbourhood <- input$map_onclick$props$neighbourhood
      }
    )

    # Update zoom of map and highlighted apartment and/or neighbourhood based on search -----
    shiny::observeEvent(
      {
        search_method()
        address_and_neighbourhood$neighbourhood
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE,
      {
        if (search_method() == "address") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            zoom_map_to_address(address_and_neighbourhood$address_sf) %>%
            zoom_map_to_neighbourhood(address_and_neighbourhood$neighbourhood) %>%
            mapboxer::update_mapboxer()
        } else if (search_method() == "neighbourhood") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            # Clear address
            toggle_layer_invisible("address_points") %>%
            zoom_map_to_neighbourhood(address_and_neighbourhood$neighbourhood) %>%
            mapboxer::update_mapboxer()
        } else if (search_method() == "back") {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            # Clear address
            toggle_layer_invisible("address_points") %>%
            # Clear neighbourhood
            zoom_map_to_neighbourhood("none") %>%
            # Zoom back out to Toronto
            mapboxer::fit_bounds(sf::st_bbox(lemur::toronto), maxZoom = 11, pitch = 0, bearing = -15) %>%
            mapboxer::update_mapboxer()
        }
      }
    )

    # Update layers -----

    ## Apartment buildings
    shiny::observeEvent(
      layer_apartment_building(),
      ignoreInit = TRUE,
      {
        if (layer_apartment_building()) {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            toggle_layer_visible(id = "apartment_buildings") %>%
            mapboxer::update_mapboxer()
        } else if (!layer_apartment_building()) {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            toggle_layer_invisible(id = "apartment_buildings") %>%
            mapboxer::update_mapboxer()
        }
      }
    )

    ## Amenity density
    shiny::observeEvent(
      layer_amenity_density(),
      ignoreInit = TRUE,
      {
        if (layer_amenity_density()) {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            toggle_layer_visible(id = "amenity_density_fill") %>%
            toggle_layer_visible(id = "amenity_density_outline") %>%
            mapboxer::update_mapboxer()
        } else if (!layer_amenity_density()) {
          mapboxer::mapboxer_proxy(ns("map")) %>%
            toggle_layer_invisible(id = "amenity_density_fill") %>%
            toggle_layer_invisible(id = "amenity_density_outline") %>%
            mapboxer::update_mapboxer()
        }
      }
    )
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
