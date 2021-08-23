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
mod_map_server <- function(id, address_and_neighbourhood, search_method, point_layers, aggregate_layers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial map ----
    output$map <- mapboxer::renderMapboxer({
      map_toronto() %>%
        add_blank_lem_layer()%>%
        add_blank_amenity_density_layer() %>%
        add_blank_address_layer() %>%
        add_blank_apartment_layer() %>%
        add_blank_apartment_evaluation_layer() %>%
        add_blank_evictions_hearings_layer() %>%
        add_blank_agi_layer() %>%
        add_blank_tdf_layer() %>%
        add_blank_neighbourhood_layer() %>%
        # Observe zoom-out level, once rendered, to know whether to zoom back out to "city view"
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
            zoom_map_to_address(address_and_neighbourhood$address) %>%
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
            mapboxer::fit_bounds(sf::st_bbox(lemur::toronto), pitch = 0, bearing = -15) %>%
            mapboxer::update_mapboxer()
        }
      }
    )

    # Update layers -----

    ## Add layers to map if they're not there already ----

    # map_layers <- shiny::reactiveValues()
    #
    # shiny::observeEvent(
    #   {
    #     point_layers()
    #     aggregate_layers()
    #   },
    #   ignoreInit = TRUE,
    #   # Priority = 1 ensures this will get run before the observeEvents below, which actually make the layers visible
    #   priority = 1,
    #   {
    #     map <- mapboxer::mapboxer_proxy(ns("map"))
    #
    #     # Add blank layers to map if they're not there already
    #     for (l in c(point_layers(), aggregate_layers())) {
    #       # Add layer to clicked to indicate it's clicked, but not added
    #       if (!l %in% map_layers$clicked) {
    #         map_layers$clicked <- c(map_layers$clicked, l)
    #       }
    #
    #       # If it's in clicked but not added, add it, then add it to $added
    #       if (l %in% map_layers$clicked & !l %in% map_layers$added) {
    #         map <- switch(l,
    #           "agi" = map %>% add_blank_agi_layer(),
    #           # "apartment_buildings" = map %>% add_blank_apartment_layer(),
    #           "apartment_evaluation" = map %>% add_blank_apartment_evaluation_layer(),
    #           "lem" = map %>% add_blank_lem_layer(),
    #           "amenity_density" = map %>% add_blank_amenity_density_layer()
    #         )
    #
    #         map_layers$added <- c(map_layers$added, l)
    #       }
    #     }
    #
    #     map <- map %>% mapboxer::update_mapboxer()
    #   }
    # )

    ## Point layers
    shiny::observeEvent(
      point_layers(),
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      priority = 0,
      {
        map <- mapboxer::mapboxer_proxy(ns("map"))

        # # Add blank layers to map if they're not there already
        # for (l in point_layers()) {
        #   # Add layer to clicked to indicate it's clicked, but not added
        #   if (!l %in% map_layers$clicked) {
        #     map_layers$clicked <- c(map_layers$clicked, l)
        #   }
        #
        #   # If it's in clicked but not added, add it, then add it to $added
        #   if (l %in% map_layers$clicked & !l %in% map_layers$added) {
        #     map <- switch(l,
        #       "agi" = map %>% add_blank_agi_layer(),
        #       "tdf" = map %>% add_blank_tdf_layer(),
        #       "apartment_buildings" = map %>% add_blank_apartment_layer(),
        #       "apartment_evaluation" = map %>% add_blank_apartment_evaluation_layer()
        #     )
        #
        #     map <- map %>% mapboxer::update_mapboxer()
        #
        #     map_layers$added <- c(map_layers$added, l)
        #   }
        # }

        # Turn selected layers on
        for (l in point_layers()) {
          map <- map %>%
            toggle_layer_visible(l)
        }

        # Turn not-selected layers off
        diff_layers <- setdiff(point_layers_choices, point_layers())

        for (l in diff_layers) {
          map <- map %>%
            toggle_layer_invisible(l)
        }

        map %>%
          mapboxer::update_mapboxer()
      }
    )

    ## Aggregate layers
    shiny::observeEvent(
      aggregate_layers(),
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      priority = 0,
      {
        map <- mapboxer::mapboxer_proxy(ns("map"))

        # Turn selected layers on
        for (l in aggregate_layers()) {
          map <- map %>%
            toggle_layer_visible(l)
        }

        # Turn not-selected layers off
        diff_layers <- setdiff(aggregate_layers_choices, aggregate_layers())

        for (l in diff_layers) {
          map <- map %>%
            toggle_layer_invisible(l)
        }

        map %>%
          mapboxer::update_mapboxer()
      }
    )
  })
}

## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# mod_map_server("map")
