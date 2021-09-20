#' Map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "calc(100vh - 120px) !important;",
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
        add_blank_lem_layer() %>%
        add_blank_rental_supply_layers() %>%
        add_blank_core_housing_need_layer() %>%
        add_blank_evictions_layer() %>%
        add_blank_amenity_density_layer() %>%
        add_blank_points_layers() %>%
        add_blank_address_layer() %>%
        add_blank_neighbourhood_layer() %>%
        # Observe zoom-out level, once rendered, to know whether to zoom back out to "city view"
        htmlwidgets::onRender("function() {
      // Send variable that map is loaded in order to trigger tour
      Shiny.onInputChange('mapLoaded', true);

      var map = mapboxer._widget['map-map'].map;
      // Get zoom level on zoom out, to know when to reset to city view
      map.on('zoomend', function () {
      var mapZoom = map.getZoom();
      Shiny.onInputChange('mapZoom', mapZoom);
      });

      // Highlight / fill neighbourhood on hover

      let hoveredNghdId = null;
      // When the user moves their mouse over the neighbourhood_click layer, we'll update the
      // feature state for the feature under the mouse.
      // Note that the mouse is observed on neighbourhood_click, which is a fill layer, so it's observed when their mouse is inside the neighbourhood
      // But the layer that is actually updated is neighbourhood_hover_line, in order to just update the border of the neighbourhood
      map.on('mousemove', 'neighbourhood_click', (e) => {
        map.getCanvas().style.cursor = 'pointer';
        if (e.features.length > 0) {
          if (hoveredNghdId !== null) {
            map.setFeatureState(
              { source: 'neighbourhoods', sourceLayer: 'neighbourhoods-0jaap1', id: hoveredNghdId },
              { hover: false }
            );
          }
        hoveredNghdId = e.features[0].id;
          map.setFeatureState(
            { source: 'neighbourhoods', sourceLayer: 'neighbourhoods-0jaap1', id: hoveredNghdId },
            { hover: true }
          );
        }
      });
      // When the mouse leaves the neighbourhood_click layer, update the feature state of the
      // previously hovered feature.
      map.on('mouseleave', 'neighbourhood_click', () => {
        if (hoveredNghdId !== null) {
          map.setFeatureState(
            { source: 'neighbourhoods', sourceLayer: 'neighbourhoods-0jaap1', id: hoveredNghdId },
            { hover: false }
          );
        }
        hoveredNghdId = null;
        // Reset the cursor style
        map.getCanvas().style.cursor = '';
      });

        }")
    })

    # Observe clicking on map ----
    # Note that there is not actually an input called "map_onclick" - it comes built in with renderMapboxer
    shiny::observeEvent(
      input$map_onclick,
      {
        current_neighbourhood <- address_and_neighbourhood$neighbourhood
        clicked_neighbourhood <- input$map_onclick$props$neighbourhood

        # Only update anything if the current neighbourhood isn't the same as the clicked neighbourhood - it's annoying to get zoomed back out if you're already there!
        # This probably happens when you're clicking on a POINT in the current neighbourhood

        if (!identical(current_neighbourhood, clicked_neighbourhood)) {
          # Clear inputs
          address_and_neighbourhood$address <- NULL
          address_and_neighbourhood$neighbourhood <- NULL

          # Update search method and neighbourhood, let the next observe handle actually updating the map :)
          search_method("neighbourhood")
          address_and_neighbourhood$neighbourhood <- clicked_neighbourhood
        }
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
            mapboxer::fit_bounds(sf::st_bbox(lemur::toronto), pitch = 0, bearing = bearing) %>%
            mapboxer::update_mapboxer()
        }
      }
    )

    # Update layers -----

    ## Point layers -----
    shiny::observeEvent(
      point_layers(),
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
      priority = 0,
      {
        map <- mapboxer::mapboxer_proxy(ns("map"))

        # Turn selected layers on
        for (l in point_layers()) {
          map <- map %>%
            toggle_layer_visible(l)
        }

        # Turn not-selected layers off
        diff_layers <- setdiff(names(point_layers_choices), point_layers())

        for (l in diff_layers) {
          map <- map %>%
            toggle_layer_invisible(l)
        }

        map %>%
          mapboxer::update_mapboxer()
      }
    )

    ## Aggregate layers -----
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
        diff_layers <- setdiff(names(aggregate_layers_choices), aggregate_layers())

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
