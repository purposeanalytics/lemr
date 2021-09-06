#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  requireNamespace("sf")

  address_and_neighbourhood <- shiny::reactiveValues()
  search_method <- shiny::reactiveVal()

  mod_search_server("search", address_and_neighbourhood, search_method)

  mod_layers_server("layers", point_layers, aggregate_layers)

  mod_map_server("map", address_and_neighbourhood, search_method, point_layers, aggregate_layers)

  # Header

  mod_sidebar_header_server("header", address_and_neighbourhood, search_method)

  # Layers

  ## Aggregate layers

  aggregate_layers <- shiny::reactiveVal()
  latest_aggregate_layer <- shiny::reactiveVal()

  mod_aggregate_layer_server("lem", address_and_neighbourhood, aggregate_layers, latest_aggregate_layer)
  mod_aggregate_layer_server("amenity_density", address_and_neighbourhood, aggregate_layers, latest_aggregate_layer)

  ## Points layers

  point_layers <- shiny::reactiveVal()

  mod_point_layer_server("apartment_buildings", point_layers)
  mod_point_layer_server("apartment_evaluation", point_layers)
  mod_point_layer_server("evictions_hearings", point_layers)
  mod_point_layer_server("agi", point_layers)
  mod_point_layer_server("tdf", point_layers)

  # Tour
  # gen_guide()$init()$start()

  shiny::observeEvent(input$mapZoom, ignoreInit = TRUE, {
    if (input$mapZoom < 12.5 & input$mapZoom != 11) {
      search_method("back")
      address_and_neighbourhood$address <- NULL
      address_and_neighbourhood$neighbourhood <- NULL
    }
  })
}

gen_guide <- function() {
  cicerone::Cicerone$
    new()$
    step(
      "search-address",
      title = "Zoom map",
      description = "Search by address or neighbourhood to zoom in"
    )
}
