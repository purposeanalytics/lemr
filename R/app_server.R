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

  point_layers <- shiny::reactiveVal()
  aggregate_layers <- shiny::reactiveVal()

  mod_layers_server("layers", point_layers, aggregate_layers)

  mod_map_server("map", address_and_neighbourhood, search_method, point_layers, aggregate_layers)

  mod_sidebar_server("sidebar", address_and_neighbourhood, search_method)

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
