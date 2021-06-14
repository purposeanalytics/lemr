#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  requireNamespace("sf")

  search_method <- shiny::reactiveVal()

  address <- mod_address_search_server("address", search_method)
  neighbourhood <- mod_neighbourhood_search_server("neighbourhood", search_method)

  mod_map_server("map", address, neighbourhood)

  mod_sidebar_server("sidebar", address, neighbourhood, search_method)
}
