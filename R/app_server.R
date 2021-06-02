#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  address <- mod_address_search_server("address")
  neighbourhood <- mod_neighbourhood_search_server("neighbourhood")
  mod_map_server("map", address)
}
