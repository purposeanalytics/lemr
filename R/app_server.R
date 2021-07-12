#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  requireNamespace("sf")

  lemur_pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = system.file("extdata/lemur.sqlite", package = "lemur")
  )

  address_and_neighbourhood <- shiny::reactiveValues()
  search_method <- shiny::reactiveVal()

  mod_search_server("search", lemur_pool, address_and_neighbourhood, search_method)

  mod_map_server("map", address_and_neighbourhood, search_method)

  mod_sidebar_server("sidebar", address_and_neighbourhood, search_method)
}
