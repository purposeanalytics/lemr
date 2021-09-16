#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  requireNamespace("sf")

  mod_home_server("home")

  # Check for link click on home page to change page
  shiny::observeEvent(
    input$page_link,
    {
      shiny::updateTabsetPanel(session, inputId = "page", selected = input$page_link)
    }
  )

  mod_page_map_server("map")

  # Tour
  # gen_guide()$init()$start()

  # shiny::observeEvent(input$mapZoom, ignoreInit = TRUE, {
  #   if (input$mapZoom < 12.5 & input$mapZoom != 11) {
  #     search_method("back")
  #     address_and_neighbourhood$address <- NULL
  #     address_and_neighbourhood$neighbourhood <- NULL
  #   }
  # })
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
