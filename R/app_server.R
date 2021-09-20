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

  # Trigger tour when map is loaded
  shiny::observeEvent(input$mapLoaded, {
    if (input$mapLoaded) {

      Sys.sleep(1.5)

      # Get cookie
      visited <- glouton::fetch_cookies()

      # If cookie is null, set to "yes" then show the tour
      if (is.null(visited$visited_site)){
        glouton::add_cookie("visited_site", "yes")
        map_guide()$init()$start()
      }
    }
  })

  # shiny::observeEvent(input$mapZoom, ignoreInit = TRUE, {
  #   if (input$mapZoom < 12.5 & input$mapZoom != 11) {
  #     search_method("back")
  #     address_and_neighbourhood$address <- NULL
  #     address_and_neighbourhood$neighbourhood <- NULL
  #   }
  # })
}

map_guide <- function() {
  cicerone::Cicerone$
    new()$
    step(
      "aggregate_layer_div",
      title = "See the big picture",
      description = "Choose a base layer to see aspects of Toronto's rentals."
    )$
    step(
      "points_layer_div",
      title = "Spot the details",
      description = "Enable point layers to identify and compare specific locations of interest."
    )$
    step(
      "map-header-full_summary-modal",
      position = "left",
      title = "Dive into the specifics",
      description = "Open the summary for a comprehensive view of available data, either city-wide or after selecting a specific neighbourhood on the map."
    )$
    step(
      "[data-value='Data & Definitions']",
      is_id = FALSE,
      position = "left",
      title = "Learn more",
      description = "Find data sources and key terms in <b>Data & Definitions</b>."
    )
}
