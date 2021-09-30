#' data_story_lem_proximity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_story_lem_proximity_ui <- function(id) {
  ns <- NS(id)

  shiny::showModal(
    shiny::modalDialog(
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            class = "full-summary-buttons",
            shiny::modalButton("Close")
          ),
          shiny::h1("Toronto's Estimated Low-end of Market Rental Stock and Proximity to Services: A City Overview Highlights Key Neighbourhoods")
        )
      ),
      shiny::HTML("<p>In this story, we look at the aggregate layers in the Low-end of Market Rental Monitor (LEMR) Map to get acquainted with the current state of the rental housing market in Toronto. The low-end of market dataset in this prototype contains estimated values and is used for demonstration purposes only.<sup>1</sup></p>"),
      shiny::HTML("<p>The lower end of the rental market includes both 'deeply affordable' and 'very affordable' housing. The term 'affordable' refers to housing that is intended to cost less than 30% of a household's before-tax income in Toronto. The terms 'deeply affordable' and 'very affordable', as used in this prototype in particular, follow the City of Toronto's definitions for \"proposed minimum affordable rent\" and \"proposed maximum affordable rent\", respectively. These set out specific thresholds for affordable rent costs, taking into account the number of bedrooms in a unit as well as the number of members in a household.<sup>2</sup> Recognizing that this segment of the rental market includes people who are precariously housed, including people in informal rental arrangements, we expect that any estimate that we make will be undercounted.</p>"),
      shiny::tags$i("The estimated annual availability of low-end of market rental units in Toronto tends towards the east end of the city, with Woburn as the neighbourhood with highest concentration of stock."),
      shiny::p("At a glance, it is evident that the bulk of the estimated low-end of market rental stock in Toronto is located in the periphery of the city. Neighbourhoods like Woburn, Malvern, Dorset Park, and Clairlea-Birchmount on the east end, and West-Humber Clairville on the west, stand out as those with the highest concentration of estimated low-end of market rental stock. In contrast, two neighbourhoods closer to the city core, Playter Estates-Danforth and Yonge-Eglinton, are noticeably devoid of any stock in the estimation."),
      shiny::div(
        align = "center",
        shiny::tags$i("Proximity to services in Toronto varies greatly by neighbourhood. The map shows limited high proximity to services, located almost exclusively within Downtown Toronto, and medium and low proximity towards the city's boundaries.")
      ),
      shiny::p("Similarly, a quick exploration of the proximity to services of rental units in the city, shows another radial pattern – as distance from the city core increases, proximity to services decreases. Services include grocery stores, pharmacies, health care facilities, childcare facilities, schools, libraries, public transit stops, and sources of employment. Proximity to these services has an impact on the success of local businesses and the quality of life of residents.<sup>3</sup> Buildings in neighbourhoods like North St. James Town, Regent Park, Kensington-Chinatown, all situated in Downtown Toronto, are entirely within high proximity to services. Yet, buildings in neighbourhoods like Newtonbrook East, Bathurst Manor, and St.Andrew-Windfields, in the city's north, all have a low proximity to services."),
      shiny::div(
        align = "center",
        shiny::tags$i("Considering a neighbourhood’s proportion of estimated annual rental stock in the lower end of the market, as well as the area’s proximity to services, provides valuable insight. Several neighbourhoods in Toronto within high proximity to services have a noticeably low proportion of estimated low-end of market rental stock.")
      ),
      shiny::tags$p("In Toronto, three neighbourhoods stand out due to their high percentage of estimated stock of low-end of market units –  over 25% of the total rental stock per neighbourhood – yet they are mainly located within low proximity to services. These are Rouge, with 97% of the neighbourhood’s residents living within low proximity to services, West Humber-Clairville, with 79% in low proximity, and Clairlea-Birchmount, with 61% in low proximity. Neighbourhoods with higher proximity to services yet a noticeably low stock of low-end of market rental units – less than 1% of the rental stock in each neighbourhood – are Regent Park, with all residents living within high proximity to services; Moss Park, with 92% of residents within high proximity; Church-Yonge Corridor, with 90% in high proximity; and Mount Pleasant West, with 69% in high proximity. It is worth noting that, in this case, we only look at units accessible through the private market and not through public housing agencies, co-ops, or other non-market means."),
      shiny::div(
        align = "center",
        shiny::p(shiny::HTML("<i>
For a full data summary, including sociodemographic and housing characteristics either by neighbourhood or city-wide, access the <b>Map</b>. For data sources and key terms, visit <b>Data & Definitions</b>.</i>"))
      )
    )
  )
}

#' data_story_lem_proximity Server Functions
#'
#' @noRd
mod_data_story_lem_proximity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_story_lem_proximity_ui("data_story_lem_proximity_ui_1")

## To be copied in the server
# mod_data_story_lem_proximity_server("data_story_lem_proximity_ui_1")
