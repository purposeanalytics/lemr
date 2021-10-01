#' data_story_lem_proximity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_story_lem_proximity_ui <- function(id) {
  ns <- shiny::NS(id)

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
      shiny::div(
        align = "center",
        class = "padded",
        shiny::img(src = fs::path("www", "map_lem_percent", ext = "png"), title = "Low-end of market rental units in the City of Toronto", width = "100%", alt = "A map of the City of Toronto divided into neighborhoods shows the estimated annual availability of low-end of market rental units concentrated in the east and west ends."),
        shiny::tags$i("The estimated annual availability of low-end of market rental units in Toronto tends towards the east end of the city, with Woburn as the neighbourhood with highest concentration of stock.")
      ),
      shiny::p("At a glance, it is evident that the bulk of the estimated low-end of market rental stock in Toronto is located in the periphery of the city. Neighbourhoods like Woburn, Malvern, Dorset Park, and Clairlea-Birchmount on the east end, and West-Humber Clairville on the west, stand out as those with the highest concentration of estimated low-end of market rental stock. In contrast, two neighbourhoods closer to the city core, Playter Estates-Danforth and Yonge-Eglinton, are noticeably devoid of any stock in the estimation."),
      shiny::div(
        align = "center",
        class = "padded",
        shiny::img(src = fs::path("www", "map_proximity_to_services", ext = "png"), title = "Proximity to services in the City of Toronto", width = "100%", alt = "A map of the City of Toronto divided into neighborhoods shows the population's proximity to services, with a higher proximity to services mainly in the city core, medium proximity mainly in a horseshoe shape surrounding the core, and low proximity mainly towards the periphery of the city."),
        shiny::tags$i("Proximity to services in Toronto varies greatly by neighbourhood. The map shows limited high proximity to services, located almost exclusively within Downtown Toronto, and medium and low proximity towards the city's boundaries.")
      ),
      shiny::p(shiny::HTML("Similarly, a quick exploration of the proximity to services of rental units in the city, shows another radial pattern - as distance from the city core increases, proximity to services decreases. Services include grocery stores, pharmacies, health care facilities, childcare facilities, schools, libraries, public transit stops, and sources of employment. Proximity to these services has an impact on the success of local businesses and the quality of life of residents.<sup>3</sup> Buildings in neighbourhoods like North St. James Town, Regent Park, Kensington-Chinatown, all situated in Downtown Toronto, are entirely within high proximity to services. Yet, buildings in neighbourhoods like Newtonbrook East, Bathurst Manor, and St.Andrew-Windfields, in the city's north, all have a low proximity to services.")),
      shiny::div(
        align = "center",
        class = "padded",
        shiny::h2("Percent of population living in low proximity to services versus percent of rental stock that is low-end of market, by neighbourhood"),
        shiny::tags$picture(
          shiny::tags$source(
            media = "(orientation: landscape)",
            srcset = "www/lem_proximity_to_services_wide.png 2700w",
            sizes = "800px"
          ),
          shiny::tags$source(
            media = "(orientation: portrait)",
            srcset = "www/lem_proximity_to_services_wide.png 2700w, www/lem_proximity_to_services_narrow.png 1500w",
            sizes = "(max-width: 767px) 400px, 800px"
          ),
          shiny::img(
            src = "www/lem_proximity_to_services_wide.png",
            title = "Proportion of annual estimated low-end of market stock versus proximity to services",
            width = "100%",
            alt = "A scatterplot shows the relationship between the proportion of estimated low-end of rental market stock in a neighbourhood and its proximity to services. It highlights neighbourhoods with a high proportion on estimated rental stock on the lower end of the market, above 25%, yet within low proximity to services, and neighbourhoods with low estimated stock yet within high proximity to services."
          )
        ),
        shiny::br(),
        shiny::tags$i("Considering a neighbourhood's proportion of estimated annual rental stock in the lower end of the market, as well as the area's proximity to services, provides valuable insight. Several neighbourhoods in Toronto within high proximity to services have a noticeably low proportion of estimated low-end of market rental stock.")
      ),
      shiny::tags$p(glue::glue("In Toronto, three neighbourhoods stand out due to their high percentage of estimated stock of low-end of market units - over 25% of the total rental stock per neighbourhood - yet they are mainly located within low proximity to services. These are Rouge, with {rouge_low} of the neighbourhood's residents living within low proximity to services, West Humber-Clairville, with {west_humber_clairville_low} in low proximity, and Clairlea-Birchmount, with {clairlea_birchmount_low} in low proximity. Neighbourhoods with higher proximity to services yet a noticeably low stock of low-end of market rental units - less than 1% of the rental stock in each neighbourhood - are Regent Park, with all residents living within high proximity to services; Moss Park, with {moss_park_high} of residents within high proximity; Church-Yonge Corridor, with {church_yonge_high} in high proximity; and Mount Pleasant West, with {mount_pleasant_west_high} in high proximity. It is worth noting that, in this case, we only look at units accessible through the private market and not through public housing agencies, co-ops, or other non-market means.",
        rouge_low = lemr::neighbourhood_aggregate[["Rouge"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "Low") %>%
          dplyr::pull("prop") %>%
          scales::percent(),
        west_humber_clairville_low = lemr::neighbourhood_aggregate[["West Humber-Clairville"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "Low") %>%
          dplyr::pull("prop") %>%
          scales::percent(),
        clairlea_birchmount_low = lemr::neighbourhood_aggregate[["Clairlea-Birchmount"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "Low") %>%
          dplyr::pull("prop") %>%
          scales::percent(),
        moss_park_high = lemr::neighbourhood_aggregate[["Moss Park"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "High") %>%
          dplyr::pull("prop") %>%
          scales::percent(),
        church_yonge_high = lemr::neighbourhood_aggregate[["Church-Yonge Corridor"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "High") %>%
          dplyr::pull("prop") %>%
          scales::percent(),
        mount_pleasant_west_high = lemr::neighbourhood_aggregate[["Mount Pleasant West"]][["amenity_density"]] %>%
          dplyr::filter(.data$group == "High") %>%
          dplyr::pull("prop") %>%
          scales::percent()
      )),
      shiny::div(
        align = "center",
        shiny::p(shiny::HTML("<i>
For a full data summary, including sociodemographic and housing characteristics either by neighbourhood or city-wide, access the <b>Map</b>. For data sources and key terms, visit <b>Data & Definitions</b>.</i>"))
      ),
      shiny::div(class = "divider-line"),
      shiny::HTML("<p><sup>[1]</sup> All other datasets in this prototype contain veridical information from a range of sources. For details, visit <b>Data & Definitions</b>.</p>"),
      shiny::HTML("<p><sup>[2]</sup> Visit the <a href = 'https://www.toronto.ca/city-government/planning-development/planning-studies-initiatives/definitions-of-affordable-housing/' target = '_blank'>City of Toronto website</a> for further details on this classification.</p>"),
      shiny::HTML("<p><sup>[3]</sup> Learn about the importance of proximity to services on the <a href = 'https://www150.statcan.gc.ca/n1/pub/18-001-x/18-001-x2020001-eng.htm' target = '_blank'>Statistics Canada website</a>.</p>")
    )
  )
}

#' data_story_lem_proximity Server Functions
#'
#' @noRd
mod_data_story_lem_proximity_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_story_lem_proximity_ui("data_story_lem_proximity_ui_1")

## To be copied in the server
# mod_data_story_lem_proximity_server("data_story_lem_proximity_ui_1")
