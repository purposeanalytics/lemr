#' data_story_agi_tdf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_story_agi_tdf_ui <- function(id) {
  map_legend <- shiny::tagList(
    create_circle_legend(layer_colours[["agi_apartment"]], "Apartment buildings with AGI applications", alt_text = "A legend showing the colour of the points of above guideline increase applications for apartment buildings."),
    create_circle_legend(layer_colours[["tdf"]], "TDF grants", alt_text = "A legend showing the colour of the points of tenant defense fund grants.")
  )

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
          shiny::h1("Above Guideline Increase Applications and Tenant Defense Fund Grants in Toronto: Three Neighbourhoods Fall Outside the Norm")
        )
      ),
      shiny::p("A notice of rent increase, recalculating the monthly budget, cutting down on non-essential expenses - or finding a new home. The cycle is not unfamiliar to renters in Toronto. In Ontario, renters are protected by a provincially mandated rent increase guideline that is tied to the Consumer Price Index. However, when landlords incur capital expenses for completing major work, building security, or accessibility features, or extraordinary increases to their property tax, they may apply to pass those costs on to tenants through an Above Guideline Increase, or AGI. In turn, tenants may choose to contest an AGI through the Landlord Tenant Board, a process that can be time-consuming and costly. To support this, the City of Toronto offers assistance, financial and otherwise, through the Tenant Defense Fund (TDF) program to tenants in private rental housing."),
      shiny::p(glue::glue("In the City of Toronto, AGI applications are abundant. Out of {n_apt} privately owned apartment buildings in the city, almost thirty percent of them had at least one AGI application in the last five years, and of these, just under a quarter received a TDF grant. In this story, we look at the distribution of AGIs and TDF grants across neighbourhoods of Toronto.",
        n_apt = scales::comma(city_aggregate[["number_of_buildings_private"]])
      )),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shiny::h2("Percent of buildings that received an Above Guideline Increase application in the last 5 years versus percent of those buildings that received a Tenant Defence Fund grant, by neighbourhood"),
          shiny::tags$picture(
            shiny::tags$source(
              media = "(orientation: landscape)",
              srcset = "www/agi_vs_tdf_wide.png 2700w",
              sizes = "800px"
            ),
            shiny::tags$source(
              media = "(orientation: portrait)",
              srcset = "www/agi_vs_tdf_wide.png 2700w, www/agi_vs_tdf_narrow.png 1500w",
              sizes = "(max-width: 767px) 400px, 800px"
            ),
            shiny::img(
              src = "www/agi_vs_tdf_wide.png",
              title = "Above Guideline Increase versus Tenant Defence Fund",
              width = "100%",
              alt = "A scatter plot showing the percent of buildings with Above Guideline Increases versus the percent of buildings with Tenant Defence Fund grants. The values for Broadview North, Yonge-St.Clair, and Mimico are highlighted in blue while the rest of the neighbourhoods' values are grey."
            )
          ),
          shiny::p(shiny::HTML("<i>Broadview North, Yonge-St.Clair, and Mimico are highlighted as outliers in the city: the rate of Above Guideline Increase applications or Tenant Defense Fund grants in these neighbourhoods fall outside the norm.</i>"))
        )
      ),
      shiny::fluidRow(
        class = "agi-data-story-neighbourhood",
        shiny::column(
          width = 5,
          shiny::h2("Broadview North"),
          shiny::p(
            glue::glue("Broadview North, located in East York, has one of the highest AGI rates for apartment buildings in the city. Here, {n_agi} of the {n_buildings} buildings - or {prop_agi} - have at least one. The rate of TDF grants is close to the city average, at {prop_tdf}, with {n_tdf} of the {n_agi} buildings facing an AGI having received a grant.",
              n_agi = lemr::neighbourhood_aggregate[["Broadview North"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$value),
              n_buildings = lemr::neighbourhood_aggregate[["Broadview North"]][["number_of_buildings_private"]],
              prop_agi = lemr::neighbourhood_aggregate[["Broadview North"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1),
              prop_tdf = lemr::neighbourhood_aggregate[["Broadview North"]][["tdf"]] %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1),
              n_tdf = lemr::neighbourhood_aggregate[["Broadview North"]][["tdf"]] %>%
                dplyr::pull(.data$n)
            ),
            map_legend
          )
        ),
        shiny::column(
          width = 7,
          shiny::img(src = fs::path("www", "map_broadview_north", ext = "png"), title = "Location of AGIs and TDF grants in Broadview North", width = "100%", alt = "A map of Broadview North showing the locations of buildings with Above Guideline Increase applications and buildings that received a Tenant Defence Fund grant")
        )
      ),
      shiny::fluidRow(
        class = "agi-data-story-neighbourhood",
        shiny::column(
          width = 5,
          shiny::h2("Mimico"),
          shiny::p(
            glue::glue("Mimico is a primarily residential neighbourhood southwest of Toronto in Etobicoke. The rate of AGIs here is {prop_agi}, or {n_agi} out of {n_buildings} buildings, but more than half of those buildings, {prop_tdf}, have organized with support from a TDF grant.",
              n_agi = lemr::neighbourhood_aggregate[["Mimico (includes Humber Bay Shores)"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$value),
              n_buildings = lemr::neighbourhood_aggregate[["Mimico (includes Humber Bay Shores)"]][["number_of_buildings_private"]],
              prop_agi = lemr::neighbourhood_aggregate[["Mimico (includes Humber Bay Shores)"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1),
              prop_tdf = lemr::neighbourhood_aggregate[["Mimico (includes Humber Bay Shores)"]][["tdf"]] %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1)
            ),
            map_legend
          )
        ),
        shiny::column(
          width = 7,
          shiny::img(src = fs::path("www", "map_mimico", ext = "png"), title = "Location of AGIs and TDF grants in Mimico", width = "100%", alt = "A map of Mimico showing the locations of buildings with Above Guideline Increase applications and buildings that received a Tenant Defence Fund grant")
        )
      ),
      shiny::fluidRow(
        class = "agi-data-story-neighbourhood",
        shiny::column(
          width = 5, shiny::h2("Yonge-St. Clair"),
          shiny::p(
            glue::glue("Yonge-St. Clair is an affluent, vibrant neighbourhood known for its restaurants, boutiques, and high-rises. Here, while AGIs are noticeably above the average at a rate of {prop_agi}, TDF grants are only at {prop_tdf}. Out of {n_buildings} apartment buildings in the neighbourhood, {n_agi} have at least one AGI. Only {n_tdf} of those buildings have received a TDF grant.",
              n_agi = lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$value),
              n_buildings = lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["number_of_buildings_private"]],
              prop_agi = lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["agi"]] %>%
                dplyr::filter(.data$group == "Apartment building") %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1),
              prop_tdf = lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["tdf"]] %>%
                dplyr::pull(.data$prop) %>%
                scales::percent(accuracy = 0.1),
              n_tdf = lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["tdf"]] %>%
                dplyr::pull(.data$n)
            ),
            map_legend
          )
        ),
        shiny::column(
          width = 7,
          shiny::img(src = fs::path("www", "map_yonge_st_clair", ext = "png"), title = "Location of AGIs and TDF grants in Yonge-St. Clair", width = "100%", alt = "A map of Yonge-St. Clair north showing the locations of buildings with Above Guideline Increase applications and buildings that received a Tenant Defence Fund grant")
        )
      ),
      shiny::p(glue::glue("How do we account for these differences? In 2016, Mimico's average rent was close to the city average at {mimico_rent}, quite a bit higher than Broadview North which had an average rent of {broadview_north_rent}. Both neighbourhoods have buildings that were mostly constructed in the 1950s and 1960s. In Mimico, either tenants are more organized, landlords are more reluctant to bring AGI applications forward, or they have done less work on their buildings in the last five years. By comparison, Yonge-St. Clair had an even higher average rent, {yonge_st_clair_rent}, and a very low rate of TDF grants. As the TDF program focuses on buildings with affordable rents, it is likely that fewer tenants were eligible for this program.",
        mimico_rent = scales::dollar(lemr::neighbourhood_aggregate[["Mimico (includes Humber Bay Shores)"]][["average_renter_shelter_cost"]]),
        broadview_north_rent = scales::dollar(lemr::neighbourhood_aggregate[["Broadview North"]][["average_renter_shelter_cost"]]),
        yonge_st_clair_rent = scales::dollar(lemr::neighbourhood_aggregate[["Yonge-St.Clair"]][["average_renter_shelter_cost"]])
      )),
      shiny::p("As the data shows, not all neighbourhoods are made equal when it comes to affordable rental housing. While the variance across rates of AGIs and TDF grants is undeniable, the conditions that create the discrepancy are not clear-cut. Further housing research, particularly as it pertains to the preservation of affordable rental housing, is crucial."),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shiny::p(shiny::HTML("<i>
For a full data summary, including sociodemographic and housing characteristics either by neighbourhood or city-wide, access the <b>Map</b>. For data sources and key terms, visit <b>Data & Definitions</b>.</i>"))
        )
      )
    )
  )
}

#' data_story_agi_tdf Server Functions
#'
#' @noRd
mod_data_story_agi_tdf_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_story_agi_tdf_ui("data_story_agi_tdf_ui_1")

## To be copied in the server
# mod_data_story_agi_tdf_server("data_story_agi_tdf_ui_1")
