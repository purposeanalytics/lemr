#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "welcome-banner",
      shiny::div(
        class = "welcome-banner-content",
        shiny::h1("Welcome to the Low-end of Market Rental Monitor", style = "font-size: 2em;"),
        shiny::tags$i(shiny::HTML("This tool is a <b>proof of concept</b> that demonstrates the potential for linking housing data from multiple sources with a future goal of estimating the volume of deeply affordable housing units, where they are located, and how this is changing over time."))
      )
    ),
    shiny::div(
      class = "content-page padded home",
      # shiny::includeMarkdown(app_sys("app", "home.md"))
      shiny::div(
        class = "intro-sentence biggest",
        shiny::HTML("<b>LEMR</b> is an interactive tool developed to understand changes in the supply of deeply affordable rental housing in the City of Toronto.")
      ),
      shiny::div(
        class = "intro-paragraph padded",
        shiny::h2("Why is this important?"),
        shiny::p(shiny::HTML("Housing is becoming increasingly expensive and anecdotal evidence suggests that deeply affordable rental is disappearing faster than new supply is being created. <i>Deeply affordable</i> describes private market rental housing units that are affordable to households at the low-end of the income spectrum. A long-standing convention defines housing to be affordable when shelter costs do not exceed 30% of before-tax household income.")),
        shiny::p("By mapping the dynamics of the low-end of the rental market, LEMR equips policy makers, housing advocates, city planners, government agencies, and researchers with information that can help them to tackle this problem. Access to data is crucial for developing policies, regulation, and programs that protect this important component of the market and advance the right to adequate housing.")
      ),
      shiny::div(
        class = "data-stories-banner",
        shiny::h2(shiny::HTML("Featured <i>Data Stories</i>")),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            shiny::h3("Title"), shiny::p("Above Guideline Increase applications and Tenant Defense Fund grants in Toronto: three neighbourhoods fall well outside the norm.")
          ),
          shiny::column(
            width = 4,
            shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            shiny::h3("Title"), shiny::p("Above Guideline Increase applications and Tenant Defense Fund grants in Toronto: three neighbourhoods fall well outside the norm.")
          ),
          shiny::column(
            width = 4,
            shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            shiny::h3("Title"), shiny::p("Above Guideline Increase applications and Tenant Defense Fund grants in Toronto: three neighbourhoods fall well outside the norm.")
          )
        )
      ),
      shiny::div(class = "divider-line"),
      shiny::div(
        class = "start-exploring",
        shiny::column(width = 12, align = "center", shiny::h2("Start exploring LEMR", style = "padding-top: 0;")),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::column(
              width = 4,
              shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            ),
            shiny::column(
              width = 8,
              shiny::HTML("Read <b>Data Stories</b> above for our data analyses and learn how their implications may impact you, your organization, or your community.")
            )
          ),
          shiny::column(
            width = 4,
            shiny::column(
              width = 4,
              shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            ),
            shiny::column(
              width = 8,
              shiny::p("Go to", shiny::tags$a(id = "link_map", href = "#", onclick = "link('Map')", shiny::tags$b("Map")), "for summary statistics, estimated rental supply, locations of awarded tenant defence fund grants, and more.")
            )
          ),
          shiny::column(
            width = 4,
            shiny::column(
              width = 4,
              shiny::div(style = "height: 100px; background-color: #F0F0F0;"),
            ),
            shiny::column(
              width = 8,
              shiny::p("Learn about the data sources included in the tool and find key terminology used throughout in", shiny::tags$a(id = "link_data_and_definitions", href = "#", onclick="link('Data & Definitions')", shiny::tags$b("Data & Definitions")), ".")
            )
          )
        )
      ),
      shiny::fluidRow(shiny::column(class = "questions biggest", width = 12, align = "center", shiny::HTML("<span style = 'color: var(--main-color);'>Questions?</span> Contact us."))),
      shiny::div(class = "divider-line"),
      shiny::div(
        class = "about",
        shiny::p(shiny::HTML("LEMR was developed by <b><a href = 'https://purposeanalytics.ca/' target = '_blank'>Purpose Analytics</a></b> as a short-listed project through the Canada Mortgage Housing Corporation's Housing Supply Challenge Data Driven Round.")),
        shiny::p("Purpose Analytics is partnering with the Centre for Equality Rights in Accommodation, Canadian Alliance to End Homelessness, BC Non-profit Housing Association, Ontario Non-profit Housing Association, and the Community Housing Transformation Centre in applying for second stage funding to scale this proof of concept to major urban areas across Canada."),
        shiny::p("For the source code, you will be able to visit the project's GitHub repository."),
        shiny::p(shiny::tags$i("Last updated: September 2021"))
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$agi_tdf_data_story, {
      shiny::showModal(
        shiny::modalDialog(
          size = "l",
          mod_data_story_agi_tdf_ui(ns("agi_tdf"))
        )
      )
    })

    mod_data_story_agi_tdf_server("agi_tdf")
  })
}

## To be copied in the UI
# mod_home_ui("home")

## To be copied in the server
# mod_home_server("home")
