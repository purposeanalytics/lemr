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
        shiny::h1("Welcome to the Low-end of Market Rental Monitor"),
        shiny::tags$i(shiny::HTML("This tool is a <b>proof of concept</b> that demonstrates the potential for linking housing data from multiple sources with a future goal of estimating the volume of deeply affordable housing units, where they are located, and how this is changing over time."))
      )
    ),
    shiny::div(
      class = "content-page padded home",
      shiny::div(
        class = "intro-sentence biggest",
        shiny::HTML("<b>LEMR</b> is an interactive tool developed to understand changes in the stock of deeply affordable rental housing in the City of Toronto.")
      ),
      shiny::div(
        class = "intro-paragraph padded",
        shiny::h2("Why is this important?"),
        shiny::p(shiny::HTML("Housing is becoming increasingly expensive and anecdotal evidence suggests that deeply affordable rental is disappearing faster than new stock is being created. <i>Deeply affordable</i> describes private market rental housing units that are affordable to households at the low-end of the income spectrum. A long-standing convention defines housing to be affordable when shelter costs do not exceed 30% of before-tax household income.")),
        shiny::p("By mapping the dynamics of the low-end of the rental market, LEMR equips policy makers, housing advocates, city planners, government agencies, and researchers with information that can help them to tackle this problem. Access to data is crucial for developing policies, regulation, and programs that protect this important component of the market and advance the right to adequate housing.")
      ),
      shiny::div(
        class = "data-stories-banner",
        shiny::h2(shiny::HTML("Featured <i>Data Stories</i>")),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            class = "very-padded",
            shiny::actionLink(
              ns("lem_proximity_data_story"),
              shiny::tagList(
                shiny::div(
                  align = "center",
                  # "" alt text indicates this image can be skipped - it does not provide any info
                  shiny::img(src = "www/preview_proximity.png", class = "data-stories-img", alt = "", style = "border: 2px solid var(--grey-color);")
                ),
                shiny::br(),
                shiny::HTML("<b>Toronto's Estimated Low-end of Market Rental Stock and Proximity to Services:</b><br>An Overview Highlights Key Neighbourhoods")
              )
            )
          ),
          shiny::column(
            width = 4,
            class = "padded",
            shiny::actionLink(
              ns("agi_tdf_data_story"),
              shiny::tagList(
                shiny::div(
                  align = "center",
                  shiny::img(src = "www/preview_agi.png", class = "data-stories-img", alt = "", style = "border: 2px solid var(--grey-color);")
                ),
                shiny::br(),
                shiny::HTML("<b>Above Guideline Increase Applications and Tenant Defence Fund Grants in Toronto:</b><br>Three Neighbourhoods Fall Outside the Norm")
              )
            )
          ),
          shiny::column(
            width = 4,
            class = "padded",
            shiny::tagList(
              shiny::div(
                align = "center",
                shiny::img(src = "www/blossom_2.png", class = "data-stories-img", alt = "", style = "border: 2px solid var(--grey-color);")
              ),
              shiny::br(),
              shiny::HTML("<b>Coming soon...</b><br>Check back again soon for new Data Stories.")
            )
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
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::icon("newspaper", class = "fa-5x fa-fw")
            ),
            shiny::column(
              width = 8,
              class = "smaller",
              shiny::HTML("Read <b>Data Stories</b> above for our data analyses and learn how their implications may impact you, your organization, or your community.")
            )
          ),
          shiny::column(
            width = 4,
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::tags$a(id = "link_map", href = "#", onclick = "link('Map')", shiny::icon("layer-group", class = "fa-5x fa-fw"), style = "color: black !important;")
            ),
            shiny::column(
              width = 8,
              class = "smaller",
              shiny::p("Go to", shiny::tags$a(id = "link_map", href = "#", onclick = "link('Map')", shiny::tags$b("Map")), "for summary statistics, estimated rental stock, locations of awarded tenant defence fund grants, and more.")
            )
          ),
          shiny::column(
            width = 4,
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::tags$a(id = "link_data_and_definitions", href = "#", onclick = "link('Data & Definitions')", shiny::icon("book-open", class = "fa-5x fa-fw"), style = "color: black !important;")
            ),
            shiny::column(
              width = 8,
              class = "smaller",
              shiny::p("Learn about the data sources included in the tool, find key terminology used throughout, and access processed data in", shiny::tags$a(id = "link_data_and_definitions", href = "#", onclick = "link('Data & Definitions')", shiny::tags$b("Data & Definitions")), ".")
            )
          )
        )
      ),
      shiny::fluidRow(shiny::column(class = "questions biggest", width = 12, align = "center", shiny::HTML("Questions? <span style = 'color: var(--main-color);'>Contact us.</span>"))),
      shiny::div(class = "divider-line"),
      shiny::div(
        class = "about",
        shiny::p(shiny::HTML("LEMR was developed by <b><a href = 'https://purposeanalytics.ca/' target = '_blank'>Purpose Analytics</a></b> as a short-listed project through the Canada Mortgage Housing Corporation's Housing Supply Challenge Data Driven Round.")),
        full_team,
        shiny::p("Purpose Analytics is partnering with the Centre for Equality Rights in Accommodation, Canadian Alliance to End Homelessness, BC Non-profit Housing Association, Ontario Non-profit Housing Association, and the Community Housing Transformation Centre in applying for second stage funding to scale this proof of concept to major urban areas across Canada."),
        shiny::p(shiny::HTML("For the source code, you can visit the project's <a href = 'https://github.com/purposeanalytics/lemur/' target = '_blank'>GitHub repository</a>.")),
        shiny::p(shiny::tags$i("Last updated: October 2021"))
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
      mod_data_story_agi_tdf_ui(ns("agi_tdf"))
    })

    mod_data_story_agi_tdf_server("agi_tdf")

    shiny::observeEvent(input$lem_proximity_data_story, {
      mod_data_story_lem_proximity_ui(ns("lem_proximity"))
    })
  })
}

team <- dplyr::tribble(
  ~name, ~role, ~link,
  "Lorena Almaraz De La Garza", "Design Researcher", "https://ca.linkedin.com/in/lalmaraz",
  "Nigel Carvalho", "Assistant Housing Consultant", "https://ca.linkedin.com/in/nigel-carvalho",
  "Sharla Gelfand", "Statistician & Software Developer", "https://www.sharlagelfand.com/",
  "Melissa Goldstein", "Housing Consultant", "https://www.melissagoldstein.com/",
  "Tara Kamal Ahmadi", "Data Analyst", "https://ca.linkedin.com/in/tara-kamal-ahmadi-651453a3",
  "Daniel Liadsky", "Project Manager", "https://purposeanalytics.ca/",
  "Thomas Rosenthal", "Data Scientist", "https://purposeanalytics.ca/",
  "Richard Marshall", NA_character_, NA_character_,
  "Steve Pomeroy", NA_character_, "http://www.focus-consult.com/",
  "Jens von Bergmann", NA_character_, "https://mountainmath.ca/"
)

generate_team_item <- function(name, link, role) {
  if (!is.na(link)) {
    name <- glue::glue("<a href = '{link}' target = '_blank'>{name}</a>")
  }

  if (!is.na(role)) {
    role <- glue::glue(", {role}")
  }

  shiny::HTML(glue::glue("{name}{role}<br>", .na = ""))
}

team <- team %>%
  dplyr::mutate(item = purrr::pmap(list(name, link, role), generate_team_item))

team_items <- team %>%
  dplyr::filter(!is.na(role)) %>%
  dplyr::pull(item) %>%
  shiny::tagList()

advisor_items <- team %>%
  dplyr::filter(is.na(role)) %>%
  dplyr::pull(item) %>%
  shiny::tagList()

full_team <- shiny::tagList(
  shiny::p("Team members:", shiny::br(), team_items),
  shiny::p("Advisors:", shiny::br(), advisor_items)
)
