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
        shiny::h1(shiny::HTML("Welcome to the<br/>Low-end of Market Rental Monitor")),
        shiny::p("This proof of concept demonstrates the potential for linking housing data from multiple sources using the City of Toronto as a pilot site. The future goal is to estimate how the supply of deeply affordable housing rent units in the private market is changing over time.")
      )
    ),
    shiny::div(
      class = "content-page padded home",
      shiny::div(
        class = "intro-paragraph padded center-padded bigger",
        shiny::h2("Why is this important?"),
        shiny::p("Housing is becoming increasingly expensive and anecdotal evidence suggests that deeply affordable rental is disappearing faster than new stock is being created. \"Deeply affordable rental\" describes private market rental housing that is affordable to households at the low-end of the income spectrum. A long-standing convention defines housing to be affordable when shelter costs do not exceed 30% of before-tax household income."),
        shiny::p("LEMR assembles datasets related to the loss of units and displacement of people from the low-end of market, equipping policy makers, housing advocates, and researchers with insight into this . In turn, decision-makers can make ithe supply and preservation of this important segment of the rental stock."),
        shiny::h2("About the data"),
        shiny::p("LEMR draws on housing-related datasets from a variety of sources (e.g. open data, freedom of information requests, other administrative data) and jurisdictions (e.g. municipal, provincial, and federal). For this proof of concept, LEMR presents a convenience sample of datasets that were readily accessible to the project team between April and September 2021. Datasets that were received late in the project cycle or whose access could not be negotiated in time have been excluded from the proof of concept."),
        shiny::p("Important Note: In this proof of concept, the \"estimated availability of low-end of market rental units\" data is a dummy dataset. Datasets that were critical to creating a data-driven estimate were not available in time. The dummy dataset is provided for demonstration purposes and should not be used to make conclusions or decisions about the supply of deeply affordable housing in Toronto. All other datasets presented in the proof of concept are real and from authoritative sources.")
      ),
      shiny::div(
        class = "data-stories-banner",
        shiny::span("", id = "data-stories"),
        shiny::h2(shiny::HTML("Featured Data Stories")),
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
                  shiny::img(src = "www/preview_proximity.png", class = "data-stories-img", alt = "")
                ),
                shiny::HTML("<b>Estimated Annual Availability of Low-end of Market Rental Stock:</b> An example exploration of some of the datasets in LEMR")
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
                  shiny::img(src = "www/preview_agi.png", class = "data-stories-img", alt = "")
                ),
                shiny::HTML("<b>Above Guideline Increase Applications and Tenant Defence Fund Grants in Toronto:</b> A closer look at three neighbourhoods ")
              )
            )
          ),
          shiny::column(
            width = 4,
            class = "padded",
            shiny::tagList(
              shiny::div(
                align = "center",
                shiny::img(src = "www/blossom_2.png", class = "data-stories-img", alt = "")
              ),
              shiny::HTML("<b>Coming soon...</b>")
            )
          )
        )
      ),
      shiny::div(class = "divider-line center-padded"),
      shiny::div(
        class = "start-exploring center-padded",
        shiny::column(width = 12, align = "center", shiny::h2("Start exploring LEMR", style = "padding-top: 0; padding-bottom: 0.5em;")),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::tags$a(id = "link_map", href = "#data-stories", shiny::icon("newspaper", class = "fa-5x fa-fw"))
            ),
            shiny::column(
              width = 8,
              shiny::HTML("Read the <a href='#data-stories'><b>Data Stories</b></a> above for analyses about the state of deeply affordable housing in the city and your community.")
            )
          ),
          shiny::column(
            width = 4,
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::tags$a(id = "link_map", href = "#", onclick = "link('Map')", shiny::icon("layer-group", class = "fa-5x fa-fw"))
            ),
            shiny::column(
              width = 8,
              shiny::p("Explore the data directly in the ", shiny::tags$a(id = "link_map", href = "#", onclick = "link('Map')", shiny::tags$b("Interactive Map")), " and access summary statistics and reports.")
            )
          ),
          shiny::column(
            width = 4,
            class = "vertical-align",
            shiny::column(
              width = 4,
              shiny::tags$a(id = "link_data_and_definitions", href = "#", onclick = "link('Data & Definitions')", shiny::icon("book-open", class = "fa-5x fa-fw"))
            ),
            shiny::column(
              width = 8,
              shiny::p("Download linked datasets, reviews definitions, and learn more about the datasets themselves in", shiny::tags$a(id = "link_data_and_definitions", href = "#", onclick = "link('Data & Definitions')", shiny::tags$b("Data & Definitions.")))
            )
          )
        )
      ),
      shiny::div(class = "divider-line center-padded"),
      shiny::h2(align = "center", "Contact Us"),
      shiny::div(class="contact-form", shiny::HTML(
        "<div class='engage-bay-source-form engagebay-forms' data-id='5664808817917952'><form class='form form-style-form1 default text-center' onsubmit='window.EhForm.submit_form(event,this)' data-id='5664808817917952'><input type='hidden' name='engagebay_skip_captcha' value='true'> <fieldset>
						<div class='form-group mb-3' style=''>
						 <div class='controls'>
							<input data-ebay_field='name' data-ebay_add_as='' id='name' title='' name='name' type='text' style='background-color:#fff;' placeholder='Your First Name*' class='form-control' required='true'>
          </div>
          </div>
          <div class='form-group mb-3' style=''>
          <div class='controls'>
          <input data-ebay_field='last_name' data-ebay_add_as='' id='last_name' title='' name='last_name' type='text' style='background-color:#fff;' placeholder='Your Last Name*' class='form-control' required='true'>
          </div>
          </div>
          <div class='form-group mb-3' style=''>
          <div class='controls'>
          <input data-ebay_field='email' data-ebay_add_as='' id='email' title='' name='email' type='email' style='background-color:#fff;' placeholder='Your Email*' class='form-control' required='true'>
          </div>
          </div>
          <div class='form-group mb-3' style=''>
          <div class='controls'>
          <input data-ebay_field='company' data-ebay_add_as='' id='company' title='' name='company' type='text' style='background-color:#fff;' placeholder='Your Organization*' class='form-control' required='true'>
          </div>
          </div>
          <div class='form-group mb-3' style=''>
          <div class='controls'>
          <textarea rows='2' data-ebay_field='' data-ebay_add_as='ADDASNOTE' id='eb_temp_field_text_box' name='eb_temp_field_text_box' type='textarea' style='background-color:#fff;' placeholder='Your Message*' class='form-control' required='true'></textarea>
          </div>
          </div>
          <div class='form-group mb-3'>
          <div class='controls'>
          <input data-ebay_field='' data-ebay_add_as='ADDASTAG' id='eb_temp_field_hidden_field' name='eb_temp_field_hidden_field' type='hidden' value='LEMR' style='background-color:#fff;' class='form-control'>
          </div>
          </div>
          <div class='form-group mb-3'>
          <div>
          <button data-ebay_field='' data-ebay_add_as='ADDASTAG' class='btn btn-default btn-lg'>Contact Us</button>
          <br>
          <span id='error-msg'></span>
          </div>
          </div>
          </fieldset>
          <div class='error-success-container notices green'></div>
          </form>
          </div>")),
      shiny::div(class = "divider-line center-padded"),
      shiny::div(
        class = "about center-padded",
        shiny::h3("Land Acknowledgement"),
        shiny::p(shiny::HTML("The project team recognizes that the displacement of people who rely on low-end of market housing runs parallel to the inescapable history of displacement of Indigenous Peoples that also occurred on these lands, inflicting harm that continues to reverberate today. We further acknowledge that this project is hosted on the traditional lands of the Mississaugas of the Credit, the Anishinabek, the Chippewa, the Haudenosaunee, and the Wendat peoples.")),
        shiny::h3("About Us"),
        shiny::p(shiny::HTML("LEMR was developed by <a href = 'https://purposeanalytics.ca/' target = '_blank'>Purpose Analytics</a> with funding from the Canada Mortgage Housing Corporation's Housing Supply Challenge – Data Driven Round (Incubation Stage). The views expressed are those of the project and CMHC accepts no responsibility for them.")),
        shiny::p(shiny::HTML("Purpose Analytics is partnering with the <a href='https://equalityrights.org' target='_blank'>Centre for Equality Rights in Accommodation</a>, <a href='https://bcnpha.ca' target='_blank'>BC Non-profit Housing Association</a>, <a href='https://caeh.ca' target='_blank'>Canadian Alliance to End Homelessness</a>, <a href='https://centre.support' target='_blank'>Community Housing Transformation Centre</a>, <a href='https://www.onpha.on.ca' target='_blank'>Ontario Non-profit Housing Association</a> in applying for implementation funding to scale this proof of concept to major urban areas across Canada.")),
        full_team,
        shiny::p(shiny::HTML("For the source code, visit the project's <a href = 'https://github.com/purposeanalytics/lemur/' target = '_blank'>GitHub repository</a>.")),
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
