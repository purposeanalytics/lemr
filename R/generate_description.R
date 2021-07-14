generate_bar_chart_description <- function(sidebar_level, neighbourhood, text) {
  switch(sidebar_level,
    "city" = glue::glue("Distribution of {text} for all households in the City of Toronto."),
    "neighbourhood" = glue::glue("Comparison of {text} for households in {neighbourhood} versus all households in the City of Toronto.")
  )
}

generate_bar_chart_alt_text <- function(sidebar_level, neighbourhood, text) {
  switch(sidebar_level,
         "city" = glue::glue("Bar chart showing distribution of {text} for all households in the City of Toronto. The data is in the table that follows."),
         "neighbourhood" = glue::glue("Bar chart comparing {text} for households in {neighbourhood} versus all households in the City of Toronto. The data is in the table that follows.")
  )
}
