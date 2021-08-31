# Pre-render all templates so that serving them up is super quick

library(lemur)
library(rmarkdown)
library(purrr)
library(pagedown)

neighbourhoods <- lemur::neighbourhoods[["neighbourhood"]]

# Function to render HTML file, then PDF from that
render_reports <- function(level, neighbourhood) {

  html_file <- here::here("inst", "templates", "html", glue::glue("{neighbourhood}.html"))

  render(
    input = here::here("inst", "templates", "neighbourhood_profile.Rmd"),
    output_format = "html_document",
    output_file = html_file,
    params = list(level = level, neighbourhood = neighbourhood),
    quiet = TRUE
  )

  pdf_file <- here::here("inst", "templates", "pdf", glue::glue("{neighbourhood}.pdf"))

  chrome_print(
    html_file,
    output = pdf_file,
    verbose = FALSE
  )
}

# Iterate over neighbourhoods

walk(neighbourhoods[1:2], ~ render_reports("neighbourhood", .x))
