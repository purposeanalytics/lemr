# Pre-render all templates so that serving them up is super quick

library(lemr)
library(rmarkdown)
library(purrr)
library(pagedown)
devtools::load_all()

neighbourhoods <- lemr::neighbourhoods[["neighbourhood"]]

# Function to render HTML file, then PDF from that
render_reports <- function(level, neighbourhood) {

  # In case of slashes in neighbourhood names, which cause issues
  neighbourhood_clean <- fs::path_sanitize(neighbourhood)

  html_file <- here::here("inst", "reports", "html", glue::glue("{neighbourhood_clean}.html"))

  render(
    input = here::here("inst", "reports", "neighbourhood_profile.Rmd"),
    output_format = "html_document",
    output_file = html_file,
    params = list(level = level, neighbourhood = neighbourhood),
    quiet = TRUE
  )

  pdf_file <- here::here("inst", "reports", "pdf", glue::glue("{neighbourhood_clean}.pdf"))

  chrome_print(
    html_file,
    output = pdf_file,
    verbose = FALSE
  )
}

# Iterate over neighbourhoods

walk(neighbourhoods, ~ render_reports("neighbourhood", .x))

# City level
render_reports("city", "Toronto")
