generate_report <- function(level, neighbourhood = NULL, format, filename = "report") {
  level <- match.arg(level, c("city", "neighbourhood"))
  format <- match.arg(format, c("pdf", "html"))

  if (level == "neighbourhood" & is.null(neighbourhood)) {
    stop("`neighbourhood` must be supplied if level = 'neighbourhood'", call. = FALSE)
  }

  if (level == "neighbourhood") {
    if (!neighbourhood %in% lemur::neighbourhoods[["neighbourhood"]]) {
      stop(neighbourhood, "is not a valid Toronto neighbourhood. See lemur::neighbourhoods", call. = FALSE)
    }
  }

  rmarkdown::render(system.file("templates/neighbourhood_profile.Rmd", package = "lemur"),
    params = list(
      level = level,
      neighbourhood = neighbourhood
    ),
    output_file = glue::glue("{filename}.{format}"),
    output_format = glue::glue("{format}_document"),
    envir = new.env()
  )
}
