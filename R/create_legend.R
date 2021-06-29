create_legend <- function(neighbourhood) {
  glue::glue('{color_preview("green")} {neighbourhood} {color_preview("grey")} City of Toronto')
}

color_preview <- function(color) {
  shiny::tagList(
    shiny::span(
      class = "color-preview",
      style = paste("background-color:", color),
      .noWS = "outside"
    )
  )
}
