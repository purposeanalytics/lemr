create_neighbourhood_legend <- function(neighbourhood) {
  glue::glue("{color_preview(main_colour)} {neighbourhood} {color_preview(grey_colour)} City of Toronto")
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

create_square_legend <- function(colour, text, alt_text) {
  shiny::div(
    role = "img",
    `aria-label` = alt_text,
    shiny::HTML(
      purrr::map2_chr(colour, text, ~ glue::glue("{color_preview(.x)} {.y}"))
    )
  )
}

color_preview_circle <- function(color) {
  shiny::tagList(
    shiny::span(
      class = "color-preview-circle",
      style = paste("background-color:", color),
      .noWS = "outside"
    )
  )
}

create_circle_legend <- function(colour, text, alt_text) {
  shiny::div(
    role = "img",
    `aria-label` = alt_text,
    shiny::HTML(
      purrr::map2_chr(colour, text, ~ glue::glue("{color_preview_circle(.x)} {.y}"))
    )
  )
}
