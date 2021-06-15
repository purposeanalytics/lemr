theme_lemur <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      rect = ggplot2::element_rect(fill = "transparent"),
      axis.ticks.y = ggplot2::element_blank()
    )
}
