#' LEMUR ggplot2 theme
#'
#' @export
theme_lemur <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      rect = ggplot2::element_rect(fill = "transparent")
    )
}
