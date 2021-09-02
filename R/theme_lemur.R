#' LEMUR ggplot2 theme
#'
#' @param base_size Base font size
#' @param base_family Base font family
#'
#' @export
theme_lemur <- function(base_size = 12, base_family = "Lato") {
  if (base_family == "Lato" & !"Lato" %in% extrafont::fonts()) {
    base_family <- "" # fall back to ggplot2 default
  }

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
  ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      rect = ggplot2::element_rect(fill = "transparent"),
      axis.text.x = ggplot2::element_text(colour = "black")
    )
}
