#' Plot household size
#'
#' @param data Input household size data for a neighbourhood, from \link{neighbourhood_profiles}.
#'
#' @export
#'
#' @examples {
#' neighbourhood_profiles[["Danforth"]][["household_size"]] %>%
#'   plot_household_size()
#' }
plot_household_size <- function(data) {
  data_labels <- data %>%
    dplyr::mutate(
      prop_label = scales::percent(prop),
      label_placement = prop + 0.015
    )

  ggplot2::ggplot(data, ggplot2::aes(x = .data$prop, y = .data$dimension)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(data = data_labels, mapping = ggplot2::aes(x = .data$label_placement, label = .data$prop_label), size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = NULL)
}
