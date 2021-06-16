#' Plot a neighbourhood profile variable
#'
#' @param data Neighbourhood profiles data for a given neighbourhood, from \link{neighbourhood_profiles}.
#' @param variablle Variable to visualize.
#'
#' @export
#'
#' @examples {
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile("household_size")
#'
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile("average_total_income")
#' }
plot_neighbourhood_profile <- function(data, variable) {
  data <- data[[variable]]

  # Flag if it's a proportion variable
  prop_variable <- "prop" %in% names(data)

  if (prop_variable) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$prop, y = .data$group)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$value, y = .data$group)) +
      ggplot2::geom_col()
  }

  p +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_lemur()
}
