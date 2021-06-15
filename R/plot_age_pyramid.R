#' Plot age pyramid
#'
#' @param data Input age pyramid data for a neighbourhood, from \link{neighbourhood_profiles}.
#' @param horizontal Whether the plot should be horizontal (with age groups on the x-axis and proportions on the y-axis). Defaults to \code{FALSE} (age groups on the y-axis and proportions on the x-axis).
#'
#' @export
#'
#' @examples {
#'   neighbourhood_profiles[["Danforth"]][["age_pyramid"]] %>%
#'     plot_age_pyramid()
#' }
plot_age_pyramid <- function(data, horizontal = FALSE) {
  prop_data <- data %>%
    dplyr::filter(
      .data$sex %in% c("female", "male"),
      .data$metric == "proportion"
    ) %>%
    dplyr::mutate(
      value_label = scales::percent(.data$value, accuracy = 0.1),
      value = dplyr::case_when(
        sex == "male" ~ -.data$value,
        sex == "female" ~ .data$value
      ),
      label_placement = dplyr::case_when(
        sex == "male" ~ .data$value - 0.005,
        sex == "female" ~ .data$value + 0.005
      )
    )

  percent_max <- prop_data %>%
    dplyr::pull(.data$value) %>%
    # dplyr::pull(.data$label_placement) %>%
    abs() %>%
    max()

  percent_range <- c(-percent_max, percent_max)
  percent_range_breaks <- pretty(percent_range)
  percent_range_labels <- percent_range_breaks %>%
    abs() %>%
    scales::percent()

  p <- ggplot2::ggplot(prop_data, ggplot2::aes(y = age_group, x = value, fill = sex)) +
    ggplot2::geom_col(show.legend = FALSE) +
    # ggplot2::geom_text(ggplot2::aes(label = value_label, x = label_placement), size = 3) +
    ggplot2::scale_x_continuous(
      limits = percent_range,
      breaks = percent_range_breaks,
      labels = percent_range_labels
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    lemur::theme_lemur()

  if (horizontal) {
    p <- p +
      ggplot2::coord_flip()
  } else {
    x_height <- length(levels(data[["age_group"]])) + 1
    p <- p +
      ggplot2::annotate("text",
        x = percent_range / 2,
        y = x_height,
        label = c("Male", "Female")
      ) +
      ggplot2::coord_cartesian(clip = "off", ylim = c(1, x_height))
  }

  p
}
