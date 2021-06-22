#' Plot a neighbourhood profile variable
#'
#' @param data Neighbourhood profiles data for a given neighbourhood, from \link{neighbourhood_profiles}, or for the city, from \link{city_profile}
#' @param variable Variable to visualize.
#' @param compare Whether to compare to City of Toronto values. Defaults to TRUE. FALSE is useful when you want to plot *just* the values for as neighbourhood or *just* the values for the city.
#' @param width Passed along to str_wrap for wrapping y-axis labels. Defaults to a width of 20.
#' @param dollar Whether the variable shown is in dollars. Defaults to FALSE.
#'
#' @export
#'
#' @examples {
#'   neighbourhood_profiles[["Danforth"]] %>%
#'     plot_neighbourhood_profile("household_size")
#'
#'   neighbourhood_profiles[["Danforth"]] %>%
#'     plot_neighbourhood_profile("average_total_income")
#' }
plot_neighbourhood_profile <- function(data, variable, compare = TRUE, width = 20, dollar = FALSE) {
  if (variable == "household_tenure") {
    return(plot_neighbourhood_household_tenure(data, compare = compare))
  }

  data <- data[[variable]] %>%
    dplyr::mutate(group = forcats::fct_rev(.data$group)) # Reverse factor levels so they read top to bottom

  if (compare) {
    city_data <- lemur::city_profile[[variable]] %>%
      dplyr::mutate(group = forcats::fct_relevel(.data$group, levels(data[["group"]])))

    data <- data %>%
      dplyr::bind_rows(city_data) %>%
      dplyr::mutate(neighbourhood = dplyr::coalesce(.data$neighbourhood, "Toronto")) %>%
      dplyr::mutate(neighbourhood = forcats::fct_relevel(.data$neighbourhood, "Toronto", after = 0)) %>%
      dplyr::mutate(group = str_wrap_factor(.data$group, width = width))
  }

  # Flag if it's a proportion variable
  prop_variable <- "prop" %in% names(data)

  if (prop_variable) {
    data <- data %>%
      dplyr::mutate(
        label = scales::percent(.data$prop, accuracy = 0.1),
        value = .data$prop
      )
  } else {
    data <- data %>%
      dplyr::mutate(label = .data$value)
  }

  if (compare) {
    p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$group, fill = .data$neighbourhood)) +
      ggplot2::geom_col(ggplot2::aes(x = .data$value), position = ggplot2::position_dodge2(preserve = "single", width = 1)) +
      ggplot2::scale_fill_manual(values = c("grey", "darkgreen"))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$group)) +
      ggplot2::geom_col(ggplot2::aes(x = .data$value), position = ggplot2::position_dodge2(preserve = "single", width = 1), fill = "grey")
  }

  if (dollar) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x = .data$value, label = scales::dollar(.data$label)), position = ggplot2::position_dodge2(preserve = "single", width = 1), hjust = -0.1, size = 3)
  } else {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x = .data$value, label = .data$label), position = ggplot2::position_dodge2(preserve = "single", width = 1), hjust = -0.1, size = 3)
  }

  p +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.20))) +
    theme_lemur(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_blank()
    )
}

str_wrap_factor <- function(x, width) {
  if (!is.factor(x)) {
    x <- as.factor(x)
  }

  levels(x) <- stringr::str_wrap(levels(x), width = width)
  x
}

plot_neighbourhood_household_tenure <- function(data, compare = TRUE) {
  if (compare) {
    data <- lemur::city_profile[["household_tenure"]] %>%
      dplyr::mutate(neighbourhood = "City of Toronto") %>%
      dplyr::bind_rows(
        data[["household_tenure"]]
      ) %>%
      dplyr::mutate(
        neighbourhood_tenure = glue::glue("{.data$neighbourhood}_{.data$group}"),
        neighbourhood = forcats::fct_relevel(.data$neighbourhood, "City of Toronto", after = 0)
      )

    ggplot2::ggplot(data, ggplot2::aes(x = .data$prop, y = .data$neighbourhood, fill = .data$neighbourhood_tenure)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c("#4c924c", "darkgreen", "lightgrey", "grey")) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      theme_lemur() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  } else {
    ggplot2::ggplot(data[["household_tenure"]], ggplot2::aes(x = .data$prop, y = "1", fill = .data$group)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c("lightgrey", "grey")) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      theme_lemur() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      )
  }
}

#' Plot the distribution of a neighbourhood profile variable
#'
#' Plot the distribution of a variable, across neighbourhoods, with an optional line showing the current neighbourhood's value
#'
#' @param data Neighbourhood profiles data for a given neighbourhood, from \link{neighbourhood_profiles}.
#' @param variable Variable to visualize
#' @param compare Whether to show a line with the current neighbourhood's value. Defaults to TRUE - FALSE is useful in the City of Toronto view.
#'
#' @export
#'
#' @examples
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("population_density")
plot_neighbourhood_profile_distribution <- function(data, variable, compare = TRUE) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_density(data = lemur::city_profile[[glue::glue("{variable}_distribution")]], ggplot2::aes(x = .data$value), fill = "grey", color = "grey") +
    theme_lemur() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  if (compare) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = data[[variable]]), color = "darkgreen")
  }

  p
}
