#' Plot a neighbourhood profile variable
#'
#' @param data Neighbourhood profiles data for a given neighbourhood, from \link{neighbourhood_profiles}, or for the city, from \link{city_profile}
#' @param variable Variable to visualize.
#' @param compare Whether to compare to City of Toronto values. Defaults to TRUE. FALSE is useful when you want to plot *just* the values for as neighbourhood or *just* the values for the city.
#' @param width Passed along to str_wrap for wrapping y-axis labels. Defaults to a width of 20.
#' @param dollar Whether the variable shown is in dollars. Defaults to FALSE.
#' @param type Type of display, "plot" or "table". Defaults to "plot".
#'
#' @export
#'
#' @examples {
#'   neighbourhood_profiles[["Danforth"]] %>%
#'     display_neighbourhood_profile("household_size")
#'
#'   neighbourhood_profiles[["Danforth"]] %>%
#'     display_neighbourhood_profile("average_total_income")
#' }
display_neighbourhood_profile <- function(data, variable, compare = TRUE, width = 20, dollar = FALSE, type = "plot") {
  if (variable %in% c("household_tenure", "amenity_density")) {
    return(display_neighbourhood_profile_horizontal(data, variable = variable, compare = compare, width = width, type = type))
  }

  data <- data[[variable]] %>%
    dplyr::mutate(group = forcats::fct_rev(.data$group)) # Reverse factor levels so they read top to bottom

  if (compare) {
    city_data <- lemur::city_profile[[variable]] %>%
      dplyr::mutate(group = forcats::fct_relevel(.data$group, levels(data[["group"]])))

    data <- data %>%
      dplyr::bind_rows(city_data) %>%
      dplyr::mutate(neighbourhood = dplyr::coalesce(.data$neighbourhood, "City of Toronto")) %>%
      dplyr::mutate(neighbourhood = forcats::fct_relevel(.data$neighbourhood, "City of Toronto", after = 0))
  }

  data <- data %>%
    dplyr::mutate(group = str_wrap_factor(.data$group, width = width))

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

  if (type == "plot") {
    if (compare) {
      p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$group, fill = .data$neighbourhood)) +
        ggplot2::geom_col(ggplot2::aes(x = .data$value), position = ggplot2::position_dodge2(preserve = "single", width = 1)) +
        ggplot2::scale_fill_manual(values = c(grey_colour, main_colour), guide = ggplot2::guide_legend(reverse = TRUE))
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$group)) +
        ggplot2::geom_col(ggplot2::aes(x = .data$value), position = ggplot2::position_dodge2(preserve = "single", width = 1), fill = grey_colour)
    }

    if (dollar) {
      p <- p +
        ggplot2::geom_text(ggplot2::aes(x = .data$value, label = scales::dollar(.data$label)), position = ggplot2::position_dodge2(preserve = "single", width = 1), hjust = -0.1, size = 3)
    } else {
      p <- p +
        ggplot2::geom_text(ggplot2::aes(x = .data$value, label = .data$label), position = ggplot2::position_dodge2(preserve = "single", width = 1), hjust = -0.1, size = 3)
    }

    p +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.20))) +
      theme_lemur(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_blank()
      )
  } else if (type == "table") {
    data <- data %>%
      dplyr::arrange(dplyr::desc(.data$group))

    if (compare) {
      res <- data %>%
        dplyr::select(.data$group, .data$neighbourhood, .data$label) %>%
        tidyr::pivot_wider(names_from = .data$neighbourhood, values_from = .data$label)
    } else {
      res <- data %>%
        dplyr::select(.data$group, .data$label)
    }

    return(res)
  }
}

str_wrap_factor <- function(x, width) {
  if (!is.factor(x)) {
    x <- as.factor(x)
  }

  levels(x) <- stringr::str_wrap(levels(x), width = width)
  x
}

display_neighbourhood_profile_horizontal <- function(data, variable, compare = TRUE, width = 20, type = "plot") {
  data <- data[[variable]]

  if (variable == "amenity_density") {
      data <- data %>%
        dplyr::filter(.data$group != "Unknown") %>%
        dplyr::mutate(group = forcats::fct_drop(.data$group, "Unknown"))
  }

  if (compare) {
    city_data <- lemur::city_profile[[variable]]

    if (variable == "amenity_density") {
      city_data <- city_data %>%
        dplyr::filter(group != "Unknown")
    }

    data <- city_data %>%
      dplyr::mutate(neighbourhood = "City of Toronto") %>%
      dplyr::bind_rows(
        data
      ) %>%
      dplyr::mutate(
        neighbourhood = forcats::fct_relevel(.data$neighbourhood, "City of Toronto", after = 0)
      ) %>%
      dplyr::arrange(.data$group)

    if (type == "table") {
      res <- data %>%
        dplyr::select(.data$group, .data$prop, .data$neighbourhood) %>%
        dplyr::mutate(prop = scales::percent(.data$prop, accuracy = 0.1)) %>%
        tidyr::pivot_wider(names_from = .data$neighbourhood, values_from = .data$prop) %>%
        dplyr::arrange(dplyr::desc(.data$group)) %>%
        dplyr::relocate(.data$`City of Toronto`, .after = dplyr::last_col())

      return(res)
    } else if (type == "plot") {
      data <- data %>%
        dplyr::mutate(neighbourhood = str_wrap_factor(.data$neighbourhood, width = width))

      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$prop, y = .data$neighbourhood, fill = .data$group)) +
        ggplot2::geom_col() +
        theme_lemur() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          legend.position = "top"
        )
    }
  } else if (!compare) {
    if (type == "table") {
      res <- data %>%
        dplyr::arrange(dplyr::desc(.data$group)) %>%
        dplyr::select(.data$group, .data$prop) %>%
        dplyr::mutate(prop = scales::percent(.data$prop, accuracy = 0.1))

      return(res)
    } else if (type == "plot") {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$prop, y = "1", fill = .data$group)) +
        ggplot2::geom_col() +
        theme_lemur() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
    }
  }

  if (type == "plot") {
    plot_colours <- switch(variable,
      "household_tenure" = c(mid_colour, high_colour),
      "amenity_density" = rev(c(high_colour, mid_colour, low_colour))
    )

    p +
      ggplot2::scale_fill_manual(values = plot_colours, drop = TRUE) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
      ggplot2::labs(fill = NULL) +
      ggplot2::theme(legend.position = "top")
  }
}

#' Plot the distribution of a neighbourhood profile variable
#'
#' Plot the distribution of a variable, across neighbourhoods, with an optional line showing the current neighbourhood's value
#'
#' @param data Neighbourhood profiles data for a given neighbourhood, from \link{neighbourhood_profiles}.
#' @param variable Variable to visualize
#' @param binwidth Bin width for geom_histogram
#' @param compare Whether to show a line with the current neighbourhood's value. Defaults to TRUE - FALSE is useful in the City of Toronto view.
#'
#' @export
#'
#' @examples
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("population_density", binwidth = 1000)
#'
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("population_change", binwidth = 0.025)
#'
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("unaffordable_housing", binwidth = 0.025)
#'
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("average_renter_shelter_cost", binwidth = 50)
#'
#' neighbourhood_profiles[["Danforth"]] %>%
#'   plot_neighbourhood_profile_distribution("lim_at", binwidth = 0.025)
plot_neighbourhood_profile_distribution <- function(data, variable, binwidth, compare = TRUE) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = lemur::city_profile[[glue::glue("{variable}_distribution")]], ggplot2::aes(x = .data$value), fill = grey_colour, binwidth = binwidth)

  if (compare) {
    # If we're comparing, we want to highlight the bar the neighbourhood is in
    # Rather than trying to construct the bins ourselves, use the underlying ggplot2 object which has it!
    plot_data <- ggplot2::ggplot_build(p)[["data"]][[1]] %>%
      dplyr::select(.data$y, .data$x, .data$xmin, .data$xmax) %>%
      dplyr::mutate(neighbourhood = data[[variable]] >= .data$xmin & data[[variable]] < .data$xmax) %>%
      dplyr::mutate(neighbourhood = dplyr::coalesce(.data$neighbourhood, FALSE)) %>%
      tidyr::uncount(weights = .data$y)

    p <- ggplot2::ggplot() +
      ggplot2::geom_histogram(data = plot_data, ggplot2::aes(x = .data$x, fill = .data$neighbourhood), binwidth = binwidth, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c(grey_colour, main_colour))
  }

  p +
    theme_lemur() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}
