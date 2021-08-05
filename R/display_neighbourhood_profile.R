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

  original_data <- data[[variable]]

  original_data <- original_data %>%
    dplyr::arrange(desc(.data$group))

  data <- original_data

  # Flag if it's a proportion variable
  prop_variable <- "prop" %in% names(original_data)

  if (compare) {

    neighbourhood_name <- data %>%
      dplyr::pull(.data$neighbourhood) %>%
      unique()

    if (prop_variable) {
      city_data <- lemur::city_profile[[variable]] %>%
        dplyr::rename(toronto = .data$prop)

      data <- data %>%
        dplyr::select(-.data$neighbourhood) %>%
        dplyr::rename(neighbourhood = .data$prop)
    } else {
      city_data <- lemur::city_profile[[variable]] %>%
        dplyr::rename(toronto = .data$value)

      data <- data %>%
        dplyr::select(-.data$neighbourhood) %>%
        dplyr::rename(neighbourhood = .data$value)
    }

    data <- data %>%
      dplyr::full_join(city_data, by = "group")
  }

  data <- data %>%
    dplyr::mutate(group = str_wrap_factor(.data$group, width = width))

  if (type == "plot") {
    if (compare) {
      p <- data %>%
        echarts4r::e_chart(x = group) %>%
        echarts4r::e_bar(neighbourhood, name = neighbourhood_name, emphasis = list(itemStyle = list(color = main_colour))) %>%
        echarts4r::e_bar(toronto, name = "City of Toronto", emphasis = list(itemStyle = list(color = grey_colour))) %>%
        echarts4r::e_flip_coords() %>%
        echarts4r::e_color(color = c(main_colour, grey_colour))
    } else {
      p <- data %>%
        echarts4r::e_chart(x = group) %>%
        echarts4r::e_bar(prop, emphasis = list(itemStyle = list(color = grey_colour)), legend = FALSE) %>%
        echarts4r::e_flip_coords() %>%
        echarts4r::e_color(color = grey_colour)
    }

    if (dollar) {
      p <- p %>%
        echarts4r::e_x_axis(formatter = echarts4r::e_axis_formatter(style = "currency"))
    } else {
      p <- p %>%
        echarts4r::e_x_axis(formatter = echarts4r::e_axis_formatter(style = "percent"))
    }

    p %>%
      echarts4r::e_x_axis(
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        splitLine = list(show = FALSE)
      ) %>%
      echarts4r::e_y_axis(
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE)
      ) %>%
      echarts4r::e_animation(show = FALSE) %>%
      echarts4r::e_grid(top = ifelse(compare, "25px", "10px"), left = "75px", right = "15px", bottom = "25px")
  } else if (type == "table") {
    if (compare) {
      res <- data %>%
        dplyr::select(.data$group, .data$neighbourhood, .data$toronto)

      names(res) <- c("group", neighbourhood_name, "City of Toronto")
    } else {

      if (prop_variable) {
        res <- data %>%
          dplyr::select(.data$group, .data$prop)
      } else {
        res <- data %>%
          dplyr::select(.data$group, .data$value)
      }
    }

    if (prop_variable) {
      res <- res %>%
        dplyr::mutate(dplyr::across(-c(.data$group), scales::percent_format(accuracy = 0.1)))
    }

    res <- res %>%
      dplyr::arrange(group)

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
plot_neighbourhood_profile_distribution <- function(data, variable, binwidth, compare = TRUE, height = NULL) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = lemur::city_profile[[glue::glue("{variable}_distribution")]], ggplot2::aes(x = .data$value), fill = grey_colour, binwidth = binwidth)

  plot_data <- ggplot2::ggplot_build(p)[["data"]][[1]] %>%
    dplyr::select(.data$y, .data$x, .data$xmin, .data$xmax)

  if (compare) {
    # If we're comparing, we want to highlight the bar the neighbourhood is in
    # Rather than trying to construct the bins ourselves, use the underlying ggplot2 object which has it!
    plot_data <- plot_data %>%
      dplyr::mutate(neighbourhood = data[[variable]] >= .data$xmin & data[[variable]] < .data$xmax) %>%
      dplyr::mutate(
        neighbourhood_y = ifelse(.data$neighbourhood, y, NA_real_)
      )

    # If there is no value for the neighbourhood (e.g. in the case of RentSafeTO scores), then all y should retain their existing values and all neighbourhood_y should be NAs
    missing_neighbourhood_y <- plot_data %>%
      dplyr::filter(!is.na(.data$neighbourhood_y)) %>%
      nrow() == 0

    # Otherwise, y is all cases where neighbourhood_y is NA
    if (!missing_neighbourhood_y) {
      plot_data <- plot_data %>%
        dplyr::mutate(y = ifelse(.data$neighbourhood, NA_real_, y))
    }
  }

  p <- plot_data %>%
    echarts4r::e_chart(x = x, height = height, dispose = FALSE) %>%
    echarts4r::e_bar(serie = y, stack = "grp", emphasis = list(itemStyle = list(color = grey_colour))) %>%
    echarts4r::e_color(color = c(grey_colour, main_colour)) %>%
    echarts4r::e_x_axis(
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      splitLine = list(show = FALSE)
    ) %>%
    echarts4r::e_y_axis(
      show = FALSE
    ) %>%
    echarts4r::e_legend(show = FALSE)

  if (compare) {
    p <- p %>%
      echarts4r::e_bar(serie = neighbourhood_y, stack = "grp", emphasis = list(itemStyle = list(color = main_colour)))
  }

  x_min <- plot_data %>%
    dplyr::pull(.data$x) %>%
    min()

  # x_steps <- plot_data %>%
  #   dplyr::mutate(diff = .data$x - dplyr::lag(.data$x)) %>%
  #   dplyr::filter(!is.na(.data$diff)) %>%
  #   dplyr::pull(.data$diff) %>%
  #   unique()

  x_max <- plot_data %>%
    dplyr::pull(.data$x) %>%
    max()

  # x_max <- x_max + x_steps

  p %>%
    echarts4r::e_animation(show = FALSE) %>%
    echarts4r::e_x_axis(max = x_max) %>%
    echarts4r::e_grid(top = "10px", left = "15px", right = "15px", bottom = "25px")
}
