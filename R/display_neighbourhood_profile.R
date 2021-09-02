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
display_neighbourhood_profile <- function(data, variable, compare = TRUE, width = 20, dollar = FALSE, type = "plot", static = FALSE) {
  if (variable %in% c("amenity_density")) {
    return(display_neighbourhood_profile_horizontal(data, variable = variable, compare = compare, width = width, type = type, static = static))
  }

  original_data <- data[[variable]]

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
    data <- data %>%
      dplyr::mutate(group = forcats::fct_rev(.data$group))

    if (compare) {
      if (prop_variable) {
        data <- data %>%
          dplyr::mutate(dplyr::across(c(.data$toronto, .data$neighbourhood), .fns = list(label = ~ scales::percent(.x, accuracy = 0.1))))
      } else if (dollar) {
        data <- data %>%
          dplyr::mutate(dplyr::across(c(.data$toronto, .data$neighbourhood), .fns = list(label = scales::dollar)))
      } else {
        data <- data %>%
          dplyr::mutate(dplyr::across(c(.data$toronto, .data$neighbourhood), .fns = list(label = ~.x)))
      }

      if (static) {
        data <- data %>%
          tidyr::pivot_longer(cols = c(neighbourhood, toronto), names_to = "new_neighbourhood", values_to = "new_value") %>%
          dplyr::mutate(
            label = dplyr::case_when(
              new_neighbourhood == "toronto" ~ toronto_label,
              new_neighbourhood == "neighbourhood" ~ neighbourhood_label
            ),
            new_neighbourhood = forcats::fct_relevel(new_neighbourhood, "toronto", "neighbourhood")
          ) %>%
          dplyr::arrange(new_neighbourhood)

        p <- ggplot2::ggplot(data, ggplot2::aes(x = new_value, y = group, fill = new_neighbourhood)) +
          ggplot2::geom_col(position = ggplot2::position_dodge2()) +
          ggplot2::scale_fill_manual(values = c(grey_colour, main_colour)) +
          ggplot2::geom_text(ggplot2::aes(x = new_value, y = group, label = label), position = ggplot2::position_dodge(width = 1), hjust = -0.1, size = 2) +
          lemur::theme_lemur() +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme(legend.position = "none")
      } else {
        p <- plotly::plot_ly(data, x = ~toronto, y = ~group, type = "bar", color = I(grey_colour), hoverinfo = "skip", text = ~toronto_label, textposition = "outside", cliponaxis = FALSE, textfont = list(color = "black")) %>%
          plotly::add_trace(x = ~neighbourhood, color = I(main_colour), hoverinfo = "skip", text = ~neighbourhood_label, textposition = "outside", cliponaxis = FALSE, textfont = list(color = "black"))
      }
    } else {
      if (prop_variable) {
        data <- data %>%
          dplyr::select(-.data$value) %>%
          dplyr::rename(value = .data$prop) %>%
          dplyr::mutate(label = scales::percent(.data$value, accuracy = 0.1))
      } else if (dollar) {
        data <- data %>%
          dplyr::mutate(label = scales::dollar(.data$value))
      } else {
        data <- data %>%
          dplyr::mutate(label = .data$value)
      }

      if (static) {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = value, y = group)) +
          ggplot2::geom_col(fill = grey_colour) +
          ggplot2::geom_text(ggplot2::aes(label = label, hjust = -0.1), size = 2.5) +
          lemur::theme_lemur() +
          ggplot2::labs(x = NULL, y = NULL)
      } else {
        p <- plotly::plot_ly(data, x = ~value, y = ~group, type = "bar", color = I(grey_colour), hoverinfo = "skip", text = ~label, textposition = "outside", cliponaxis = FALSE, textfont = list(color = "black"))
      }
    }

    if (dollar) {
      if (static) {
        p <- p + ggplot2::scale_x_continuous(labels = scales::dollar, expand = ggplot2::expansion(mult = c(0, 0.2)))
      } else {
        p <- p %>% plotly::layout(xaxis = list(tickprefix = "$"))
      }
    } else {
      if (static) {
        p <- p + ggplot2::scale_x_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.15)))
      } else {
        p <- p %>% plotly::layout(xaxis = list(tickformat = "%"))
      }
    }

    if (!static) {
      p <- p %>%
        plotly::layout(
          yaxis = list(title = NA, showgrid = FALSE, fixedrange = TRUE),
          xaxis = list(title = NA, fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
          margin = list(t = 15, r = 25, b = 5, l = 25),
          showlegend = FALSE,
          font = list(family = "Lato", size = 12, color = "black")
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }
    p
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
      dplyr::arrange(.data$group)

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

display_neighbourhood_profile_horizontal <- function(data, variable, compare = TRUE, width = 20, type = "plot", static = FALSE) {
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
        dplyr::filter(.data$group != "Unknown")
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
        dplyr::relocate(.data$`City of Toronto`, .after = dplyr::last_col())

      return(res)
    } else if (type == "plot") {
      if (static) {
        plot_amenity_density(data %>%
          dplyr::mutate(neighbourhood = forcats::fct_rev(neighbourhood)), static = TRUE) +
          ggplot2::facet_wrap(dplyr::vars(neighbourhood))
      } else {
        neighbourhood_data <- data %>%
          dplyr::filter(neighbourhood != "City of Toronto")

        neighbourhood <- neighbourhood_data %>%
          dplyr::pull(neighbourhood) %>%
          unique()

        neighbourhood_plot <- plot_amenity_density(neighbourhood_data, neighbourhood, static = static)
        city_plot <- plot_amenity_density(data %>%
          dplyr::filter(neighbourhood == "City of Toronto"), "City of Toronto", static = static)

        plotly::subplot(neighbourhood_plot, city_plot, shareY = TRUE, titleX = TRUE) %>%
          plotly::layout(showlegend = FALSE)
      }
    }
  } else if (!compare) {
    if (type == "table") {
      res <- data %>%
        dplyr::select(.data$group, .data$prop) %>%
        dplyr::mutate(prop = scales::percent(.data$prop, accuracy = 0.1))

      return(res)
    } else if (type == "plot") {
      plot_amenity_density(data, static = static)
    }
  }
}

plot_amenity_density <- function(data, xaxis_title = FALSE, b = 15, static = FALSE) {
  data <- data %>%
    dplyr::mutate(label = scales::percent(.data$prop, accuracy = 0.1))

  if (static) {
    ggplot2::ggplot(data, ggplot2::aes(x = group, y = prop, fill = group)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.1, size = 2.5) +
      lemur::theme_lemur() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::scale_fill_manual(values = c(low_colour, mid_colour, high_colour))
  } else {
    plotly::plot_ly(data,
      x = ~group, y = ~prop, type = "bar", hoverinfo = "skip",
      marker = list(color = c(low_colour, mid_colour, high_colour)),
      text = ~label, textposition = "outside", cliponaxis = FALSE,
      textfont = list(color = "black")
    ) %>%
      plotly::layout(
        xaxis = list(showgrid = FALSE, title = xaxis_title, fixedrange = TRUE),
        yaxis = list(
          showgrid = FALSE, zeroline = FALSE, title = FALSE,
          tickformat = "%",
          fixedrange = TRUE
        ),
        margin = list(t = 15, r = 0, b = b, l = 15),
        font = list(family = "Lato", size = 12, color = "black")
      ) %>%
      plotly::config(displayModeBar = FALSE)
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
plot_neighbourhood_profile_distribution <- function(data, variable, binwidth, compare = TRUE, height = NULL, width = NULL, static = FALSE) {
  # Create histogram first to get underlying data and bins
  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = lemur::city_profile[[glue::glue("{variable}_distribution")]], ggplot2::aes(x = .data$value), fill = grey_colour, binwidth = binwidth)

  plot_data <- ggplot2::ggplot_build(p)[["data"]][[1]] %>%
    dplyr::select(.data$y, .data$x, .data$xmin, .data$xmax)

  if (compare) {
    # If we're comparing, we want to highlight the bar the neighbourhood is in
    # Rather than trying to construct the bins ourselves, use the underlying ggplot2 object which has it!
    plot_data <- plot_data %>%
      dplyr::mutate(
        is_neighbourhood = dplyr::case_when(
          data[[variable]] >= .data$xmin & data[[variable]] < .data$xmax ~ "yes",
          is.na(data[[variable]]) ~ "no",
          TRUE ~ "no"
        )
      )
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(is_neighbourhood = "no")
  }

  # Widen data to get yes/no columns

  plot_data <- plot_data %>%
    tidyr::pivot_wider(names_from = .data$is_neighbourhood, values_from = .data$y) %>%
    # Set NAs to 0 to avoid warning of missing values
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("yes", "no")), dplyr::coalesce, 0))

  if (static) {
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_col(ggplot2::aes(x = x, y = no), fill = grey_colour) +
      ggplot2::labs(x = NULL, y = NULL) +
      lemur::theme_lemur() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())

    if (compare & "yes" %in% names(plot_data)) {
      p <- p +
        ggplot2::geom_col(ggplot2::aes(x = x, y = yes), fill = main_colour)
    }
  } else {
    p <- plotly::plot_ly(plot_data, x = ~x, y = ~no, type = "bar", hoverinfo = "skip", color = I(grey_colour)) %>%
      plotly::layout(
        yaxis = list(title = NA, zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE, fixedrange = TRUE),
        xaxis = list(title = NA, zeroline = FALSE, fixedrange = TRUE),
        margin = list(t = 15, r = 25, b = 5, l = 25),
        barmode = "stack",
        showlegend = FALSE,
        font = list(family = "Lato", size = 12, color = "black")
      ) %>%
      plotly::config(displayModeBar = FALSE)

    if (compare & "yes" %in% names(plot_data)) {
      p <- p %>%
        plotly::add_trace(x = ~x, y = ~yes, type = "bar", hoverinfo = "skip", color = I(main_colour))
    }
  }

  p
}
