
### Data -----

# Variable: Total - Age groups and average age of the population - 100% data

age_pyramid_by_neighbourhood <- census_profiles_toronto_cts %>%
  keep_most_detailed_dimension("Total - Age groups and average age of the population - 100% data") %>%
  select(ct = geo_code, neighbourhood, dimension, total, female, male) %>%
  group_by(neighbourhood, dimension) %>%
  summarise(across(c(total, male, female), sum, na.rm = TRUE), .groups = "drop")

# Calculate proportion for female / male

age_pyramid_by_neighbourhood_prop <- age_pyramid_by_neighbourhood %>%
  group_by(neighbourhood) %>%
  mutate(
    full_total = sum(total),
    across(c(total, male, female), ~ .x / full_total)
  ) %>%
  ungroup() %>%
  select(neighbourhood, dimension, total, male, female) %>%
  mutate(metric = "proportion")

# Convert to long and combine

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  pivot_longer(cols = c(total, male, female), names_to = "sex", values_to = "value") %>%
  mutate(metric = "count")

age_pyramid_by_neighbourhood_prop <- age_pyramid_by_neighbourhood_prop %>%
  pivot_longer(cols = c(total, male, female), names_to = "sex", values_to = "value")

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  bind_rows(age_pyramid_by_neighbourhood_prop) %>%
  select(neighbourhood, dimension, sex, metric, value) %>%
  arrange(neighbourhood, dimension, sex, metric, value)

rm(age_pyramid_by_neighbourhood_prop)

# Clean up and reorder dimension
# e.g. we want "0 - 4" instead of "0 to 4 years", and need to make sure that the age groups in order

age_pyramid_by_neighbourhood <- age_pyramid_by_neighbourhood %>%
  separate(dimension, into = c("discard", "max_age"), sep = " to ", fill = "left", remove = FALSE) %>%
  separate(dimension, into = "min_age", sep = " ", extra = "drop", remove = FALSE, convert = TRUE) %>%
  select(-discard) %>%
  mutate(
    max_age = parse_number(max_age),
    age_group = case_when(
      min_age < max_age ~ glue::glue("{min_age}-{max_age}"),
      min_age == max_age & min_age == 100 ~ glue::glue("100+")
    ),
    age_group = fct_reorder(age_group, min_age)
  ) %>%
  select(-min_age, -max_age) %>%
  mutate(sex = fct_relevel(sex, "male", "female"))

# Functions -----

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

# Shiny -----

shiny::plotOutput(ns("age_pyramid"))

output$age_pyramid <- shiny::renderPlot(
  {
    neighbourhood_profile[["age_pyramid"]] %>%
      plot_age_pyramid()
  },
  bg = "transparent",
  res = 96,
  height = 300
)
