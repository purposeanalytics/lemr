determine_dataset_from_level <- function(level, neighbourhood) {
  switch(level,
    "city" = lemur::city_profile,
    "neighbourhood" = lemur::neighbourhood_profiles[[neighbourhood]]
  )
}

get_measure <- function(data, measure) {
  data[[measure]]
}

format_measure <- function(data, measure) {
  if (measure == "population_change") {
    pop_change_percent <- data %>%
      abs() %>%
      scales::percent(accuracy = 0.1)

    sign <- ifelse(data > 0, "+", "-")

    glue::glue("{sign}{pop_change_percent}")
  } else if (measure == "population_density") {
    scales::comma(round(data))
  }
}

generate_bar_chart_description <- function(level, neighbourhood, text) {
  switch(level,
    "city" = glue::glue("Distribution of {text} for all households in the City of Toronto."),
    "neighbourhood" = glue::glue("Comparison of {text} for households in {neighbourhood} versus all households in the City of Toronto.")
  )
}

generate_bar_chart_alt_text <- function(level, neighbourhood, text) {
  switch(level,
    "city" = glue::glue("Bar chart showing distribution of {text} for all households in the City of Toronto. The data is in the table that follows."),
    "neighbourhood" = glue::glue("Bar chart comparing {text} for households in {neighbourhood} versus all households in the City of Toronto. The data is in the table that follows.")
  )
}

generate_table <- function(data, measure, compare, first_column_name, rest_column_names) {
  res <- data %>%
    display_neighbourhood_profile(measure, compare = compare, type = "table")

  if (!compare) {
    names(res) <- c(first_column_name, rest_column_names)
  } else {
    names(res)[[1]] <- first_column_name
  }

  res %>%
    kableExtra::kable(align = c("l", rep("r", ncol(res) - 1))) %>%
    kableExtra::kable_styling()
}


# Population change ----

population_change_number <- function(population_change_formatted) {
  glue::glue("2011 to 2016: {population_change_formatted}")
}

population_change_description <- function(level, neighbourhood, population_change, population_change_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_profile[["population_change_distribution"]][["value"]])
    value_percentile <- value_distribution(population_change)
  }

  switch(level,
    "city" = "Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_change_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population change.")
  )
}

population_change_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_profile[["population_change_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of population change from 2011 to 2016 for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} population change and the distribution is heavily skewed left with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s population change is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

population_change_plot <- function(data, compare) {
  data %>%
    plot_neighbourhood_profile_distribution("population_change", compare = compare, binwidth = 0.01) +
    ggplot2::scale_x_continuous(labels = scales::label_percent())
}

# Population density ----

population_density_number <- function(data) {
  glue::glue("{data} people per square kilometre")
}

population_density_description <- function(level, neighbourhood, population_density, population_density_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_profile[["population_density_distribution"]][["value"]])
    value_percentile <- value_distribution(population_density)
  }

  switch(level,
    "city" = "Distribution of population density for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population density for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_density_formatted} people per square kilometre, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population density.")
  )
}

population_density_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_profile[["population_density_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of population density for each of Toronto's neighbourhoods. The values range from {round(min)} to {round(max)} people per square kilometer and the distribution is heavily skewed left with most values between {round(skew_min)} and {round(skew_max)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s population density is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

population_density_plot <- function(data, compare) {
  data %>%
    plot_neighbourhood_profile_distribution("population_density", compare = compare, binwidth = 1000) +
    ggplot2::scale_x_continuous(labels = scales::comma)
}

# Household size ----

household_size_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level, neighbourhood, "household sizes")
}

household_size_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level, neighbourhood, "household sizes")
}

household_size_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("household_size", width = 10, compare = compare)
}
