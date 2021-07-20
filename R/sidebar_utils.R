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
  } else if (measure %in% c("unaffordable_housing", "lim_at")) {
    scales::percent(data, accuracy = 0.1)
  } else if (measure == "average_renter_shelter_cost") {
    scales::dollar(data, accuracy = 1)
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

generate_table <- function(data, measure, compare, first_column_name, rest_column_names, format = "none") {
  res <- data %>%
    display_neighbourhood_profile(measure, compare = compare, type = "table")

  if (format == "dollar") {
    res <- res %>%
      dplyr::mutate_at(dplyr::vars(-.data$group), ~ scales::dollar(.x, accuracy = 1))
  }

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

# Average total household income ----

average_total_household_income_description <- function(level, neighbourhood) {
  switch(level,
    "city" = "Average total income for 1 person versus 2+ person households in the City of Toronto.",
    "neighbourhood" = glue::glue("Comparison of average total income for 1 person versus 2+ person households in {neighbourhood} versus in the City of Toronto.")
  )
}

average_total_household_income_plot_alt_text <- function(level, neighbourhood) {
  switch(level,
    "city" = "Bar chart comparing average total income for 1 person versus 2+ person households in the City of Toronto. The data is in the table that follows.",
    "neighbourhood" = glue::glue("Bar chart comparing average total income for 1 person versus 2+ person households in {neighbourhood} versus in the City of Toronto. The data is in the table that follows.")
  )
}

average_total_household_income_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE, compare = compare)
}

# Unaffordable housing ----

unaffordable_housing_number <- function(unaffordable_housing_formatted) {
  glue::glue("Percent of tenants with unaffordable housing: {unaffordable_housing_formatted}")
}

unaffordable_housing_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {scales::percent(lemur::city_profile[["unaffordable_housing"]], accuracy = 0.1)})')
  } else {
    NULL
  }
}

unaffordable_housing_description <- function(level, neighbourhood, unaffordable_housing, unaffordable_housing_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_profile[["unaffordable_housing_distribution"]][["value"]])
    value_percentile <- value_distribution(unaffordable_housing)
  }

  switch(level,
    "city" = "Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {unaffordable_housing_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' percent of tenants with unaffordable housing.")
  )
}

unaffordable_housing_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_profile[["unaffordable_housing_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of percent of tenants with unaffordable housing for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} of tenants with unaffordable housing with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s percent of tenants with unaffordable housing is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

unaffordable_housing_plot <- function(data, compare) {
  data %>%
    plot_neighbourhood_profile_distribution("unaffordable_housing", compare = compare, binwidth = 0.025) +
    ggplot2::scale_x_continuous(labels = scales::label_percent())
}

# LIM-AT

lim_at_number <- function(data) {
  glue::glue("Percent of people under LIM-AT: {data}")
}

lim_at_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {format_measure(lemur::city_profile[["lim_at"]], "lim_at")})')
  } else {
    NULL
  }
}

lim_at_description <- function(level, neighbourhood, lim_at, lim_at_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_profile[["lim_at_distribution"]][["value"]])
    value_percentile <- value_distribution(lim_at)
  }

  switch(level,
    "city" = "Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {lim_at_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

lim_at_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_profile[["lim_at_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s value is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

lim_at_plot <- function(data, compare) {
  data %>%
    plot_neighbourhood_profile_distribution("lim_at", compare = compare, binwidth = 0.025) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1))
}

# Visible minority

visible_minority_number <- function(data) {
  prop <- data[["visible_minority"]] %>%
    dplyr::filter(.data$group != "Not a visible minority") %>%
    dplyr::pull(.data$prop) %>%
    sum() %>%
    scales::percent(accuracy = 0.1)

  glue::glue("Visible Minority Population: {prop}")
}

visible_minority_city <- function(level) {
  if (level == "neighbourhood") {
    city_prop <- lemur::city_profile[["visible_minority"]] %>%
      dplyr::filter(.data$group != "Not a visible minority") %>%
      dplyr::pull(.data$prop) %>%
      sum() %>%
      scales::percent(accuracy = 0.1)

    glue::glue("(City of Toronto: {city_prop})")
  } else {
    NULL
  }
}

visible_minority_description <- function(level, neighbourhood) {
  switch(level,
    "city" = "Breakdown of visible minority groups in the City of Toronto.",
    "neighbourhood" = glue::glue("Comparison of visible minority groups in {neighbourhood} versus in the City of Toronto.")
  )
}

visible_minority_plot_alt_text <- function(level, neighbourhood) {
  switch(level,
    "city" = "Bar chart showing the breakdown of visible minority groups in the City of Toronto. The data is in the table that follows.",
    "neighbourhood" = glue::glue("Bar chart comparing the breakdown of visible minority groups in {neighbourhood} versus in the City of Toronto. The data is in the table that follows.")
  )
}

visible_minority_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("visible_minority", width = 20, compare = compare) +
    ggplot2::labs(caption = 'Note: "n.i.e." = not included elsewhere')
}

# Structure type ----

structure_type_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "structure type")
}

structure_type_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "housing structure type")
}

structure_type_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("structure_type", compare = compare)
}

# Bedrooms ----

bedrooms_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "number of bedrooms")
}

bedrooms_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "number of bedrooms")
}

bedrooms_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("bedrooms", compare = compare)
}

# Household tenure -----

household_tenure_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)")
}

household_tenure_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)")
}

household_tenure_plot <- function(data, compare) {
  data %>%
    display_neighbourhood_profile("household_tenure", compare = compare, width = 10)
}

# Shelter cost -----

shelter_cost_number <- function(shelter_cost_formatted) {
  glue::glue("Average monthly rent: {shelter_cost_formatted}")
}

shelter_cost_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {scales::dollar(lemur::city_profile[["average_renter_shelter_cost"]], accuracy = 1)})')
  } else {
    NULL
  }
}

average_renter_shelter_cost_description <- function(level, neighbourhood, shelter_cost, shelter_cost_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_profile[["average_renter_shelter_cost_distribution"]][["value"]])
    value_percentile <- value_distribution(shelter_cost)
  }

  switch(level,
    "city" = "Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {shelter_cost_formatted} per month, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' average rent.")
  )
}

average_renter_shelter_cost_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_profile[["average_renter_shelter_cost_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of average renter shelter cost for each of Toronto's neighbourhoods. The values range from {scales::dollar(min, accuracy = 1)} to {scales::dollar(max, accuracy = 1)} with most values between {scales::dollar(skew_min, accuracy = 1)} and {scales::dollar(skew_max, accuracy = 1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s average monthly rent is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

average_renter_shelter_cost_plot <- function(data, compare) {
  data %>%
    plot_neighbourhood_profile_distribution("average_renter_shelter_cost", compare = compare, binwidth = 50) +
    ggplot2::scale_x_continuous(labels = scales::dollar)
}
