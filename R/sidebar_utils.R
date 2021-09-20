determine_dataset_from_level <- function(level, neighbourhood) {
  switch(level,
    "city" = lemur::city_aggregate,
    "neighbourhood" = lemur::neighbourhood_aggregate[[neighbourhood]]
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
  } else if (measure %in% c("population_density", "number_of_buildings", "number_of_units")) {
    scales::comma(round(data))
  } else if (measure %in% c("unaffordable_housing", "lim_at")) {
    scales::percent(data, accuracy = 0.1)
  } else if (measure == "average_renter_shelter_cost") {
    scales::dollar(data, accuracy = 1)
  } else if (measure == "apartment_building_evaluation") {
    paste0(data, "%")
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
    kableExtra::kable(format = "html", align = c("l", rep("r", ncol(res) - 1))) %>%
    kableExtra::kable_styling() %>%
    kableExtra::kable_styling(bootstrap_options = "condensed")
}

# Rental supply -----

rental_supply_plot_alt_text <- function(level, neighbourhood) {
  switch(level,
    "city" = "Bar chart showing the breakdown of the rental market supply in the City of Toronto. The data is in the table that follows.",
    "neighbourhood" = glue::glue("Bar chart showing the breakdown of the rental market supply in {neighbourhood}. The data is in the table that follows.")
  )
}

rental_supply_plot <- function(data) {
  data[["rental_supply"]] %>%
    dplyr::mutate(
      group = forcats::fct_expand(group, names(rental_supply_colors())),
      group = forcats::fct_relevel(group, names(rental_supply_colors()))
    ) %>%
    plotly::plot_ly(x = ~prop, y = 1, color = ~group, type = "bar", orientation = "h", hoverinfo = "skip", colors = rental_supply_colors()) %>%
    plotly::layout(barmode = "stack") %>%
    plotly::layout(
      yaxis = list(title = NA, showgrid = FALSE, showticklabels = FALSE, fixedrange = TRUE),
      xaxis = list(title = NA, fixedrange = TRUE, showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(0, 1)),
      margin = list(t = 5, r = 5, b = 5, l = 5),
      showlegend = FALSE,
      font = list(family = "Lato", size = 12, color = "black")
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

rental_supply_colors <- function() {
  stats::setNames(c("#27a167", "#2ded92", "#0642a1", "#1569ed", "#f53216", "#f77460"), c("Apartment", "Non-Apartment", "Condo", "Non-Condo", "Toronto Community Housing", "Other Non-Market"))
}

rental_supply_table <- function(data, market) {
  totals_name <- ifelse(market == "Non-market", "Non-market units:", glue::glue("{market} market units"))

  data <- data[["rental_supply"]] %>%
    dplyr::filter(market == !!market) %>%
    dplyr::select(group, value, prop)

  layer_order <- names(rental_supply_colors())[names(rental_supply_colors()) %in% data[["group"]]]

  data %>%
    dplyr::mutate(group_order = forcats::fct_relevel(group, layer_order)) %>%
    dplyr::mutate(group = purrr::map_chr(group, function(x) {
      create_square_legend(rental_supply_colors()[[x]], paste0(x, ":"), glue::glue("A legend showing the color that represents {x} rentals in the above plot.")) %>% as.character()
    })) %>%
    janitor::adorn_totals(name = totals_name, fill = totals_name) %>%
    dplyr::mutate(
      group_order = forcats::fct_relevel(group_order, totals_name, layer_order),
      value = scales::comma(value),
      percent = scales::percent(prop, accuracy = 0.1),
      value_percent = glue::glue("{value}{space}({percent})",
        space = ifelse(prop < 0.1, " &nbsp; &nbsp;", " ")
      )
    ) %>%
    dplyr::arrange(group_order) %>%
    dplyr::select(group, value_percent) %>%
    knitr::kable(col.names = NULL, align = "lr", escape = FALSE) %>%
    kableExtra::kable_minimal(
      html_font = "\"Lato\", sans-serif",
      full_width = TRUE
    ) %>%
    kableExtra::row_spec(row = 1, bold = TRUE)
}

rental_supply_primary_table <- function(data) {
  rental_supply_table(data, "Primary")
}

rental_supply_secondary_table <- function(data) {
  rental_supply_table(data, "Secondary")
}

rental_supply_non_market_table <- function(data) {
  rental_supply_table(data, "Non-market")
}

# Number of apartments ----

number_of_apartments_number <- function(number_of_apartments_formatted, number_of_units_formatted) {
  glue::glue("Apartment buildings: {number_of_apartments_formatted} ({number_of_units_formatted} units)")
}

number_of_apartments_description <- function(level, neighbourhood, number_of_apartments, number_of_apartments_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["number_of_buildings_distribution"]][["value"]])
    value_percentile <- value_distribution(number_of_apartments)
  }

  # Change the level to change the description if there are 0 apartments - doesn't make sense to report a percentile.
  if (number_of_apartments == 0) {
    level <- "neighbourhood_zero"
  }

  switch(level,
    "city" = "Distribution of number of apartment buildings for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of number of apartment buildings for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {number_of_apartments_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'."),
    "neighbourhood_zero" = glue::glue("Distribution of number of apartment buildings for each of the City of Toronto neighbourhoods. There are zero apartment buildings in {neighbourhood}.")
  )
}

number_of_apartments_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["number_of_buildings_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of number of apartment buildings for each of Toronto's neighbourhoods. The values range from {min} to {max} apartment buildings and the distribution is heavily skewed left with most values between {skew_min} and {skew_max}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the number of apartment buildings in {neighbourhood}'s is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

number_of_apartments_plot <- function(data, compare, static = FALSE) {
  data %>%
    plot_neighbourhood_profile_distribution("number_of_buildings", compare = compare, binwidth = 5, static = static)
}

number_of_units_description <- function(level, neighbourhood, number_of_units, number_of_units_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["number_of_units_distribution"]][["value"]])
    value_percentile <- value_distribution(number_of_units)
  }

  # Change the level to change the description if there are 0 apartments - doesn't make sense to report a percentile.
  if (number_of_units == 0) {
    level <- "neighbourhood_zero"
  }

  switch(level,
    "city" = "Distribution of number of units in apartment buildings for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of number of units in apartment buildings for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {number_of_units_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'."),
    "neighbourhood_zero" = glue::glue("Distribution of number of units in apartment buildings for each of the City of Toronto neighbourhoods. There are zero apartment building units in {neighbourhood}.")
  )
}

number_of_units_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["number_of_units_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of number of units inapartment buildings for each of Toronto's neighbourhoods. The values range from {min} to {max} units and the distribution is heavily skewed left with most values between {skew_min} and {skew_max}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the number of units in apartment buildings in {neighbourhood}'s is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

number_of_units_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("number_of_units", compare = compare, binwidth = 250, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(labels = scales::comma)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = ",d"))
  }
}

# Apartment building evaluation (RentSafeTO) ----

apartment_building_evaluation_number <- function(apartment_building_evaluation_formatted) {
  if (is.na(apartment_building_evaluation_formatted)) {
    return("There are no apartment buildings in this neighbourhood, so no RentSafeTO scores to report.")
  }
  glue::glue("Median RentSafeTO evaluation score: {apartment_building_evaluation_formatted}")
}

apartment_building_evaluation_description <- function(level, neighbourhood, apartment_building_evaluation, apartment_building_evaluation_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["apartment_building_evaluation_distribution"]][["value"]])
    value_percentile <- value_distribution(apartment_building_evaluation)
  }

  # Switch level to "city" if there are no buildings (therefore no scores) in the neighbourhood.
  if (is.na(apartment_building_evaluation)) {
    level <- "city"
  }

  switch(level,
    "city" = "Distribution of median RentSafeTO evaluation score for each of the City of Toronto neighbourhoods with apartment buildings.",
    "neighbourhood" = glue::glue("Distribution of median RentSafeTO evaluation score for each of the City of Toronto neighbourhoods with apartment buildings. The value for {neighbourhood}, {apartment_building_evaluation_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

apartment_building_evaluation_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["apartment_building_evaluation_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of median RentSafeTO evaluation score for each of Toronto's neighbourhoods that have apartment buildings. The values range from {min}% to {max}% and the distribution is normally distributed with most values between {skew_min}% and {skew_max}%.",
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    skew_min = stats::quantile(values, 0.1, na.rm = TRUE),
    skew_max = stats::quantile(values, 0.9, na.rm = TRUE)
  )

  # Switch level to "city" if there are no buildings (therefore no scores) in the neighbourhood.
  if (level == "neighbourhood") {
    if (is.na(lemur::neighbourhood_aggregate[[neighbourhood]][["apartment_building_evaluation"]])) {
      level <- "city"
    }
  }

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the median RentSafeTO score in {neighbourhood} is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

apartment_building_evaluation_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("apartment_building_evaluation", compare = compare, binwidth = 2, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"))
  } else {
    p %>%
      plotly::layout(xaxis = list(range = c(0, 100), ticksuffix = "%"))
  }
}

# Amenity density -----

amenity_density_description <- function(level, neighbourhood) {
  switch(level,
    "city" = glue::glue("Breakdown of population living in high, medium, and low amenity density areas in the City of Toronto."),
    "neighbourhood" = glue::glue("Comparison of population living in high, medium, and low amenity density areas in {neighbourhood} versus in the City of Toronto.")
  )
}

amenity_density_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level, neighbourhood, "amenity density by population")
}

amenity_density_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("amenity_density", compare = compare, width = 25, static = static)
}


# Population change ----

population_change_number <- function(population_change_formatted) {
  glue::glue("2011 to 2016: {population_change_formatted}")
}

population_change_description <- function(level, neighbourhood, population_change, population_change_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["population_change_distribution"]][["value"]])
    value_percentile <- value_distribution(population_change)
  }

  switch(level,
    "city" = "Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_change_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population change.")
  )
}

population_change_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["population_change_distribution"]][["value"]]

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

population_change_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("population_change", compare = compare, binwidth = 0.01, static = static)

  if (static) {
    p + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# Population density ----

population_density_number <- function(data) {
  glue::glue("{data} people per square kilometre")
}

population_density_description <- function(level, neighbourhood, population_density, population_density_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["population_density_distribution"]][["value"]])
    value_percentile <- value_distribution(population_density)
  }

  switch(level,
    "city" = "Distribution of population density for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population density for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_density_formatted} people per square kilometre, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population density.")
  )
}

population_density_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["population_density_distribution"]][["value"]]

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

population_density_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("population_density", compare = compare, binwidth = 1000, static = static)

  if (static) {
    p + ggplot2::scale_x_continuous(labels = scales::comma)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = ",d"))
  }
}

# Household size ----

household_size_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level, neighbourhood, "household sizes")
}

household_size_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level, neighbourhood, "household sizes")
}

household_size_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("household_size", width = 10, compare = compare, static = static)
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

average_total_household_income_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    display_neighbourhood_profile("average_total_income", width = 10, dollar = TRUE, compare = compare, static = static)

  if (!static) {
    p %>%
      plotly::layout(xaxis = list(tickformat = ",d"))
  } else {
    p
  }
}

# Unaffordable housing ----

unaffordable_housing_number <- function(unaffordable_housing_formatted) {
  glue::glue("Percent of tenants with unaffordable housing: {unaffordable_housing_formatted}")
}

unaffordable_housing_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {scales::percent(lemur::city_aggregate[["unaffordable_housing"]], accuracy = 0.1)})')
  } else {
    NULL
  }
}

unaffordable_housing_description <- function(level, neighbourhood, unaffordable_housing, unaffordable_housing_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["unaffordable_housing_distribution"]][["value"]])
    value_percentile <- value_distribution(unaffordable_housing)
  }

  switch(level,
    "city" = "Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of tenants with unaffordable housing for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {unaffordable_housing_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' percent of tenants with unaffordable housing.")
  )
}

unaffordable_housing_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["unaffordable_housing_distribution"]][["value"]]

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

unaffordable_housing_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("unaffordable_housing", compare = compare, binwidth = 0.025, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# LIM-AT

lim_at_number <- function(data) {
  glue::glue("Percent of people under LIM-AT: {data}")
}

lim_at_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {format_measure(lemur::city_aggregate[["lim_at"]], "lim_at")})')
  } else {
    NULL
  }
}

lim_at_description <- function(level, neighbourhood, lim_at, lim_at_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["lim_at_distribution"]][["value"]])
    value_percentile <- value_distribution(lim_at)
  }

  switch(level,
    "city" = "Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {lim_at_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

lim_at_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["lim_at_distribution"]][["value"]]

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

lim_at_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("lim_at", compare = compare, binwidth = 0.025, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1))
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# Visible minority

visible_minority_number <- function(data) {
  prop <- data[["visible_minority"]] %>%
    dplyr::filter(.data$group != "Not a visible minority") %>%
    dplyr::pull(.data$prop) %>%
    sum() %>%
    scales::percent(accuracy = 0.1)

  glue::glue("Visible minority population: {prop}")
}

visible_minority_city <- function(level) {
  if (level == "neighbourhood") {
    city_prop <- lemur::city_aggregate[["visible_minority"]] %>%
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

visible_minority_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("visible_minority", width = 20, compare = compare, static = static)
}

# Structure type ----

structure_type_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "structure type")
}

structure_type_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "housing structure type")
}

structure_type_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("structure_type", compare = compare, static = static)
}

# Bedrooms ----

bedrooms_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "number of bedrooms")
}

bedrooms_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "number of bedrooms")
}

bedrooms_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("bedrooms", compare = compare, static = static)
}

# Household tenure -----

household_tenure_description <- function(level, neighbourhood) {
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)")
}

household_tenure_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)")
}

household_tenure_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("household_tenure", compare = compare, width = 25, static = static)
}

# Shelter cost -----

shelter_cost_number <- function(shelter_cost_formatted) {
  glue::glue("Average monthly rent: {shelter_cost_formatted}")
}

shelter_cost_city <- function(level) {
  if (level == "neighbourhood") {
    glue::glue('(City of Toronto: {scales::dollar(lemur::city_aggregate[["average_renter_shelter_cost"]], accuracy = 1)})')
  } else {
    NULL
  }
}

average_renter_shelter_cost_description <- function(level, neighbourhood, shelter_cost, shelter_cost_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemur::city_aggregate[["average_renter_shelter_cost_distribution"]][["value"]])
    value_percentile <- value_distribution(shelter_cost)
  }

  switch(level,
    "city" = "Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {shelter_cost_formatted} per month, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' average rent.")
  )
}

average_renter_shelter_cost_plot_alt_text <- function(level, neighbourhood) {
  values <- lemur::city_aggregate[["average_renter_shelter_cost_distribution"]][["value"]]

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

average_renter_shelter_cost_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("average_renter_shelter_cost", compare = compare, binwidth = 50, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(labels = scales::dollar)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickprefix = "$", tickformat = ",d"))
  }
}
