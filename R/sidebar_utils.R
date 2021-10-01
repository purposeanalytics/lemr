determine_dataset_from_level <- function(level, neighbourhood) {
  switch(level,
    "city" = lemr::city_aggregate,
    "neighbourhood" = lemr::neighbourhood_aggregate[[neighbourhood]]
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
  } else if (measure %in% c("unaffordable_housing", "lim_at", "evictions", "core_housing_need", "vacancy_rate")) {
    scales::percent(data, accuracy = 0.1)
  } else if (measure == "average_renter_shelter_cost") {
    scales::dollar(data, accuracy = 1)
  } else if (measure == "apartment_building_evaluation") {
    paste0(data, "%")
  }
}

generate_bar_chart_description <- function(level, neighbourhood, text, renter = TRUE) {
  switch(level,
    "city" = glue::glue("Distribution of {text} for all{renter} households in the City of Toronto.", renter = ifelse(renter, " renter", "")),
    "neighbourhood" = glue::glue("Comparison of {text} for{renter} households in {neighbourhood} versus all{renter} households in the City of Toronto.", renter = ifelse(renter, " renter", ""))
  )
}

generate_bar_chart_alt_text <- function(level, neighbourhood, text, renter = TRUE) {
  switch(level,
    "city" = glue::glue("Bar chart showing breakdown of {text} for all{renter} households in the City of Toronto. The data is in the table that follows.", renter = ifelse(renter, " renter", "")),
    "neighbourhood" = glue::glue("Bar chart comparing {text} for{renter} households in {neighbourhood} versus all{renter} households in the City of Toronto. The data is in the table that follows.", renter = ifelse(renter, " renter", ""))
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

# Summary statistics

summary_statistics_table <- function(data) {
  dplyr::tibble(
    `Total households (2016)` = data[["households"]] %>% scales::comma(),
    `Total population (2016)` = scales::comma(data[["population"]]),
    `Proportion renters (2016)` = data[["household_tenure"]] %>%
      dplyr::filter(.data$group == "Renter") %>%
      dplyr::pull(.data$prop) %>% scales::percent(accuracy = 0.1),
    `Renter households in core housing need (2016)` = format_measure(data[["core_housing_need"]], "core_housing_need"),
    `Eviction filings (2016)` = format_measure(data[["evictions"]], "evictions"),
    `Primary market vacancy rate (2020)` = format_measure(data[["vacancy_rate_2020"]], "vacancy_rate")
  ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    knitr::kable(col.names = NULL, align = "lr") %>%
    kableExtra::kable_minimal(
      html_font = "\"Lato\", sans-serif",
      full_width = TRUE
    )
}

# Rental supply -----

rental_supply_plot_alt_text <- function(level, neighbourhood) {
  switch(level,
    "city" = "Bar chart showing the breakdown of the rental market stock in the City of Toronto. The data is in the table that follows.",
    "neighbourhood" = glue::glue("Bar chart showing the breakdown of the rental market stock in {neighbourhood}. The data is in the table that follows.")
  )
}

# Number of apartments ----

number_of_apartments_number <- function(number_of_apartments_formatted) {
  glue::glue("Apartment buildings (2021): {number_of_apartments_formatted}")
}

number_of_apartments_breakdown <- function(data) {
  glue::glue("({scales::comma(privately_owned)} privately owned, {scales::comma(tch)} Toronto Community Housing, {scales::comma(social_housing)} other non-market)",
    privately_owned = data[["number_of_buildings_private"]],
    tch = data[["number_of_buildings_tch"]],
    social_housing = data[["number_of_buildings_social_housing"]]
  )
}

number_of_apartments_description <- function(level, neighbourhood, number_of_apartments, number_of_apartments_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["number_of_buildings_distribution"]][["value"]])
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
  values <- lemr::city_aggregate[["number_of_buildings_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of number of apartment buildings for each of Toronto's neighbourhoods. The values range from {min} to {max} apartment buildings and the distribution is heavily skewed left with most values between {skew_min} and {skew_max}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the number of apartment buildings in {neighbourhood} is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

number_of_apartments_plot <- function(data, compare, static = FALSE) {
  data %>%
    plot_neighbourhood_profile_distribution("number_of_buildings", compare = compare, binwidth = 5, static = static)
}

number_of_units_number <- function(number_of_units_formatted) {
  glue::glue("Apartment building units (2021): {number_of_units_formatted}")
}

number_of_units_breakdown <- function(data) {
  glue::glue("({scales::comma(privately_owned)} privately owned, {scales::comma(tch)} Toronto Community Housing, {scales::comma(social_housing)} other non-market)",
    privately_owned = data[["number_of_units_private"]],
    tch = data[["number_of_units_tch"]],
    social_housing = data[["number_of_units_social_housing"]]
  )
}

number_of_units_description <- function(level, neighbourhood, number_of_units, number_of_units_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["number_of_units_distribution"]][["value"]])
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
  values <- lemr::city_aggregate[["number_of_units_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of number of units in apartment buildings for each of Toronto's neighbourhoods. The values range from {min} to {max} units and the distribution is heavily skewed left with most values between {skew_min} and {skew_max}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the number of units in apartment buildings in {neighbourhood} is highlighted.")
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
  if (apartment_building_evaluation_formatted == "NA%") {
    "Apartment building evaluation scores (2021)"
  } else {
    glue::glue("Median apartment building evaluation score (2021): {apartment_building_evaluation_formatted}")
  }
}

apartment_building_evaluation_none <- function(apartment_building_evaluation_formatted) {
  if (apartment_building_evaluation_formatted == "NA%") {
    "There are no apartment buildings in this neighbourhood, so no evaluation scores to report."
  }
}

apartment_building_evaluation_description <- function(level, neighbourhood, apartment_building_evaluation, apartment_building_evaluation_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["apartment_building_evaluation_distribution"]][["value"]])
    value_percentile <- value_distribution(apartment_building_evaluation)
  }

  # Switch level to "city" if there are no buildings (therefore no scores) in the neighbourhood.
  if (is.na(apartment_building_evaluation)) {
    level <- "city"
  }

  switch(level,
    "city" = "Distribution of median apartment building evaluation score for each of the City of Toronto neighbourhoods with apartment buildings.",
    "neighbourhood" = glue::glue("Distribution of median apartment building evaluation score for each of the City of Toronto neighbourhoods with apartment buildings. The value for {neighbourhood}, {apartment_building_evaluation_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

apartment_building_evaluation_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["apartment_building_evaluation_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of median apartment building evaluation score for each of Toronto's neighbourhoods that have apartment buildings. The values range from {min}% to {max}% and the distribution is normally distributed with most values between {skew_min}% and {skew_max}%.",
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    skew_min = stats::quantile(values, 0.1, na.rm = TRUE),
    skew_max = stats::quantile(values, 0.9, na.rm = TRUE)
  )

  # Switch level to "city" if there are no buildings (therefore no scores) in the neighbourhood.
  if (level == "neighbourhood") {
    if (is.na(lemr::neighbourhood_aggregate[[neighbourhood]][["apartment_building_evaluation"]])) {
      level <- "city"
    }
  }

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing the median apartment building score in {neighbourhood} is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

apartment_building_evaluation_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("apartment_building_evaluation", compare = compare, binwidth = 2, static = static)

  if (static) {
    p +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
  } else {
    p %>%
      plotly::layout(xaxis = list(ticksuffix = "%"))
  }
}

# Amenity density -----

amenity_density_description <- function(level, neighbourhood) {
  switch(level,
    "city" = glue::glue("Breakdown of population living in areas that have high, medium, and low proximity to services in the City of Toronto."),
    "neighbourhood" = glue::glue("Comparison of population living in areas that have high, medium, and low proximity to services in {neighbourhood} versus in the City of Toronto.")
  )
}

amenity_density_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level, neighbourhood, "proximity to services by population", renter = FALSE)
}

amenity_density_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("amenity_density", compare = compare, width = 25, static = static)
}

# Core housing need ----


core_housing_need_number <- function(core_housing_need_formatted) {
  glue::glue("Core housing need (2016): {core_housing_need_formatted} of renter households")
}

core_housing_need_description <- function(level, neighbourhood, core_housing_need, core_housing_need_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["core_housing_need_distribution"]][["value"]])
    value_percentile <- value_distribution(core_housing_need)
  }

  switch(level,
    "city" = "Distribution of percent of renter households in core housing need for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of renter households in core housing need for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {core_housing_need_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

core_housing_need_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["core_housing_need_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of percent of renter households in core housing need for each of the City of Toronto neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} in core housing need with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s core housing need percent is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

core_housing_need_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("core_housing_need", compare = compare, binwidth = 0.025, static = static)

  if (static) {
    p + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# Evictions -----

evictions_number <- function(evictions_formatted) {
  glue::glue("Eviction filings per renter households (2016): {evictions_formatted} of renter households")
}

evictions_description <- function(level, neighbourhood, evictions, evictions_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["evictions_distribution"]][["value"]])
    value_percentile <- value_distribution(evictions)
  }

  switch(level,
    "city" = "Distribution of percent of rental households with evictions for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of rental households with evictions for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {evictions_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

evictions_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["evictions_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of percent of rental households with evictions for each of the City of Toronto neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} evictions, and the distribution is heavily skewed with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s evictions is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

evictions_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("evictions", compare = compare, binwidth = 0.0075, static = static)

  if (static) {
    p + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# Vacancy rate ----

vacancy_rate_number <- function(vacancy_rate_formatted) {
  glue::glue("Primary market vacancy rate (2020): {vacancy_rate_formatted} of renter households")
}

vacancy_rate_description <- function(level, neighbourhood, vacancy_rate, vacancy_rate_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["vacancy_rate_2020_distribution"]][["value"]])
    value_percentile <- value_distribution(vacancy_rate)
  }

  switch(level,
    "city" = "Distribution of primary market vacancy rate for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of primary market vacancy rate for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {vacancy_rate_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

vacancy_rate_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["vacancy_rate_2020_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of vacancy rate for each of the City of Toronto neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} vacancy_rate, and the distribution is heavily left skewed with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s vacancy rate is highlighted.")
    alt_text <- glue::glue("{alt_text} {neighbourhood_alt_text}")
  }

  alt_text
}

vacancy_rate_plot <- function(data, compare, static = FALSE) {
  p <- data %>%
    plot_neighbourhood_profile_distribution("vacancy_rate_2020", compare = compare, binwidth = 0.005, static = static)

  if (static) {
    p + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    p %>%
      plotly::layout(xaxis = list(tickformat = "%"))
  }
}

# Population change ----

population_change_number <- function(population_change_formatted) {
  glue::glue("Population change, 2011 to 2016: {population_change_formatted}")
}

population_change_description <- function(level, neighbourhood, population_change, population_change_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["population_change_distribution"]][["value"]])
    value_percentile <- value_distribution(population_change)
  }

  switch(level,
    "city" = "Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population change from 2011 to 2016 for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_change_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population change.")
  )
}

population_change_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["population_change_distribution"]][["value"]]

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
  glue::glue("Population density (2016): {data} people per square kilometre")
}

population_density_description <- function(level, neighbourhood, population_density, population_density_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["population_density_distribution"]][["value"]])
    value_percentile <- value_distribution(population_density)
  }

  switch(level,
    "city" = "Distribution of population density for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of population density for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {population_density_formatted} people per square kilometre, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' population density.")
  )
}

population_density_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["population_density_distribution"]][["value"]]

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
    "city" = "Average total income by renter household size in the City of Toronto.",
    "neighbourhood" = glue::glue("Comparison of average total income by renter household size in {neighbourhood} versus in the City of Toronto.")
  )
}

average_total_household_income_plot_alt_text <- function(level, neighbourhood) {
  switch(level,
    "city" = "Bar chart comparing average total income by renter household size in the City of Toronto. The data is in the table that follows.",
    "neighbourhood" = glue::glue("Bar chart comparing average total income by renter household size in {neighbourhood} versus in the City of Toronto. The data is in the table that follows.")
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

unaffordable_housing_number <- function(unaffordable_housing_formatted, level) {
  number <- glue::glue("Unaffordable housing (2016): {unaffordable_housing_formatted} of renter households")

  if (level == "neighbourhood") {
    glue::glue('{number} (City of Toronto: {scales::percent(lemr::city_aggregate[["unaffordable_housing"]], accuracy = 0.1)})')
  } else {
    number
  }
}

unaffordable_housing_description <- function(level, neighbourhood, unaffordable_housing, unaffordable_housing_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["unaffordable_housing_distribution"]][["value"]])
    value_percentile <- value_distribution(unaffordable_housing)
  }

  switch(level,
    "city" = "Distribution of percent of renter households with unaffordable housing for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of renter households with unaffordable housing for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {unaffordable_housing_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' percent of renter households with unaffordable housing.")
  )
}

unaffordable_housing_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["unaffordable_housing_distribution"]][["value"]]

  alt_text <- glue::glue("Histogram showing the distribution of percent of renter households with unaffordable housing for each of Toronto's neighbourhoods. The values range from {scales::percent(min, accuracy = 0.1)} to {scales::percent(max, accuracy = 0.1)} of renter households with unaffordable housing with most values between {scales::percent(skew_min, accuracy = 0.1)} and {scales::percent(skew_max, accuracy = 0.1)}.",
    min = min(values),
    max = max(values),
    skew_min = stats::quantile(values, 0.1),
    skew_max = stats::quantile(values, 0.9)
  )

  if (level == "neighbourhood") {
    neighbourhood_alt_text <- glue::glue("The bar containing {neighbourhood}'s percent of renter households with unaffordable housing is highlighted.")
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

lim_at_number <- function(data, level) {
  number <- glue::glue("Low-income measure after tax (2016): {data} of population")

  if (level == "neighbourhood") {
    glue::glue('{number} (City of Toronto: {format_measure(lemr::city_aggregate[["lim_at"]], "lim_at")})')
  } else {
    number
  }
}

lim_at_description <- function(level, neighbourhood, lim_at, lim_at_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["lim_at_distribution"]][["value"]])
    value_percentile <- value_distribution(lim_at)
  }

  switch(level,
    "city" = "Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of percent of people considered low income based on the low-income measure after tax (LIM-AT) for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {lim_at_formatted}, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods'.")
  )
}

lim_at_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["lim_at_distribution"]][["value"]]

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

visible_minority_number <- function(data, level) {
  prop <- data[["visible_minority"]] %>%
    dplyr::filter(.data$group != "Not a visible minority") %>%
    dplyr::pull(.data$prop) %>%
    sum() %>%
    scales::percent(accuracy = 0.1)

  number <- glue::glue("Visible minority population (2016): {prop}")

  if (level == "neighbourhood") {
    city_prop <- lemr::city_aggregate[["visible_minority"]] %>%
      dplyr::filter(.data$group != "Not a visible minority") %>%
      dplyr::pull(.data$prop) %>%
      sum() %>%
      scales::percent(accuracy = 0.1)

    glue::glue("{number} (City of Toronto: {city_prop})")
  } else {
    number
  }
}

visible_minority_description <- function(level, neighbourhood) {
  switch(level,
    "city" = "Breakdown of visible minority groups by population in the City of Toronto.",
    "neighbourhood" = glue::glue("Comparison of visible minority groups by population in {neighbourhood} versus in the City of Toronto.")
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
  generate_bar_chart_description(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)", renter = FALSE)
}

household_tenure_plot_alt_text <- function(level, neighbourhood) {
  generate_bar_chart_alt_text(level = level, neighbourhood = neighbourhood, text = "household tenure (renter versus owner)", renter = FALSE)
}

household_tenure_plot <- function(data, compare, static = FALSE) {
  data %>%
    display_neighbourhood_profile("household_tenure", compare = compare, width = 25, static = static)
}

# Shelter cost -----

shelter_cost_number <- function(shelter_cost_formatted, level) {
  number <- glue::glue("Average renter shelter cost (2016): {shelter_cost_formatted}")

  if (level == "neighbourhood") {
    glue::glue('{number} (City of Toronto: {scales::dollar(lemr::city_aggregate[["average_renter_shelter_cost"]], accuracy = 1)})')
  } else {
    number
  }
}

average_renter_shelter_cost_description <- function(level, neighbourhood, shelter_cost, shelter_cost_formatted) {
  if (level == "neighbourhood") {
    value_distribution <- stats::ecdf(lemr::city_aggregate[["average_renter_shelter_cost_distribution"]][["value"]])
    value_percentile <- value_distribution(shelter_cost)
  }

  switch(level,
    "city" = "Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods.",
    "neighbourhood" = glue::glue("Distribution of average renter shelter cost for each of the City of Toronto neighbourhoods. The value for {neighbourhood}, {shelter_cost_formatted} per month, is higher than {scales::percent(accuracy = 1, value_percentile)} of other neighbourhoods' average rent.")
  )
}

average_renter_shelter_cost_plot_alt_text <- function(level, neighbourhood) {
  values <- lemr::city_aggregate[["average_renter_shelter_cost_distribution"]][["value"]]

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

# AGIs and TDFs ----

agi_tdf_description <- function(level, neighbourhood) {
  switch(level,
    "city" = "Number of AGI applications in privately owned apartment buildings (and the rate at which they occur in those buildings) and number of TDF grants received (with rate), in the City of Toronto.",
    "neighbourhood" = glue::glue("Number of Above Guideline Increase applications in privately owned apartment buildings (and the rate at which they occur in those buildings) and number of Tenant Defence Fund grants received (with rate), in {neighbourhood} versus in the City of Toronto.")
  )
}

agi_non_apartments <- function(data, level, neighbourhood) {
  switch(level,
    "city" = glue::glue("There were {n_agi} AGI applications for other buildings in the City of Toronto.",
      n_agi = lemr::city_aggregate[["agi"]] %>%
        dplyr::filter(.data$group == "Non-apartment building") %>%
        dplyr::pull(.data$value)
    ),
    "neighbourhood" = glue::glue("There {were_word} {n_agi} AGI {application_word} for other buildings in {neighbourhood} ({n_agi_toronto} in the City of Toronto).",
      n_agi = data[["agi"]] %>%
        dplyr::filter(.data$group == "Non-apartment building") %>%
        dplyr::pull(.data$value),
      were_word = ifelse(.data$n_agi == 1, "was", "were"),
      application_word = ifelse(.data$n_agi == 1, "application", "applications"),
      n_agi_toronto = lemr::city_aggregate[["agi"]] %>%
        dplyr::filter(.data$group == "Non-apartment building") %>%
        dplyr::pull(.data$value)
    )
  )
}
