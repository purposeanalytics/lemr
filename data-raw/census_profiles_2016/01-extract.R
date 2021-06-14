# Read in census profiles data, only keep Toronto CTs and derive dimension hierarchy

# Retrieved from: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/page_dl-tc.cfm?Lang=E
# Geographic level: Census metropolitan areas (CMAs), tracted census agglomerations (CAs) and census tracts (CTs)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Set up paths for data sets ------

census_path <- here::here("data-raw", "census_profiles_2016", "raw", "98-401-X2016043_eng_CSV", "98-401-X2016043_English_CSV_data.csv")
variables_path <- here::here("data-raw", "census_profiles_2016", "raw", "98-401-X2016043_eng_CSV", "98-401-X2016043_English_meta.txt")

# ### Get starting and ending row of Toronto -----

starting_row <- read_csv(here::here("data-raw", "census_profiles_2016", "raw", "98-401-X2016043_eng_CSV", "Geo_starting_row_CSV.csv"))

# The Geo Names seem to be numeric, unless they are a city  - then everything between cities is for that city. So look for Toronto, then look for the next city, and everything between is Toronto.

toronto_start_with_next_geo <- starting_row %>%
  mutate(geo_name_as_numeric = parse_number(`Geo Name`)) %>%
  filter(is.na(geo_name_as_numeric)) %>%
  arrange(`Line Number`) %>%
  mutate(
    next_geo = lead(`Geo Name`),
    next_geo_line_number = lead(`Line Number`)
  ) %>%
  filter(`Geo Name` == "Toronto")

# Next geography is Hamilton (makes sense), and starts on line 7235342.

toronto_start_end <- toronto_start_with_next_geo %>%
  select(
    start = `Line Number`,
    end = next_geo_line_number
  ) %>%
  mutate(end = end - 1) # Subtract 1 since Hamilton starts on 7235342.

### Build variable hierarchy from metadata -----

original_variables <- read_lines(variables_path, skip = 211, n_max = 2458 - 211)

variables <- tibble(x = original_variables) %>%
  # Fix some where there's " -[A-Z]" instead of " - " (missing space at end)
  mutate(x = str_replace(original_variables, " -[A-Z]", " - ")) %>%
  # And some where there's " -  " instead of " - " (one extra space at end)
  mutate(x = str_replace(original_variables, " -  ", " - ")) %>%
  mutate(
    x = str_replace(x, " \\(\\d+\\)", ""), # Remove (#) at the end of lines
    x = str_replace_all(x, " ", "@@") # Replace any spaces with @@, for easier viewing / separating
  ) %>%
  separate(x, into = c("member_id", "dimension"), "\\.@@", extra = "merge") %>%
  mutate(
    member_id = str_replace_all(member_id, "@", ""),
    dimension = str_replace_all(dimension, "@@@@", ";"),
    dimension = str_replace_all(dimension, "@@", " ")
  )

n_levels <- variables %>%
  mutate(levels = str_count(dimension, ";")) %>%
  pull(levels) %>%
  max()

n_levels <- n_levels + 1

level_cols <- glue::glue("level_{1:n_levels}")

variables <- variables %>%
  separate(dimension, into = level_cols, sep = ";", fill = "right", remove = FALSE) %>%
  mutate_all(na_if, "") %>%
  fill(level_1)

# Fill each level down, unless the level *before* it has changed

fill_level <- function(df, level) {
  var <- glue::glue("level_{level}")
  var <- rlang::sym(var)

  prev_var <- glue::glue("level_{level - 1}")
  prev_var <- rlang::sym(prev_var)

  all_prev_vars <- glue::glue("level_{1:(level - 1)}")
  all_prev_vars <- rlang::syms(all_prev_vars)

  df %>%
    group_by(!!!all_prev_vars) %>%
    fill(!!var) %>%
    mutate(!!var := case_when(
      !!prev_var != lag(!!prev_var) ~ NA_character_,
      TRUE ~ !!var
    )) %>%
    ungroup()
}

variables <- variables %>%
  fill_level(2) %>%
  fill_level(3) %>%
  fill_level(4) %>%
  fill_level(5) %>%
  fill_level(6) %>%
  fill_level(7) %>%
  fill_level(8)

# Construct full dimension and the final level
variables <- variables %>%
  mutate(
    dimension_full = paste(level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, sep = "::"),
    level = as.numeric(!is.na(level_1)) +
      as.numeric(!is.na(level_2)) +
      as.numeric(!is.na(level_3)) +
      as.numeric(!is.na(level_4)) +
      as.numeric(!is.na(level_5)) +
      as.numeric(!is.na(level_6)) +
      as.numeric(!is.na(level_7)) +
      as.numeric(!is.na(level_8))
  ) %>%
  mutate_all(list(~ str_replace_all(., "::NA", ""))) %>%
  mutate(dimension = str_replace_all(dimension, ";", ""))

hierarchy_for_level <- function(df, level) {
  if (level == 1) {
    distinct_levels <- df %>%
      distinct(level_1) %>%
      mutate(level_hierarchy = as.character(row_number()))

    res <- df %>%
      left_join(distinct_levels, by = "level_1") %>%
      select(level_hierarchy)
  } else {
    vars_chr <- glue::glue("level_{1:(level - 1)}")
    vars <- rlang::syms(vars_chr)

    level_var_chr <- glue::glue("level_{level}")
    level_var <- level_var_chr %>%
      rlang::sym()

    distinct_levels <- df %>%
      group_by(!!!vars) %>%
      distinct(!!level_var) %>%
      filter(!is.na(!!level_var)) %>%
      mutate(level_hierarchy = as.character(row_number())) %>%
      ungroup()

    res <- df %>%
      left_join(distinct_levels, by = c(vars_chr, level_var_chr)) %>%
      select(level_hierarchy) %>%
      mutate(level_hierarchy = coalesce(level_hierarchy, ""))
  }

  names(res) <- glue::glue("level_{level}")

  res
}

generate_hierarchy <- function(df) {
  level_vars <- df %>%
    select(starts_with("level_", ignore.case = FALSE)) %>%
    names()

  levels <- readr::parse_number(level_vars)

  res <- purrr::map(levels, ~ hierarchy_for_level(df, .x))

  res <- bind_cols(res)

  res %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to = "level", values_to = "value", names_prefix = "level_") %>%
    group_by(id) %>%
    arrange(level) %>%
    filter(value != "") %>%
    summarise(dimension_id = paste0(value, collapse = ".")) %>%
    select(dimension_id)
}

variables_hierarchy <- variables %>%
  generate_hierarchy()

variables_with_hierarchy <- variables_hierarchy %>%
  bind_cols(variables)

hierarchy <- variables_with_hierarchy %>%
  mutate(
    parent_id = str_replace(dimension_id, "\\.[^\\.]*$", ""),
    member_id = as.numeric(member_id)
  ) %>%
  select(member_id, parent_id, dimension_id, dimension, dimension_full)

### Get Toronto census tracts -----
# Load only from the start to the end of Toronto

toronto_cts <- read_csv(census_path, skip = toronto_start_end[["start"]] - 1, guess_max = 100000, n_max = toronto_start_end[["end"]] - toronto_start_end[["start"]] + 1, trim_ws = FALSE, col_names = FALSE)

# Get column names since we skipped those on load

census_col_names <- read_csv(census_path, n_max = 1, trim_ws = FALSE)

names(toronto_cts) <- names(census_col_names)


# ### Add hierarchy to census tracts ----

toronto_cts <- hierarchy %>%
  inner_join(toronto_cts, by = c("member_id" = "Member ID: Profile of Census Tracts (2247)"))

# ### Tidy names

toronto_cts <- toronto_cts %>%
  rename(
    total = `Dim: Sex (3): Member ID: [1]: Total - Sex`,
    # Some conversion of values (... to NA) needs to be done, but I'll do that in the cleaning step so that I can document the ...
    male = `Dim: Sex (3): Member ID: [2]: Male`,
    female = `Dim: Sex (3): Member ID: [3]: Female`
  ) %>%
  clean_names()

# ### Save Toronto census tracts

saveRDS(toronto_cts, here::here("data-raw", "census_profiles_2016", "extract", "toronto_census_tracts.rds"))
