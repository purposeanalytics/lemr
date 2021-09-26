# Get "actual" vacancy rate for each neighbourhood by going from CMHC "neighbourhoods" -> CTs -> neighbourhoods

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
devtools::load_all()

vacancy_rate_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2016.rds"))
vacancy_rate_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_2020.rds"))

vacancy_rate_toronto_2016 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2016.rds")) %>%
  pull(vacancy_rate)

vacancy_rate_toronto_2020 <- readRDS(here::here("data-raw", "aggregate_data", "vacancy_rate", "extract", "vacancy_rate_toronto_2020.rds")) %>%
  pull(vacancy_rate)

cmhc_to_neighbourhood <- read_csv(here::here("data-raw", "aggregate_data", "vacancy_rate", "raw", "ct_2016_to_cmhc_neighbourhood.csv"))

cmhc_to_neighbourhood <- cmhc_to_neighbourhood %>%
  mutate(ct = glue::glue("535{CTNAME}")) %>%
  select(ct, cmhc_neighbourhood = CMHC_NEIGHBOURHOOD, neighbourhood = AREA_NAME) %>%
  mutate(neighbourhood = clean_neighbourhood_names(neighbourhood))

join_cmhc_to_neighbourhood <- function(vacancy_rate) {
  vacancy_rate %>%
    inner_join(cmhc_to_neighbourhood, by = "cmhc_neighbourhood") %>%
    distinct(ct, neighbourhood, vacancy_rate)
}

vacancy_rate_2016 <- vacancy_rate_2016 %>%
  join_cmhc_to_neighbourhood()

vacancy_rate_2020 <- vacancy_rate_2020 %>%
  join_cmhc_to_neighbourhood()

# Some are missing, so try to fill them in
# If only 2016 values are missing, model 2016 from 2020 and predict
# Same, if only 2020 are missing, model 2020 from 2016 and predict
# If both are missing, just use the city average for the year

vacancy_rate_combined <- vacancy_rate_2016 %>%
  full_join(vacancy_rate_2020, by = c("ct", "neighbourhood"), suffix = c("_2016", "_2020"))

vacancy_rate_combined_for_model <- vacancy_rate_combined %>%
  filter(!is.na(vacancy_rate_2016 & !is.na(vacancy_rate_2020)))

lm_2020_from_2016 <- lm(vacancy_rate_2020 ~ vacancy_rate_2016, data = vacancy_rate_combined)

vacancy_rate_predict_2020 <- vacancy_rate_combined %>%
  filter(is.na(vacancy_rate_2020), !is.na(vacancy_rate_2016))

predictions_2020 <- predict(lm_2020_from_2016, new = vacancy_rate_predict_2020)

vacancy_rate_predict_2020 <- vacancy_rate_predict_2020 %>%
  mutate(vacancy_rate_2020 = predictions_2020)

lm_2016_from_2020 <- lm(vacancy_rate_2016 ~ vacancy_rate_2020, data = vacancy_rate_combined)

vacancy_rate_predict_2016 <- vacancy_rate_combined %>%
  filter(!is.na(vacancy_rate_2020), is.na(vacancy_rate_2016))

predictions_2016 <- predict(lm_2016_from_2020, new = vacancy_rate_predict_2016)

vacancy_rate_predict_2016 <- vacancy_rate_predict_2016 %>%
  mutate(vacancy_rate_2016 = predictions_2016)

vacancy_rate_combined <- vacancy_rate_combined %>%
  mutate(
    estimate_2020 = FALSE,
    estimate_2016 = FALSE
  ) %>%
  rows_update(vacancy_rate_predict_2020 %>%
    mutate(
      estimate_2020 = TRUE,
      estimate_2016 = FALSE
    ), by = c("ct", "neighbourhood")) %>%
  rows_update(vacancy_rate_predict_2016 %>%
    mutate(
      estimate_2020 = FALSE,
      estimate_2016 = TRUE
    ), by = c("ct", "neighbourhood"))

vacancy_rate_combined <- vacancy_rate_combined %>%
  mutate(
    estimate_2016_2020 = is.na(vacancy_rate_2016) & is.na(vacancy_rate_2020),
    vacancy_rate_2016 = coalesce(vacancy_rate_2016, vacancy_rate_toronto_2016),
    vacancy_rate_2020 = coalesce(vacancy_rate_2020, vacancy_rate_toronto_2020)
  )

vacancy_rate_combined %>%
  mutate(estimate_type = case_when(
    estimate_2016_2020 ~ "2016 and 2020",
    estimate_2016 ~ "2016",
    estimate_2020 ~ "2020",
    TRUE ~ "none"
  )) %>%
  distinct(neighbourhood, vacancy_rate_2016, vacancy_rate_2020, estimate_type) %>%
  ggplot(aes(x = vacancy_rate_2016, vacancy_rate_2020, color = estimate_type)) +
  geom_jitter(alpha = 0.5)

# Save interpolated vacancy rates
vacancy_rate_2016 <- vacancy_rate_combined %>%
  select(ct, neighbourhood, vacancy_rate = vacancy_rate_2016)

vacancy_rate_2020 <- vacancy_rate_combined %>%
  select(ct, neighbourhood, vacancy_rate = vacancy_rate_2020)

saveRDS(vacancy_rate_2016, here::here("data-raw", "aggregate_data", "vacancy_rate", "interpolate", "vacancy_rate_2016.rds"))
saveRDS(vacancy_rate_2020, here::here("data-raw", "aggregate_data", "vacancy_rate", "interpolate", "vacancy_rate_2020.rds"))
