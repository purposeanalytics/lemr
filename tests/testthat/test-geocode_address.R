test_that("geocode_address geocodes an address and returns bing fields", {
  skip_if_offline()
  skip_on_ci()

  res <- geocode_address("100 Queen St W Toronto ON", quiet = TRUE)

  expect_identical(res, structure(list(
    bing_status_code = 200L, bing_address = "100 Queen St W",
    bing_municipality = "Toronto", bing_postal_code = "M5C 1S6",
    bing_method = "Rooftop", bing_confidence = "Medium", bing_latitude = 43.653512,
    bing_longitude = -79.383987
  ), row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )))
})

test_that("geocode_address returns a 404 and NAs if address can't be geocoded", {
  skip_if_offline()
  skip_on_ci()

  res <- geocode_address("25 Grandstand Toronto ON", quiet = TRUE)
  expect_identical(res, structure(list(
    bing_status_code = 404, bing_address = NA, bing_municipality = NA,
    bing_postal_code = NA, bing_method = NA, bing_confidence = NA,
    bing_latitude = NA, bing_longitude = NA
  ), row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")))
})

test_that("geocode_address returns a 401 and NAs if the token is invalid", {
  skip_if_offline()
  skip_on_ci()

  res <- geocode_address("101 Queen Street W Toronto ON", token = "lemur", quiet = TRUE)
  expect_identical(
    res,
    structure(list(
      bing_status_code = 401L, bing_address = NA, bing_municipality = NA,
      bing_postal_code = NA, bing_method = NA, bing_confidence = NA,
      bing_latitude = NA, bing_longitude = NA
    ), row.names = c(
      NA,
      -1L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
})

test_that("geocode_address errors if token is '' (if the environment variable isn't set) or NULL}", {
  expect_error(geocode_address("101 Queen Street W Toronto ON", token = ""), "No token provided")

  expect_error(geocode_address("101 Queen Street W Toronto ON", token = NULL), "No token provided")
})

test_that("geocode_address can geocode an address whose street name needs to be converted to numeric", {
  skip_if_offline()
  skip_on_ci()

  res <- geocode_address("88 E Forty Second St, Hamilton, ON", quiet = TRUE)
  expect_identical(res, structure(list(
    bing_status_code = 200L, bing_address = "88 E 42nd St",
    bing_municipality = "Hamilton", bing_postal_code = "L8T",
    bing_method = "Rooftop", bing_confidence = "High", bing_latitude = 43.233584,
    bing_longitude = -79.834694
  ), row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )))
})

test_that("convert_street_name_to_numeric converts a street name to its numeric version", {
  expect_identical(convert_street_name_to_numeric("88 Forty Second St"), "88 42nd St")

  expect_identical(convert_street_name_to_numeric("100 Queen St"), "100 Queen St")
})
