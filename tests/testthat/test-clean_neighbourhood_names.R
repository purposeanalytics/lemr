test_that("clean_neighbourhood_names warns if not all neighbourhoods could be cleaned, and returns the mismatches, plus the full values", {
  expect_warning(dplyr::tibble(neighbourhood = "Donlands (66)") %>% mutate(neighbourhood = clean_neighbourhood_names(neighbourhood)), "Donlands \\(66\\)")

  mismatch <- suppressWarnings(dplyr::tibble(neighbourhood = "Donlands (66)") %>% mutate(neighbourhood = clean_neighbourhood_names(neighbourhood)))
  expect_true(nrow(mismatch) == 1)

  expect_warning(dplyr::tibble(nbhd = c("Donlands (66)", "Danforth (66)")) %>% dplyr::mutate(neighbourhood = clean_neighbourhood_names(nbhd)), "Donlands \\(66\\)")
  mismatch <- suppressWarnings(dplyr::tibble(nbhd = c("Donlands (66)", "Danforth (66)")) %>% dplyr::mutate(neighbourhood = clean_neighbourhood_names(nbhd)))
  expect_true(nrow(mismatch) == 2)
})

test_that("clean_neighbourhood_names returns cleaned neighbourhoods and originals for any mismatches", {
  mismatch <- suppressWarnings(dplyr::tibble(nbhd = c("Donlands (66)", "Danforth (66)")) %>% dplyr::mutate(neighbourhood = clean_neighbourhood_names(nbhd)))
  expect_equal(mismatch[["neighbourhood"]], c("Donlands (66)", "Danforth"))
})
