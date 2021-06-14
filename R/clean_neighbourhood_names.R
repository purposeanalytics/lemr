#' Clean neighbourhood names
#'
#' Clean up City of Toronto neighbourhood names to a standardized format and remove neighbourhood numbers, matching what's in the \link{neighbourhoods} data set.
#'
#' If there are any cleaning errors and the names cannot all be matched to that format (meaning additional cleaning is required), the function produces a warning with the mismatched neighbourhood names, and returns the matching names cleaned, and the mismatching names in their original form.
#'
#' @param neighbourhood Name of neighbourhood column to be cleaned
#'
#' @export
#'
#' @examples {
#' library(opendatatoronto)
#' library(dplyr)
#'
#' list_package_resources("https://open.toronto.ca/dataset/neighbourhoods/") %>%
#'   get_resource() %>%
#'   mutate(neighbourhood = clean_neighbourhood_names(AREA_NAME))
#' }
clean_neighbourhood_names <- function(neighbourhood) {

  res <- dplyr::tibble(x = neighbourhood) %>%
    # Remove neighbourhood numbers
    tidyr::separate(.data$x, into = ".neighbourhood_new", sep = " \\([0-9]", extra = "drop", remove = FALSE) %>%
    dplyr::rename(neighbourhood = .data$.neighbourhood_new, .neighbourhood_original = .data$x) %>%
    # Flag if cleaned neighbourhoods match the neighbourhoods data set
    dplyr::mutate(.neighbourhood_match = neighbourhood %in% lemur::neighbourhoods[["neighbourhood"]]) %>%
    # If not, return the ORIGINAl name (with a warning)
    dplyr::mutate(neighbourhood = dplyr::case_when(
      .data$.neighbourhood_match ~ .data$neighbourhood,
      !.data$.neighbourhood_match ~ .data$.neighbourhood_original
    ))

  mismatch <- res %>%
    dplyr::filter(!.data$.neighbourhood_match) %>%
    dplyr::pull(.data$.neighbourhood_original) %>%
    unique()

  all_match <- length(mismatch) == 0

  if (!all_match) {
    warning("Not all neighbourhoods could be cleaned to match the formats in `neighbourhoods` data.\nMismatches: ", paste0(mismatch, collapse = ", "), immediate. = TRUE, call. = FALSE)
  }

  res[["neighbourhood"]]
}
