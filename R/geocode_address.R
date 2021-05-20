#' Geocode an address
#'
#' Geocode an address using the Bing geocoding API (\link{https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/find-a-location-by-address}), to get a cleaned address, municipality, postal code, latitude, and longitude. This function adds an additional 0.25 seconds at the end of the call to comply with the Bing API license (\link{http://mapsforenterprise.binginternal.com/en-us/maps/product}), which only allows 5 calls per second.
#'
#' @param address Address to geocode
#' @param base Base URL of Bing Geocoder: \link{http://dev.virtualearth.net/REST/v1/Locations/CA/}.
#' @param token BING Geocoder token, defaults to \code{BING_TOKEN} environment variable.
#' @param quiet Whether to silence message that address is being geocoded, with its status - defaults to \code{FALSE} (display the message). Useful if geocoding many addresses at once.
#'
#' @return A tibble containing status code, address, municipality, postal code, method used, method confidence, latitude, and longitude.
#' @export
#'
#' @examples
#' geocode_address("235 Bloor St E")
geocode_address <- function(address, base = "http://dev.virtualearth.net/REST/v1/Locations/CA/", token = Sys.getenv("BING_TOKEN"), quiet = FALSE) {

  # Error if no token
  if (token == "") {
    stop("No token provided", call. = FALSE)
  }

  clean_address <- address %>%
    stringr::str_squish() %>% # Remove excess whitespace
    stringr::str_replace_all("[^a-zA-Z0-9]", "%20") # Replace any spaces with %20, required for URLs
  call <- glue::glue("{base}{clean_address}Toronto%20ON?key={token}") # Full call URL

  # Get geocoding
  geocode_result <- httr::GET(call)

  # Status code
  status_code <- geocode_result[["status_code"]]

  # Display progress if quiet = FALSE
  if (!quiet) {
    cat(usethis::ui_info("Fetching {address} - Status: {status_code}"))
  }

  # If successful (status code 200), extract address

  if (status_code == 200) {
    geocode_json_tidied <- httr::content(geocode_result, "text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("resourceSets") %>%
      # Extract the element named "resourceSets"
      dplyr::pull(resources) %>%
      # Column named "resources"
      purrr::pluck(1) # First element

    # Address
    address <- geocode_json_tidied[["address.addressLine"]]

    # Municipality
    municipality <- geocode_json_tidied[["address.adminDistrict2"]]

    # Postal code
    postal_code <- geocode_json_tidied[["address.postalCode"]]

    # Information on geocoding
    geocode_points_tidied <- geocode_json_tidied %>%
      dplyr::pull(geocodePoints) %>%
      purrr::pluck(1) %>%
      dplyr::slice(1)

    # Method of determining geocoding
    method <- geocode_points_tidied[["calculationMethod"]]

    # Confidence of geocoding
    confidence <- geocode_json_tidied[["confidence"]]

    # Latitude and longitude
    latitude_longitude <- geocode_points_tidied %>%
      dplyr::pull(coordinates) %>% # Pull coordinates
      purrr::pluck(1) # In a list, so first element

    latitude <- latitude_longitude[[1]]
    longitude <- latitude_longitude[[2]]
  }

  res <- list(
    status_code = status_code,
    address = address,
    municipality = municipality,
    postal_code = postal_code,
    method = method,
    confidence = confidence,
    latitude = latitude,
    longitude = longitude
  )

  # Replace any NULLs with NAs, then turn into a tibble
  res <- purrr::map(res, function(x) {
    if (is.null(x)) {
      NA
    } else {
      x
    }
  }) %>%
    dplyr::as_tibble()

  names(res) <- glue::glue("bing_{names(res)}")

  Sys.sleep(0.25) # Sleep for 0.25 seconds to comply with API, which only allows for 5 calls per second

  res
}
