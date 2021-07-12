#' Geocode an address
#'
#' Geocode an address using the \href{https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/find-a-location-by-address}{Bing geocoding API}, to get a cleaned address, municipality, postal code, latitude, and longitude. This function adds an additional 0.25 seconds at the end of the call to comply with the \href{http://mapsforenterprise.binginternal.com/en-us/maps/product}{Bing API license}, which only allows 5 calls per second.
#'
#' @param address Address to geocode
#' @param base Base URL of Bing Geocoder: http://dev.virtualearth.net/REST/v1/Locations/CA/
#' @param token BING Geocoder token, defaults to \code{BING_TOKEN} environment variable.
#' @param quiet Whether to silence message that address is being geocoded, with its status - defaults to \code{FALSE} (display the message). Useful if geocoding many addresses at once.
#'
#' @return A tibble containing status code, address, municipality, postal code, method used, method confidence, latitude, and longitude.
#' @export
#'
#' @examples
#' geocode_address("235 Bloor St E Toronto ON")
#'
#' geocode_address("25 Grandstand Toronto ON")
geocode_address <- function(address, base = "http://dev.virtualearth.net/REST/v1/Locations/CA/", token = Sys.getenv("BING_TOKEN"), quiet = FALSE) {

  # Error if no token
  if (is.null(token) | identical(token, "")) {
    stop("No token provided", call. = FALSE)
  }

  clean_address <- address %>%
    stringr::str_squish() %>% # Remove excess whitespace
    stringr::str_replace_all("[^a-zA-Z0-9]", "%20") # Replace any spaces with %20, required for URLs
  call <- glue::glue("{base}{clean_address}?maxResults=1&key={token}") # Full call URL

  # Get geocoding
  geocode_result <- httr::GET(call)

  # Status code
  status_code <- geocode_result[["status_code"]]

  if (status_code != 200) {
    # Display progress if quiet = FALSE
    if (!quiet) {
      cat(usethis::ui_info("Fetching {address} - Status: {status_code}"))
    }

    # Return all NAs with status code

    res <- dplyr::tibble(
      status_code = status_code,
      address = NA,
      municipality = NA,
      postal_code = NA,
      method = NA,
      confidence = NA,
      latitude = NA,
      longitude = NA
    )

    names(res) <- glue::glue("bing_{names(res)}")

    Sys.sleep(0.25) # Sleep for 0.25 seconds to comply with API, which only allows for 5 calls per second

    return(res)
  } else if (status_code == 200) { # If successful (status code 200), extract address

    geocode_json_tidied <- httr::content(geocode_result, "text") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("resourceSets") %>%
      # Extract the element named "resourceSets"
      dplyr::pull(.data$resources) %>%
      # Column named "resources"
      purrr::pluck(1) # First element

    # Address
    geocode_address <- geocode_json_tidied[["address.addressLine"]]

    # If the address is NULL, return NAs for everything - there may still be results for latitude, longitude, etc, but in the case of Toronto, when it can't find it, it just returns the lat / long for city hall! Eek! Better to return NAs to make it clear that the geocoding failed.

    # Also change the status code to 404, "not found" - this is probably the closest option and I'd rather also flag issues this way, rather than returning 200 (= all good)

    if (is.null(geocode_address)) {
      if (!quiet) {
        cat(usethis::ui_info("Fetching {address} - Status: 404"))
      }

      res <- dplyr::tibble(
        status_code = 404,
        address = NA,
        municipality = NA,
        postal_code = NA,
        method = NA,
        confidence = NA,
        latitude = NA,
        longitude = NA
      )

      names(res) <- glue::glue("bing_{names(res)}")

      Sys.sleep(0.25) # Sleep for 0.25 seconds to comply with API, which only allows for 5 calls per second

      return(res)
    }

    # Display progress if quiet = FALSE
    if (!quiet) {
      cat(usethis::ui_info("Fetching {address} - Status: 200"))
    }

    # Municipality
    municipality <- geocode_json_tidied[["address.adminDistrict2"]]

    # Postal code
    postal_code <- geocode_json_tidied[["address.postalCode"]]

    # Information on geocoding
    geocode_points_tidied <- geocode_json_tidied %>%
      dplyr::pull(.data$geocodePoints) %>%
      purrr::pluck(1) %>%
      dplyr::slice(1) %>%
      dplyr::filter(.data$usageTypes == "Display")

    # Method of determining geocoding
    method <- geocode_points_tidied[["calculationMethod"]]

    # Confidence of geocoding
    confidence <- geocode_json_tidied[["confidence"]]

    # Latitude and longitude
    latitude_longitude <- geocode_points_tidied %>%
      dplyr::pull(.data$coordinates) %>% # Pull coordinates
      purrr::pluck(1) # In a list, so first element

    latitude <- latitude_longitude[[1]]
    longitude <- latitude_longitude[[2]]

    res <- list(
      status_code = status_code,
      address = geocode_address,
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
}
