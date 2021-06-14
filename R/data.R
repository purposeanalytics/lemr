#' Apartment building registry
#'
#' Apartment building registry retrieved from the City of Toronto's \href{https://open.toronto.ca/dataset/apartment-building-registration/}{Apartment Building Registration dataset}, and geocoded using the \href{https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/find-a-location-by-address}{Bing geocoding API}. Contains the address of all rental apartment buildings in the city with 3 or more storeys and 10 or more units, the neighbourhood that each apartment is in, and many attributes about the apartment.
#' @examples
#' library(sf)
#' apartment_building_registry
"apartment_building_registry"

#' City of Toronto boundaries
#'
#' City of Toronto boundaries, retrieved from the \href{https://open.toronto.ca/dataset/regional-municipal-boundary/}{Regional Municipal Boundary dataset}
#' @examples
#' library(sf)
#' toronto
"toronto"

#' City of Toronto Neighbourhoods
#' Neighbourhood boundaries from the City of Toronto, from \href{https://open.toronto.ca/dataset/neighbourhoods/}{Neighbourhoods data set}
#' @examples
#' library(sf)
#' neighbourhoods
"neighbourhoods"
