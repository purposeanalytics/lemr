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

#' City of Toronto Neighbourhood Profiles
#'
#' \code{neighbourhood profiles} contains variables describing profiles of neighbourhoods in Toronto from the 2016 census. The data set is a list, with one element for each neighbourhood. Within each neighbourhood there is one element for each variable. \code{city_profile} describes the city of Toronto from the 2016 census, including breakdowns for the city or the distribution across neighbourhoods, depending on the variable.
#'
#' The variables are:
#'
#' * **Population**: Population residing in the area at the time of the 2016 Census. Includes Canadian citizens and landed immigrants with a usual place of residence in Canada, and non-permanent residents living in Canada.
#' * **Households**: A private household is a person or group of persons who occupy the same dwelling and do not have a usual place of residence elsewhere in Canada or abroad. Private dwellings are all households which are not collective in nature.
#' * **Population change**: Population change reflects the per cent change in the population since the last Census in 2011.
#' * **Population density**: Number of people per square kilometer.
#' * **Household size**: Breakdown of number of persons in private households.
#' * **Average total income**: Average total income broken down by one and two-plus person households. Total income refers to the total amount of income for a household in 2015 that is of a regular and recurring nature, such as investment and pension income, employment income, and income from government sources.
#' * **Unaffordable housing**: Unaffordable housing is the percentage of private households spending more than 30 per cent of their total household income on shelter costs.
#' * **Low income (LIM-AT)**: Low income (LIM-AT) is the percentage of people in private households in low income status according to the Low Income Measure, After-Tax.
#' * **Visible minority**: Visible minority population is the percentage of people in private households who belong to a visible minority group, i.e. persons, other than Aboriginal peoples, who are non-Caucasian in race or non-white in colour.
#' * **Private dwellings by structure type**: Breakdown of dwellings by structure type, referring to the structural characteristics and/or configuration of the dwelling. Two categories in the Census, mobile dwellings and ""other"" single-attached house, make up a very small proportion of Toronto's occupied dwellings and so are not reported.
#' * **Number of bedrooms**: Breakdown of number of bedrooms.
#' * **Household tenure**: Households by tenure provides data on the number and proportion of private households that own or rent their dwelling. A third tenure type, which is not present in the City of Toronto, is whether the dwelling is band housing (on an Indian reserve or settlement).
#' * **Average shelter cost for rentals**: Shelter cost refers to the average monthly total of all shelter expenses paid by households that rent their dwelling. Shelter costs include, where applicable, the rent and the costs of electricity, heat, water and other municipal services. The reference period for shelter cost data is 2016, while household total income is reported for the year 2015.
#'
#' @rdname profiles
#' @md
#' @examples
#' neighbourhood_profiles[["Danforth"]]
#'
#' city_profile[["average_total_income"]]
"neighbourhood_profiles"

#' @rdname profiles
"city_profile"

#' Address Points
#'
#' Address points for over 500,000 addresses within the City of Toronto, retrieved from \href{https://open.toronto.ca/dataset/address-points-municipal-toronto-one-address-repository/}{Address Points (Municipal) - Toronto One Address Repository}. The data is stored in a SQLite database, so it can be filtered like a regular data frame, then results must be "collected" with \link[dplyr]{collect}.
#'
#' @export
#'
#' @examples \dontrun{
#' library(dplyr)
#' address_points() %>%
#'   collect()
#'
#' address_points() %>%
#'   filter(address == "404 Lake Promenade") %>%
#'   collect()
#' }
address_points <- function() {
  dplyr::tbl(pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = system.file("extdata/lemur.sqlite", package = "lemur")
), "address_points")
}

