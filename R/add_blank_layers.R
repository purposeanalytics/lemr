#' Add blank points layers
#'
#' Add empty layers of \link{buildings}. The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}. This function adds the following layers (accessed via IDs): apartment building registry (apartment_buildings), apartment building evaluation scores (apartment_evaluation), Above Guideline Increase applications (agi), and tenant defense fund grants (tdf).
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' map_toronto() %>%
#'   add_blank_points_layers() %>%
#'   toggle_layer_visible("tdf")
add_blank_points_layers <- function(map) {
  blur <- 0
  radius <- list(
    "stops" = list(
      c(10, 4),
      c(12, 6),
      c(14, 8),
      c(16, 10)
    )
  )
  opacity <- 0.8
  stroke_colour <- "#FFFFFF"
  stroke_width <- 1

  # Temporarily setting NA score_colour to "none" so we can filter the data in the RentSafeTO layer
  # I can't figure out how to filter out NA/null yet
  data <- lemr::buildings %>%
    dplyr::mutate(
      score_bucket = dplyr::coalesce(as.character(.data$score_bucket), "none"),
      show_tdf = .data$tdf & .data$apartment
    )

  map %>%
    # All points layers source data ----
    mapboxer::add_source(
      source = mapboxer::as_mapbox_source(data),
      id = "points_data_source"
    ) %>%
    # Apartment buildings ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "apartment_buildings",
      filter = list("==", "apartment", TRUE),
      circle_color = list(
        "case",
        list("==", c("get", "property_type"), "Privately owned"), rental_supply_colors()[["Apartment"]],
        list("==", c("get", "property_type"), "Toronto Community Housing"), rental_supply_colors()[["Toronto Community Housing"]],
        list("==", c("get", "property_type"), "Social housing"), rental_supply_colors()[["Other Non-Market"]],
        # Defaults to 'white'
        "white"
      ),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # Rooming houses ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "rooming_houses",
      filter = list("==", "rooming_house", TRUE),
      circle_color = list(
        "case",
        list("==", c("get", "rooming_house_status"), "Licensed prior to 2018"), rooming_house_colors()[["Licensed prior to 2018"]],
        list("==", c("get", "rooming_house_status"), "Licensed 2018 onwards"), rooming_house_colors()[["Licensed 2018 onwards"]],
        list("==", c("get", "rooming_house_status"), "Lapsed"), rooming_house_colors()[["Lapsed"]],
        # Defaults to 'white'
        "white"
      ),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # RentSafeTO Evaluation Scores -----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "apartment_evaluation",
      filter = list("!=", "score_bucket", "none"),
      circle_color = list(
        "case",
        list("==", c("get", "score_bucket"), "1"), rentsafe_colors()[[1]],
        list("==", c("get", "score_bucket"), "2"), rentsafe_colors()[[2]],
        list("==", c("get", "score_bucket"), "3"), rentsafe_colors()[[3]],
        list("==", c("get", "score_bucket"), "4"), rentsafe_colors()[[4]],
        # Defaults to 'white'
        "white"
      ),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # Above guideline increases ----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "agi",
      filter = list("==", "agi", TRUE),
      circle_color = list(
        "case",
        list("==", c("get", "apartment"), TRUE), layer_colours[["agi_apartment"]],
        list("==", c("get", "apartment"), FALSE), layer_colours[["agi_other"]],
        # Defaults to 'white'
        "white"
      ),
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    ) %>%
    # Tenant Defence Fund -----
    mapboxer::add_circle_layer(
      source = "points_data_source",
      id = "tdf",
      filter = list("==", "show_tdf", TRUE),
      circle_color = layer_colours[["tdf"]],
      circle_blur = blur,
      circle_opacity = opacity,
      circle_radius = radius,
      circle_stroke_color = stroke_colour,
      circle_stroke_width = stroke_width,
      visibility = FALSE,
      popup = "{{{tooltip}}}"
    )
}

#' Add a blank address layer
#'
#' Add an empty layer to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling highlighting an address, via \link{zoom_map_to_address}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_address_layer()
add_blank_address_layer <- function(map) {
  initial_data <- dplyr::tibble(longitude = "-79.384293", latitude = "43.653908") %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(initial_data), id = "address_points", circle_color = "red", circle_blur = 0.5, visibility = FALSE) %>%
    # Add tooltips with address
    mapboxer::add_tooltips(layer_id = "address_points", "{{address}}")
}

#' Add a blank amenity density layer
#'
#' Add an empty layer of amenity density to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible} (with the id "amenity_density").
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_amenity_density_layer() %>%
#'   toggle_layer_visible("amenity_density")
add_blank_amenity_density_layer <- function(map) {
  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "mapbox://purposeanalytics.15drt2xq"
    ),
    id = "amenity_density_data"
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = "amenity_density",
        "type" = "fill",
        "source" = "amenity_density_data",
        "source-layer" = "amenity_density-cxezz6",
        "layout" = list(
          "visibility" = "none"
        ),
        "paint" = list(
          "fill-color" = list(
            "case",
            list("==", c("get", "amenity_dense"), "Low"), amenity_density_colours()[["Low"]],
            list("==", c("get", "amenity_dense"), "Medium"), amenity_density_colours()[["Medium"]],
            list("==", c("get", "amenity_dense"), "High"), amenity_density_colours()[["High"]],
            # Defaults to 'white'
            "white"
          ),
          "fill-opacity" = 0.75
        )
      )
    )
}

#' Add a blank aggregate layers
#'
#' Add empty aggregate layers to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layers on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}. This function adds the following layers (accessed via IDs): estimated proportion low-end of market rentals (lem_percent), rental supply (primary - rental_supply_primary; condo - rental_supply_condo; non-condo secondary - rental_supply_non_condo; non-market - rental_supply_non_market), core housing need (core_housing_need), eviction filings rate (eviction_rate), and all neighbourhood outline / click / etc layers. Note that LEM is visible by default.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_aggregate_layers() %>%
#'   toggle_layer_visible("lem_percent")
add_blank_aggregate_layers <- function(map) {
  source_name <- "neighbourhoods_data"
  source_url <- "mapbox://purposeanalytics.4juivyoh"
  source_layer <- "neighbourhoods-0jaap1"
  opacity <- 0.75

  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = source_url,
      promoteId = "id"
    ),
    id = source_name
    ) %>%
    # LEM % ----
    add_blank_aggregate_layer_fill("lem_percent", source_name, source_layer, visibility = "visible") %>%
    # Rental supply ----
    ## Primary market ----
    add_blank_aggregate_layer_fill("rental_supply_primary", source_name, source_layer) %>%
    ## Condo ----
    add_blank_aggregate_layer_fill("rental_supply_condo", source_name, source_layer) %>%
    ## Non-condo ----
    add_blank_aggregate_layer_fill("rental_supply_non_condo", source_name, source_layer) %>%
    ## Non-market
    add_blank_aggregate_layer_fill("rental_supply_non_market", source_name, source_layer) %>%
    # Core housing need ----
    add_blank_aggregate_layer_fill("core_housing_need", source_name, source_layer) %>%
    # Evictions ----
    add_blank_aggregate_layer_fill("eviction_rate", source_name, source_layer) %>%
    # Vacancy rate ----
    add_blank_aggregate_layer_fill("vacancy_rate", source_name, source_layer) %>%
    # Neighbourhood ----
    ## Outline layer ----
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_line",
        "type" = "line",
        "source" = source_name,
        "source-layer" = "neighbourhoods-0jaap1",
        "paint" = list(
          "line-color" = default_line_colour,
          "line-width" = 1.5,
          "line-opacity" = 0.6
        )
      )
    ) %>%
    ## Add a "blank" layer for clicking on, that contains all neighbourhoods ----
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_click",
        "type" = "fill",
        "source" = source_name,
        "source-layer" = source_layer,
        "paint" = list(
          "fill-color" = "white",
          "fill-opacity" = 0
        )
      )
    ) %>%
    ## Add an actual layer for neighbourhoods that will be thickened ----
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_click_line",
        "type" = "line",
        "source" = source_name,
        "source-layer" = source_layer,
        filter = list("==", "neighbourhood", "none"),
        "paint" = list(
          "line-color" = main_colour,
          "line-width" = 4
        )
      )
    ) %>%
    ##  Hover layer ----
    mapboxer::add_layer(
      list(
        "id" = "neighbourhood_hover_line",
        "type" = "line",
        "source" = source_name,
        "source-layer" = source_layer,
        "paint" = list(
          "line-color" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), main_colour,
            "white"
          ),
          "line-width" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 4,
            1
          ),
          "line-opacity" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 1,
            0
          )
        )
      )
    )
}

add_blank_aggregate_layer_fill <- function(map, layer, source_name, source_layer, visibility = "none") {
  map %>%
    mapboxer::add_layer(
      list(
        "id" = layer,
        "type" = "fill",
        "source" = source_name,
        "source-layer" = source_layer,
        "layout" = list(
          "visibility" = visibility
        ),
        "paint" = list(
          "fill-color" = list(
            "case",
            list("==", c("get", layer), 0), low_high_legend_colors()[1],
            list("==", c("get", layer), 1), low_high_legend_colors()[2],
            list("==", c("get", layer), 2), low_high_legend_colors()[3],
            list("==", c("get", layer), 3), low_high_legend_colors()[4],
            list("==", c("get", layer), 4), low_high_legend_colors()[5],
            list("==", c("get", layer), 5), low_high_legend_colors()[6],
            # Defaults to 'white'
            "black"
          ),
          "fill-opacity" = 0.75
        )
      )
    )
}
