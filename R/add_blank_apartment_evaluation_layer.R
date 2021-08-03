#' Add a blank apartment evaluation layer
#'
#' Add an invisible layer of \link{apartment_evaluation} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the apartment building layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_apartment_evaluation_layer() %>%
#'   toggle_layer_visible("apartment_evaluation")
add_blank_apartment_evaluation_layer <- function(map) {

  # Get geography
  apartment_building_evaluation <- lemur::apartment_building_evaluation %>%
    dplyr::filter(!is.na(.data$score)) %>%
    dplyr::select(.data$rsn, .data$score, .data$site_address) %>%
    dplyr::left_join(lemur::apartment_building_registry %>%
      dplyr::select(.data$rsn), by = "rsn") %>%
    sf::st_sf()

  # Set colours
  n <- 8
  apartment_building_evaluation <- apartment_building_evaluation %>%
    dplyr::mutate(score_bucket = cut(score, breaks = seq(20, 100, length.out = n)))

  score_bucket_colors <- dplyr::tibble(
    score_bucket = levels(apartment_building_evaluation[["score_bucket"]]),
    # color = grDevices::colorRampPalette(c("#ffe69d", "#841e13"))(n - 1)
    color = c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3B", "#FC4E2B", "#BD0026", "#800126")
  )

  apartment_building_evaluation <- apartment_building_evaluation %>%
    dplyr::left_join(score_bucket_colors, by = "score_bucket")

  map %>%
    # Add the layer
    mapboxer::add_circle_layer(source = mapboxer::as_mapbox_source(apartment_building_evaluation), id = "apartment_evaluation", circle_color = c("get", "color"), circle_blur = 0.5) %>%
    # Set the visibility to "none", so it's not shown
    mapboxer::set_layout_property(layer_id = "apartment_evaluation", "visibility", "none")
}
