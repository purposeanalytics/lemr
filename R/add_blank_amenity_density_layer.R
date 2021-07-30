#' Add a blank amenity density layer
#'
#' Add an empty layer of \link{amenity_density} to a map (created via \link{map_toronto}). The purpose of this function is to allow for toggling the layer on and off, via \link{toggle_layer_visible} and \link{toggle_layer_invisible}.
#'
#' @param map Map created via \link{map_toronto}
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' map_toronto() %>%
#'   add_blank_amenity_density_layer()
add_blank_amenity_density_layer <- function(map) {
  amenity_density_colours <- tibble::tribble(
    ~amenity_dense, ~colour,
    "Low", low_colour,
    "Medium", accent_colour,
    "High", high_colour,
    "Unknown", "white"
  )

  amenity_density_with_colours <- amenity_density %>%
    dplyr::left_join(amenity_density_colours, by = "amenity_dense")

  map %>%
    # Add the layers (one for the fill, one for the outline)
    mapboxer::add_fill_layer(source = mapboxer::as_mapbox_source(amenity_density_with_colours), fill_color = c("get", "colour"), fill_opacity = 0.75, id = "amenity_density_fill") %>%
    mapboxer::add_line_layer(source = mapboxer::as_mapbox_source(amenity_density_with_colours), line_color = "black", line_width = 0.5, id = "amenity_density_outline") %>%
  # Set the visibility to "none", so they're not shown
  mapboxer::set_layout_property(layer_id = "amenity_density_fill", "visibility", "none") %>%
    mapboxer::set_layout_property(layer_id = "amenity_density_outline", "visibility", "none")
}
