main_colour <- "#08569A"
default_line_colour <- "#FFFFFF"
accent_colour <- mid_colour <- "#FFC20A"
low_colour <- "#3BA535"
high_colour <- "#9C62BE"
grey_colour <- "#B8B8B8"
base_size <- 14
bearing <- -17

layer_colours <- c(apartment_buildings = "#27a167", evictions_hearings = accent_colour, agi = "#fc8d59", tdf = "#ffffbf")

amenity_density_colours <- function() {
  stats::setNames(c("#3C95E3", accent_colour, "#fc8d59"), c("Low", "Medium", "High"))
}
