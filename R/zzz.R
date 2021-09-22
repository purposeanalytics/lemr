main_colour <- "#08569A"
default_line_colour <- "#FFFFFF"
accent_colour <- mid_colour <- "#FFC20A"
low_colour <- "#3BA535"
high_colour <- "#9C62BE"
grey_colour <- "#B8B8B8"
base_size <- 14
bearing <- -17

layer_colours <- c(
  apartment_buildings_private = "#27a167",
  agi_apartment = "#fc8d59",
  agi_other = "#1569ed",
  tdf = "#ffffbf"
)

rental_supply_colors <- function() {
  stats::setNames(
    c("#27a167", "#2ded92", "#0642a1", "#1569ed", "#f53216", "#f77460"),
    c("Apartment", "Non-Apartment", "Condo", "Non-Condo", "Toronto Community Housing", "Other Non-Market")
  )
}

low_high_legend_colors <- function() {
  c("white", "#CEE4F8", "#85BDED", "#3C95E3", "#0A6EC6", "#08569A")
}

amenity_density_colours <- function() {
  stats::setNames(c("#3C95E3", accent_colour, "#FB6B27"), c("Low", "Medium", "High"))
}
