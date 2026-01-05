##' Create a crop mask for map shapes
##'
##' This function generates a mask polygon that can be overlaid on a map to crop it to various shapes (circle, ellipse, etc.). The mask is a polygon with a hole in the desired shape.
##'
##' @param bbox Bounding box as a numeric vector c(xmin, ymin, xmax, ymax)
##' @param crop_shape Shape to crop to. Options: "circle", "ellipse"
##' @param bg_color Background color for the mask (should match plot background)
##' @return A list with 'mask' (sf polygon object) and 'adjusted_bbox' (bbox, unchanged)
##' @import sf
##' @export
##' @examples
##' bbox <- c(-0.1, 51.5, 0.1, 51.6)
##' result <- create_crop_mask(bbox, "circle", "#0a0e27")
create_crop_mask <- function(bbox, crop_shape, bg_color) {
  if (is.null(crop_shape) || !crop_shape %in% c("circle", "ellipse")) {
    return(list(mask = NULL, adjusted_bbox = bbox))
  }

  center_x <- base::mean(c(bbox[1], bbox[3]))
  center_y <- base::mean(c(bbox[2], bbox[4]))

  # Create the inner boundary (the shape we want to preserve)
  if (crop_shape == "circle") {
    # For circle, use the smaller dimension as diameter to ensure it fits
    width <- bbox[3] - bbox[1]
    height <- bbox[4] - bbox[2]
    radius <- base::min(width, height) / 2

    # Create circle coordinates
    angles <- base::seq(0, 2 * base::pi, length.out = 100)
    inner_coords <- base::data.frame(
      x = center_x + radius * base::cos(angles),
      y = center_y + radius * base::sin(angles)
    )
  } else if (crop_shape == "ellipse") {
    # Use bbox dimensions for ellipse (respects original aspect ratio)
    radius_x <- (bbox[3] - bbox[1]) / 2
    radius_y <- (bbox[4] - bbox[2]) / 2

    angles <- base::seq(0, 2 * base::pi, length.out = 100)
    inner_coords <- base::data.frame(
      x = center_x + radius_x * base::cos(angles),
      y = center_y + radius_y * base::sin(angles)
    )
  }

  # Create outer boundary (rectangular bbox)
  outer_coords <- base::data.frame(
    x = c(bbox[1], bbox[3], bbox[3], bbox[1], bbox[1]),
    y = c(bbox[2], bbox[2], bbox[4], bbox[4], bbox[2])
  )

  # Create polygon with hole: outer boundary with inner boundary cut out
  # The hole is created by reversing the direction of the inner polygon
  mask_polygon <- sf::st_polygon(
    list(
      base::as.matrix(outer_coords[, c("x", "y")]),
      base::as.matrix(inner_coords[base::nrow(inner_coords):1, c("x", "y")])
    )
  )

  # Convert to sfc and set CRS to match typical lat/lon
  mask_sf <- sf::st_sfc(mask_polygon, crs = 4326)

  return(list(mask = mask_sf, adjusted_bbox = bbox))
}
