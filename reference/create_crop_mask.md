# Create a crop mask for map shapes

This function generates a mask polygon that can be overlaid on a map to
crop it to various shapes (circle, ellipse, etc.). The mask is a polygon
with a hole in the desired shape.

## Usage

``` r
create_crop_mask(bbox, crop_shape, bg_color)
```

## Arguments

- bbox:

  Bounding box as a numeric vector c(xmin, ymin, xmax, ymax)

- crop_shape:

  Shape to crop to. Options: "circle", "ellipse"

- bg_color:

  Background color for the mask (should match plot background)

## Value

A list with 'mask' (sf polygon object) and 'adjusted_bbox' (bbox,
unchanged)

## Examples

``` r
bbox <- c(-0.1, 51.5, 0.1, 51.6)
result <- create_crop_mask(bbox, "circle", "#0a0e27")
```
