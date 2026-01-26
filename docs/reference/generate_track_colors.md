# Generate visually distinct track colors from a base color

Creates a palette of colors that are visually distinct from each other
while maintaining harmony with the base route color. Uses HSL color
space to vary hue, saturation, and lightness for maximum distinction.

## Usage

``` r
generate_track_colors(base_color, n_colors, method = "hue_shift")
```

## Arguments

- base_color:

  Character string, a hex color code (e.g., "#d1af82")

- n_colors:

  Integer, number of distinct colors to generate

- method:

  Character, method for generating colors:

  - "hue_shift": Varies hue while maintaining similar
    saturation/lightness (default)

  - "complementary": Creates complementary and analogous colors

  - "gradient": Creates a gradient with varying lightness and saturation

## Value

Character vector of hex color codes

## Examples

``` r
# Generate 14 distinct colors from a gold base
generate_track_colors("#d1af82", 14)
#> Error in generate_track_colors("#d1af82", 14): could not find function "generate_track_colors"

# Use complementary method
generate_track_colors("#d1af82", 14, method = "complementary")
#> Error in generate_track_colors("#d1af82", 14, method = "complementary"): could not find function "generate_track_colors"
```
