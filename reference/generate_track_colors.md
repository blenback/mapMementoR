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
#>  [1] "#d1af82" "#E7E5B1" "#CCE59E" "#9BE183" "#68DA78" "#52D196" "#48C6BD"
#>  [8] "#4A98BC" "#5773B7" "#746CB7" "#A183BF" "#C59ACA" "#D7ACCB" "#E0B8C5"

# Use complementary method
generate_track_colors("#d1af82", 14, method = "complementary")
#>  [1] "#d1af82" "#7692BF" "#D9CB98" "#EECCC1" "#2A54CD" "#B2B87C" "#D29FA3"
#>  [8] "#C5CAEA" "#86C136" "#DD578C" "#A4A4CE" "#D0E6C9" "#B54294" "#7062D3"
```
