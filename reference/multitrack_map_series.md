# Loop over multiple track sets and styles to create multitrack maps

This function processes multiple track sets (groups of GPX files) and
generates maps with different visual styles. If route colors are not
specified in the YAML configuration, it automatically generates visually
distinct colors using the generate_track_colors() function based on each
style's route_color.

## Usage

``` r
multitrack_map_series(
  output_dir = "outputs",
  styles = c("Dark"),
  custom_styles = NULL,
  track_sets_path = "track_sets.yaml",
  cache_data = TRUE,
  dpi = 300,
  page_size = "A4",
  base_size = 12,
  orientation = "portrait",
  with_elevation = TRUE,
  with_labels = FALSE,
  label_spacing = 0.5,
  label_size = 4,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL,
  track_color_method = "hue_shift"
)
```

## Arguments

- output_dir:

  Directory to save maps

- styles:

  Character vector of style names to use (built-in or custom) see
  `.mapMementoR_builtin_styles` in R/styles.R for built-in styles

- custom_styles:

  Optional named list of custom styles, each a list of color settings
  (route_color, bg_color, street_color, highway_color, water_color)

- track_sets_path:

  Path to track sets YAML file

- cache_data:

  Whether to cache OSM and hillshade data

- dpi:

  Image resolution in dots per inch

- page_size:

  Page size (e.g., "A5", "A4")

- base_size:

  Base font size for map text

- orientation:

  Page orientation (e.g., "portrait", "landscape")

- with_elevation:

  Boolean to include elevation charts

- with_labels:

  Boolean to include text labels along routes

- label_spacing:

  Spacing for labels along the path

- label_size:

  Size of route labels

- with_OSM:

  Boolean to include OSM background features

- with_hillshade:

  Boolean to include hillshade (elevation relief) background

- components:

  Character vector specifying which OSM components to include. Any
  combination of 'highways', 'streets', 'water', 'coast'. Defaults to
  all.

- fade_directions:

  Character vector specifying which sides to apply fade gradients to.
  Any combination of 'top', 'bottom', 'left', 'right'. Defaults to
  c('top', 'bottom').

- crop_shape:

  Optional shape to crop map to. Options: "circle", "ellipse". If NULL,
  no cropping is applied.

- track_color_method:

  Method for generating track colors when not specified. Options:

  - "hue_shift": Varies hue while maintaining similar
    saturation/lightness (default)

  - "complementary": Creates complementary and analogous colors

  - "gradient": Creates a gradient with varying lightness and saturation

## Value

Saves maps for all track sets and styles

## Examples

``` r
multitrack_map_series(
  output_dir = "outputs",
  styles = c("Dark", "Emerald"),
  custom_styles = list(MyStyle = list(route_color = "#123456", ...)),
  track_sets_path = "data-raw/track_sets.yaml",
  dpi = 300,
  page_size = "A4",
  base_size = 12,
  orientation = "portrait",
  with_elevation = TRUE,
  with_labels = FALSE,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  cache_data = TRUE,
  components = c("highways", "streets", "water", "coast")
)
#> Warning: cannot open file 'data-raw/track_sets.yaml': No such file or directory
#> Error in file(file, "rt", encoding = fileEncoding): cannot open the connection
```
