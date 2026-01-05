# Loop over multiple races and styles to create maps

Loop over multiple races and styles to create maps

## Usage

``` r
memento_map_series(
  output_dir = "maps",
  styles = c("Dark"),
  custom_styles = NULL,
  races_path = "races.yaml",
  cache_data = TRUE,
  dpi = 300,
  page_size = "A5",
  base_size,
  orientation = "portrait",
  with_elevation = TRUE,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL
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

- races_path:

  Path to races YAML

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

  Boolean to include elevation chart

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

## Value

Saves maps for all races and styles

## Examples

``` r
memento_map_series(
  output_dir = "maps",
  styles = c("Dark", "Emerald"),
  custom_styles = list(MyStyle = list(route_color = "#123456", ...)),
  races_path = "data/races.yaml",
  dpi = 300,
  page_size = "A5",
  base_size = 18,
  orientation = "portrait",
  with_elevation = TRUE,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  cache_data = TRUE,
  components = c("highways", "streets", "water", "coast")
)
#> Warning: cannot open file 'data/races.yaml': No such file or directory
#> Error in file(file, "rt", encoding = fileEncoding): cannot open the connection
```
