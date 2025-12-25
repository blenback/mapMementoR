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
  orientation = "portrait",
  with_elevation = TRUE,
  with_OSM = TRUE,
  with_hillshade = FALSE
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

- orientation:

  Page orientation (e.g., "portrait", "landscape")

- with_elevation:

  Boolean to include elevation chart

- with_OSM:

  Boolean to include OSM background features

- with_hillshade:

  Boolean to include hillshade (elevation relief) background

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
  orientation = "portrait",
  with_elevation = TRUE,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  cache_data = TRUE
)
#> Error in memento_map_series(output_dir = "maps", styles = c("Dark", "Emerald"),     custom_styles = list(MyStyle = list(route_color = "#123456",         ...)), races_path = "data/races.yaml", dpi = 300, page_size = "A5",     orientation = "portrait", with_elevation = TRUE, with_OSM = TRUE,     with_hillshade = FALSE, cache_data = TRUE): could not find function "memento_map_series"
```
