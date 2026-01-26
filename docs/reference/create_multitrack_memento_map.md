# Create a map with multiple race routes from GPX files

This function generates a publication-quality map with multiple race
routes from GPX files, with optional labels for each route. It uses most
of the same functionality as create_memento_map but supports multiple
tracks on a single map.

## Usage

``` r
create_multitrack_memento_map(
  gpx_files,
  track_labels = NULL,
  route_colors = "#d1af82",
  with_labels = FALSE,
  label_spacing = 0.3,
  label_size = 3,
  map_title = NULL,
  cache_string = NULL,
  route_size = 1.2,
  bg_color = "#0a0e27",
  street_color = "#1a1f3a",
  highway_color = "#2d3250",
  water_color = "#1a2332",
  text_color = NULL,
  font_family = "Outfit-VariableFont_wght",
  output_dir,
  with_elevation = TRUE,
  dpi = 300,
  page_size = "A4",
  orientation = "portrait",
  base_size = 12,
  with_OSM = TRUE,
  cache_data = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL
)
```

## Arguments

- gpx_files:

  Character vector of paths to GPX files

- track_labels:

  Optional character vector of labels for each track (must match length
  of gpx_files) @param elev_labels Optional character vector of labels
  for elevation plots (must match length of gpx_files)

- route_colors:

  Character vector of colors for each route (if single color, applied to
  all)

- with_labels:

  Boolean to include text labels along routes using geomtextpath

- label_spacing:

  Spacing for labels along the path (default 0.3)

- label_size:

  Size of route labels (default 3)

- map_title:

  map_title string for map title, @param cache_string used for caching
  OSM data

- route_size:

  Size of the route lines (overridden by page size)

- bg_color:

  Background color for the map

- street_color:

  Color for streets

- highway_color:

  Color for highways

- water_color:

  Color for water bodies

- text_color:

  Color for title text (defaults to first route_color)

- font_family:

  Font family for text

- output_dir:

  Directory to save the output map

- with_elevation:

  Boolean to include elevation chart for first track

- dpi:

  DPI for saved image

- page_size:

  Page size (e.g., "A3", "A4")

- orientation:

  Page orientation: "portrait" or "landscape"

- base_size:

  Base font size for plot text

- with_OSM:

  Boolean to include OSM background features

- cache_data:

  Boolean; if TRUE, cache OSM data to disk (default)

- with_hillshade:

  Boolean to include hillshade (elevation relief) background

- components:

  Character vector of OSM components to include

- fade_directions:

  Character vector specifying fade directions

- crop_shape:

  Shape to crop the map to. Options: NULL, "circle", "ellipse"

## Value

Saves the map to the specified output directory (PNG file)

## Examples

``` r
create_multitrack_memento_map(
  gpx_files = c("data-raw/route1.gpx", "data-raw/route2.gpx"),
  track_labels = c("2023", "2024"),
  route_colors = c("#d1af82", "#82d1af"),
  with_labels = TRUE,
  map_title = "Marathon Comparison",
  bg_color = "#0a0e27",
  output_dir = "maps",
  dpi = 300,
  page_size = "A4",
  orientation = "portrait"
)
#> Error in create_multitrack_memento_map(gpx_files = c("data-raw/route1.gpx",     "data-raw/route2.gpx"), track_labels = c("2023", "2024"),     route_colors = c("#d1af82", "#82d1af"), with_labels = TRUE,     map_title = "Marathon Comparison", bg_color = "#0a0e27",     output_dir = "maps", dpi = 300, page_size = "A4", orientation = "portrait"): could not find function "create_multitrack_memento_map"
```
