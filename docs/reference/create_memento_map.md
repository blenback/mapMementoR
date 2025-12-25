# Create a map of a race route with OSM background and elevation chart

This function generates a publication-quality map of a race route from a
GPX file, with OpenStreetMap features, elevation chart, and customizable
appearance. All theme and style arguments are handled directly in the
function (no external theme required).

## Usage

``` r
create_memento_map(
  gpx_file,
  competitor_name,
  entries,
  location = NULL,
  route_color = "#d1af82",
  route_size = 1.2,
  bg_color = "#0a0e27",
  street_color = "#1a1f3a",
  highway_color = "#2d3250",
  water_color = "#1a2332",
  text_color = NULL,
  font_family = "Outfit-VariableFont_wght",
  output_dir,
  with_elevation = TRUE,
  dpi,
  page_size,
  orientation,
  base_size = 12,
  with_OSM = TRUE,
  cache_data = TRUE,
  with_hillshade = FALSE
)
```

## Arguments

- gpx_file:

  Path to GPX file

- competitor_name:

  Name of the competitor

- entries:

  List of race entries (each entry should have `race_year` and
  `race_time`)

- location:

  Location string for map title

- route_color:

  Color for the route line

- route_size:

  Size of the route line (overridden by page size)

- bg_color:

  Background color for the map

- street_color:

  Color for streets

- highway_color:

  Color for highways

- water_color:

  Color for water bodies

- text_color:

  Color for title and subtitle text (defaults to route_color)

- font_family:

  Font family for text

- output_dir:

  Directory to save the output map

- with_elevation:

  Boolean to include elevation chart

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

  Boolean; if TRUE, cache OSM data to disk (default), if FALSE always
  fetch fresh OSM data

- with_hillshade:

  Boolean to include hillshade (elevation relief) background

## Value

Saves the map to the specified output directory (PNG file)

## Examples

``` r
create_memento_map(
gpx_file = "data/sample_race.gpx",
competitor_name = "John Doe",
entries = list(
 list(race_year = "2021", race_time = "3:15:30"),
list(race_year = "2022", race_time = "3:10:45")
),
location = "Sample Marathon",
route_color = "#d1af82",
bg_color = "#0a0e27",
output_dir = "maps",
with_elevation = TRUE,
dpi = 300,
page_size = "A3",
orientation = "portrait",
base_size = 12,
with_OSM = TRUE,
cache_data = TRUE,
with_hillshade = FALSE
)
#> Error in create_memento_map(gpx_file = "data/sample_race.gpx", competitor_name = "John Doe",     entries = list(list(race_year = "2021", race_time = "3:15:30"),         list(race_year = "2022", race_time = "3:10:45")), location = "Sample Marathon",     route_color = "#d1af82", bg_color = "#0a0e27", output_dir = "maps",     with_elevation = TRUE, dpi = 300, page_size = "A3", orientation = "portrait",     base_size = 12, with_OSM = TRUE, cache_data = TRUE, with_hillshade = FALSE): could not find function "create_memento_map"
```
