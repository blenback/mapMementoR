# Marathon Route Mapping

Create beautiful visualizations of marathon routes from GPX track files.

## Features

- Parse GPX files and extract route coordinates
- Create aesthetic route maps with start/finish markers
- Enhanced maps with elevation or distance gradient coloring
- Option to add Google Maps backgrounds (requires API key)
- Batch processing for multiple marathons
- Customizable colors, styling, and labels

## Quick Start

1. Install required R packages:
```r
install.packages(c("sf", "ggplot2", "ggmap", "dplyr", "xml2"))
```

2. Load the script:
```r
source("create_maps.R")
```

3. Create a simple marathon map:
```r
my_map <- create_marathon_map_simple(
  gpx_file = "data/marathon.gpx",
  competitor_name = "Runner Name",
  race_year = "2024",
  race_time = "3:45:23",
  city_name = "City Name"
)
```

## Function Reference

### `create_marathon_map_simple()`
Creates a basic route map without background tiles.

**Parameters:**
- `gpx_file`: Path to GPX file
- `competitor_name`: Runner's name for labels
- `race_year`: Year of the race
- `race_time`: Finish time (e.g., "3:45:23")
- `city_name`: City name for title (optional)
- `route_color`: Color for the route line (default: "#FF6B6B")
- `route_size`: Width of the route line (default: 1.2)

### `create_marathon_map_enhanced()`
Creates a route map with elevation or distance gradient coloring.

**Parameters:**
- Same as simple version, but `route_size` default is 1.5
- Automatically uses elevation data if available, otherwise distance

### `create_marathon_map_with_background()`
Creates a route map with Google Maps background tiles.

**Requirements:**
- Google Maps API key registered with `ggmap::register_google(key = "YOUR_KEY")`

**Additional Parameters:**
- `map_type`: Google Maps type ("terrain", "satellite", "roadmap", "hybrid")
- `zoom_level`: Map zoom level (default: 12)

### `create_marathon_series()`
Batch process multiple marathon GPX files.

**Parameters:**
- `race_data`: Data frame with columns: gpx_file, competitor_name, city_name, race_year, race_time
- `output_dir`: Output directory for saved maps (default: "marathon_maps")
- `enhanced`: Use enhanced version with gradients (default: TRUE)

## Example Usage

### Single Marathon
```r
# Simple version
map1 <- create_marathon_map_simple(
  gpx_file = "data/chicago.gpx",
  competitor_name = "John Doe",
  race_year = "2024",
  race_time = "3:45:23",
  city_name = "Chicago",
  route_color = "#E74C3C"
)

# Enhanced version with elevation coloring
map2 <- create_marathon_map_enhanced(
  gpx_file = "data/chicago.gpx",
  competitor_name = "John Doe",
  race_year = "2024",
  race_time = "3:45:23",
  city_name = "Chicago"
)

# Save maps
ggsave("chicago_simple.png", map1, width = 12, height = 8, dpi = 300)
ggsave("chicago_enhanced.png", map2, width = 12, height = 10, dpi = 300)
```

### Multiple Marathons
```r
# Create race data frame
races <- data.frame(
  gpx_file = c("data/chicago.gpx", "data/boston.gpx", "data/nyc.gpx"),
  competitor_name = c("John Doe", "John Doe", "John Doe"),
  city_name = c("Chicago", "Boston", "New York"),
  race_year = c("2024", "2023", "2022"),
  race_time = c("3:45:23", "3:52:10", "4:01:45"),
  route_color = c("#E74C3C", "#3498DB", "#2ECC71")
)

# Process all races
create_marathon_series(races, output_dir = "my_marathons", enhanced = TRUE)
```

## File Structure

```
Running_maps/
├── create_maps.R          # Main script
├── README.md             # This file
├── data/
│   └── chicago.gpx       # Example GPX file
└── output/               # Generated maps (created automatically)
```

## Google Maps API Setup (Optional)

For maps with satellite/terrain backgrounds:

1. Get a Google Cloud Platform account
2. Enable the Maps Static API
3. Create an API key
4. Register it in R:
```r
ggmap::register_google(key = "your_api_key_here")
```

## Notes

- GPX files should contain track points with latitude and longitude
- Elevation data is optional but will be used for gradient coloring if present
- Start and finish points are automatically detected from first/last track points
- Maps are saved as high-resolution PNG files (300 DPI)
- All functions return ggplot objects that can be further customized

## Troubleshooting

**"No symbol named 'get_map'"**: ggmap package not loaded or Google API key not set
**Empty/missing track data**: Check GPX file format and ensure track points exist
**Map bounds too wide**: Large gaps in GPS data can cause issues; try filtering track points