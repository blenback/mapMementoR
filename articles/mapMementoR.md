# Introduction to mapMementoR

## Introduction to mapMementoR

**mapMementoR** is an R package for creating beautiful, print-ready maps
of running routes, races, hiking trails, and multi-day adventures from
GPX data. The package combines GPS track data with OpenStreetMap
backgrounds, elevation profiles, and customizable styling to produce
publication-quality visualizations.

### Installation

``` r
# Install from GitHub
pak::pak("blenback/mapMementoR")
```

``` r
library(mapMementoR)
```

### Core Concepts

#### Two Main Workflows

1.  **Single Route Maps**
    ([`memento_map_series()`](blenback.github.io/mapMementoR/reference/memento_map_series.md)):
    Create maps showing individual routes with performance data (e.g.,
    race times across multiple years)

2.  **Multi-Track Maps**
    ([`multitrack_map_series()`](blenback.github.io/mapMementoR/reference/multitrack_map_series.md)):
    Combine multiple GPS tracks on a single map (e.g., relay stages,
    multi-day events)

#### Configuration via YAML

Both workflows use YAML files to define your data:

- **Races YAML**: Defines individual races with GPX files and
  performance entries
- **Track Sets YAML**: Defines collections of tracks to plot together

#### Built-in Styles

Six professionally designed color schemes: - **Dark**: Warm gold on deep
navy - **Emerald**: Forest green on cream - **Nautical**: Blue-grey on
warm cream - **Zen**: Charcoal on light grey - **Ghost**: White on dark
background - **Obsidian**: Bright gold on black

### Quick Start: Single Race Map

#### Step 1: Prepare Your Data

Create a YAML file defining your races. The package includes a demo file
at `inst/extdata/races.yaml`:

``` yaml
# races.yaml
races:
  - gpx_file: "inst/extdata/london.gpx"
    competitor_name: "John Doe"
    location: "London"
    entries:
      - race_year: "2025"
        race_time: "2:28:09"
      - race_year: "2024"
        race_time: "2:30:45"
```

#### Step 2: Generate Maps

``` r
memento_map_series(
  output_dir = "my_maps",
  races_path = "my_races.yaml",
  styles = c("Dark", "Emerald"),
  page_size = "A4",
  orientation = "portrait",
  dpi = 300,
  with_elevation = TRUE,
  with_OSM = TRUE
)
```

This generates maps for all races in all selected styles, organized in
folders:

    my_maps/
    ├── Dark/
    │   └── A4/
    │       └── full_page/
    │           └── London.png
    └── Emerald/
        └── A4/
            └── full_page/
                └── London.png

### Multi-Track Maps

#### Creating a Track Set

For events with multiple stages or tracks. The package includes a demo
file at `inst/extdata/track_sets.yaml`:

``` yaml
# track_sets.yaml
track_sets:
  - map_title: "City Relay\n2024"
    gpx_files:
      - "inst/extdata/leg1.gpx"
      - "inst/extdata/leg2.gpx"
      - "inst/extdata/leg3.gpx"
      - "inst/extdata/leg4.gpx"
    track_labels:
      - "Leg 1"
      - "Leg 2"
      - "Leg 3"
      - "Leg 4"
    elev_labels:
      - "Sarah"
      - "Mike"
      - "Emma"
      - "Tom"
    font_family: "Outfit-VariableFont_wght"
```

#### Generate Multi-Track Maps

``` r
multitrack_map_series(
  output_dir = "outputs",
  track_sets_path = "track_sets.yaml",
  styles = c("Dark", "Zen"),
  page_size = "A3",
  orientation = "landscape",
  with_elevation = TRUE,
  with_labels = TRUE,
  label_spacing = 0.5,
  track_color_method = "hue_shift"
)
```

### Customization

#### Custom Color Schemes

Create your own styles alongside built-in ones:

``` r
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark", "Sunset"),
  custom_styles = list(
    Sunset = list(
      route_color = "#FF6B35",    # Coral
      bg_color = "#004E89",        # Deep blue
      street_color = "#1A659E",    # Medium blue
      highway_color = "#2E86AB",   # Lighter blue
      water_color = "#1A659E"      # Ocean blue
    )
  ),
  page_size = "A4",
  dpi = 300
)
```

#### Map Components

Control which OpenStreetMap features to include:

``` r
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark"),
  components = c(
    "highways",  # Major roads
    "streets",   # Minor roads
    "water",     # Water bodies (lakes, rivers)
    "coast"      # Coastlines and seas
  ),
  with_hillshade = TRUE,  # Add terrain shading
  fade_directions = c("top", "bottom")  # Fade edges
)
```

#### Page Formats and Orientation

``` r
# Portrait A4 for framing
memento_map_series(
  races_path = "my_races.yaml",
  page_size = "A4",
  orientation = "portrait",
  dpi = 300
)

# Landscape A3 for wall posters
memento_map_series(
  races_path = "my_races.yaml",
  page_size = "A3",
  orientation = "landscape",
  dpi = 300
)
```

Supported sizes: A5, A4, A3, A2, A1, A0

#### Circular Cropping

Create artistic circular maps:

``` r
memento_map_series(
  races_path = "my_races.yaml",
  crop_shape = "circle",  # or "ellipse"
  page_size = "A4"
)
```

### Advanced Features

#### Automatic Color Generation

For multi-track maps, colors can be auto-generated:

``` r
multitrack_map_series(
  track_sets_path = "track_sets.yaml",
  track_color_method = "hue_shift"  # Options: "hue_shift", "complementary", "gradient"
)
```

- **hue_shift**: Varies hue while maintaining saturation/lightness
- **complementary**: Creates complementary and analogous colors
- **gradient**: Gradient with varying lightness and saturation

#### Data Caching

OpenStreetMap data is cached by default:

``` r
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = TRUE  # Default: speeds up regeneration
)

# Force fresh data download
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = FALSE
)
```

Cache files are stored in `data_cache_tmp/` as `.rds` files. Delete them
to refresh OSM data for a location.

#### Power of 10 Integration

Automatically import UK running race data:

``` r
save_powerof10_to_yaml(
  first_name = "Alex",
  surname = "Black",
  club = "North Shields Poly",
  event = c("HM", "Mar"),
  year = c(2023, 2024, 2025),
  yaml_path = "alex_black_races.yaml"
)
```

Then add GPX file paths to the generated YAML and create maps.

### Low-Level API

For programmatic control, use core functions directly:

#### Single Map

``` r
create_memento_map(
  gpx_file = system.file("extdata", "london.gpx", package = "mapMementoR"),
  competitor_name = "John Doe",
  location = "London",
  entries = list(
    list(race_year = "2025", race_time = "2:28:09"),
    list(race_year = "2024", race_time = "2:30:45")
  ),
  route_color = "#d1af82",
  bg_color = "#0a0e27",
  street_color = "#1a1f3a",
  highway_color = "#2d3250",
  water_color = "#1a2332",
  output_dir = "maps",
  dpi = 300,
  page_size = "A4",
  orientation = "portrait",
  base_size = 12,
  with_elevation = TRUE,
  with_OSM = TRUE
)
```

#### Multi-Track Map

``` r
create_multitrack_memento_map(
  gpx_files = system.file("extdata", 
                          c("leg1.gpx", "leg2.gpx", "leg3.gpx", "leg4.gpx"),
                          package = "mapMementoR"),
  track_labels = c("Leg 1", "Leg 2", "Leg 3", "Leg 4"),
  elev_labels = c("Sarah", "Mike", "Emma", "Tom"),
  route_colors = c("#d1af82", "#82b4d1", "#d182a8", "#a8d182"),
  map_title = "City Relay\n2024",
  output_dir = "maps",
  page_size = "A3",
  orientation = "landscape",
  with_labels = TRUE,
  label_size = 4
)
```

### Helper Functions

#### Parse GPX Files

``` r
# Use the demo London GPX file
gpx_path <- system.file("extdata", "london.gpx", package = "mapMementoR")
track_data <- parse_gpx(gpx_path)
head(track_data)
# Returns data.frame with columns: lat, lon, ele, time
```

#### Get Track Statistics

``` r
stats <- get_track_stats(track_data)
# Returns: total_distance, elevation_gain, elevation_loss, etc.
```

#### Generate Color Palettes

``` r
colors <- generate_track_colors(
  base_color = "#d1af82",
  n_colors = 5,
  method = "hue_shift"
)
```

#### Page Dimensions

``` r
dims <- get_page_dimensions(page_size = "A4", orientation = "portrait")
# Returns: list(width = 210, height = 297) in mm
```

### Workflow Examples

#### Example 1: Marathon Progression

Track improvement across multiple years:

``` yaml
# marathons.yaml
races:
  - gpx_file: "data-raw/berlin.gpx"
    competitor_name: "Runner Name"
    event: "Mar"
    location: "Berlin"
    entries:
      - race_year: "2025"
        race_time: "2:58:12"
      - race_year: "2024"
        race_time: "3:05:34"
      - race_year: "2023"
        race_time: "3:15:47"
```

``` r
memento_map_series(
  races_path = "marathons.yaml",
  styles = c("Dark", "Zen"),
  page_size = "A3",
  dpi = 300
)
```

#### Example 2: Multi-Day Hiking Trip

``` yaml
# trek.yaml
track_sets:
  - map_title: "Tour du Mont Blanc"
    gpx_files:
      - "tmb_stage1.gpx"
      - "tmb_stage2.gpx"
      - "tmb_stage3.gpx"
      - "tmb_stage4.gpx"
      - "tmb_stage5.gpx"
    track_labels:
      - "Les Houches to Les Contamines"
      - "Les Contamines to Les Chapieux"
      - "Les Chapieux to Courmayeur"
      - "Courmayeur to La Fouly"
      - "La Fouly to Champex"
    elev_labels:
      - "Day 1"
      - "Day 2"
      - "Day 3"
      - "Day 4"
      - "Day 5"
```

``` r
multitrack_map_series(
  track_sets_path = "trek.yaml",
  styles = c("Emerald", "Nautical"),
  page_size = "A2",
  orientation = "landscape",
  with_elevation = TRUE,
  with_hillshade = TRUE,
  track_color_method = "gradient"
)
```

#### Example 3: Relay Race

``` yaml
# relay.yaml
track_sets:
  - map_title: "City Relay\n2024"
    gpx_files:
      - "leg1.gpx"
      - "leg2.gpx"
      - "leg3.gpx"
      - "leg4.gpx"
    track_labels:
      - "Leg 1 - Sarah"
      - "Leg 2 - Mike"
      - "Leg 3 - Emma"
      - "Leg 4 - Tom"
    route_colors:
      - "#FF6B6B"
      - "#4ECDC4"
      - "#45B7D1"
      - "#FFA07A"
```

``` r
multitrack_map_series(
  track_sets_path = "relay.yaml",
  styles = c("Ghost"),
  page_size = "A3",
  with_labels = TRUE,
  with_elevation = TRUE
)
```

### Troubleshooting

#### GPX Issues

``` r
# Check if GPX can be parsed
track <- parse_gpx("data-raw/problematic.gpx")
nrow(track)  # Should be > 0

# If empty, check GPX structure:
# - Must contain <trkpt> elements
# - Must have lat/lon attributes
# - Re-export from source if corrupted
```

#### Missing OSM Features

``` r
# Clear cache for a location
unlink("data_cache_tmp/highways_london.rds")
unlink("data_cache_tmp/streets_london.rds")
unlink("data_cache_tmp/water_london.rds")

# Regenerate with fresh data
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = FALSE  # Force re-download
)
```

#### Memory Issues

``` r
# Reduce resolution
memento_map_series(
  races_path = "my_races.yaml",
  dpi = 150  # Lower than 300
)

# Process one style at a time
for (style in c("Dark", "Emerald", "Zen")) {
  memento_map_series(
    races_path = "my_races.yaml",
    styles = style
  )
}
```

### Tips and Best Practices

1.  **GPS Quality**: Ensure good satellite signal during recording
2.  **Test First**: Generate one map before batch processing
3.  **Color Contrast**: Ensure route color contrasts with background
4.  **Naming**: Use concise location names for better layout
5.  **Print Preview**: View maps at intended size before final print
6.  **Cache Management**: Keep cache enabled for faster iteration
7.  **Font Selection**: Test custom fonts before large batch runs

### Next Steps

- Explore the [README](blenback.github.io/mapMementoR/README.md) for
  quick reference
- Check out the [function
  reference](blenback.github.io/mapMementoR/reference/index.md)
- Try different color combinations
- Create maps for your personal races and adventures
- Share your creations!

### Support

For bugs, feature requests, or questions:

- [Open an issue on
  GitHub](https://github.com/blenback/mapMementoR/issues)
- Check existing issues for solutions
- Contribute improvements via pull requests
