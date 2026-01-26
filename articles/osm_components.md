# OpenStreetMap Components and Features

## Introduction

`mapMementoR` leverages OpenStreetMap (OSM) data to create rich,
detailed background layers for your route maps. This vignette explains
how to control which map features appear, customize their appearance,
and optimize map rendering for different use cases.

### What is OpenStreetMap?

OpenStreetMap is a free, editable map of the world created by
volunteers. It contains detailed data about roads, buildings, natural
features, and points of interest. `mapMementoR` uses this data to create
contextual backgrounds that make your routes meaningful and beautiful.

## Available Components

### Core Components

The package supports four main OSM component types:

1.  **highways**: Major roads and motorways
2.  **streets**: Minor roads, residential streets, paths
3.  **water**: Rivers, lakes, ponds, streams
4.  **coast**: Coastlines and sea areas

### Component Selection

``` r
library(mapMementoR)

# Include all components (default)
memento_map_series(
  races_path = system.file("extdata", "races.yaml", package = "mapMementoR"),
  output_dir = "maps",
  styles = c("Dark"),
  components = c("highways", "streets", "water", "coast")
)

# Include only roads (no water features)
memento_map_series(
  races_path = "my_races.yaml",
  output_dir = "maps",
  styles = c("Dark"),
  components = c("highways", "streets")
)

# Minimal map (only major roads)
memento_map_series(
  races_path = "my_races.yaml",
  output_dir = "maps",
  styles = c("Dark"),
  components = c("highways")
)

# Water-focused (useful for coastal/waterfront routes)
memento_map_series(
  races_path = "my_races.yaml",
  output_dir = "maps",
  styles = c("Nautical"),
  components = c("water", "coast")
)
```

## Component Details

### Highways

**What’s included**: Motorways, trunk roads, primary roads, major
arterials

**Best for**: - Urban marathons - Major road races - Routes with
significant highway sections

**Rendering**: - Thicker lines than streets - More prominent color -
Higher z-order (drawn above streets)

``` r
# Highway-only map for city marathon
memento_map_series(
  races_path = "city_marathon.yaml",
  styles = c("Dark"),
  components = c("highways"),  # Only major roads
  page_size = "A3"
)
```

### Streets

**What’s included**: Residential streets, service roads, paths, tertiary
roads

**Best for**: - Neighborhood runs - Detailed urban routes - Trail
systems in parks

**Rendering**: - Thinner lines than highways - Subtle color (close to
background) - Lower z-order (drawn below highways)

``` r
# Detailed street-level map
memento_map_series(
  races_path = "neighborhood_run.yaml",
  styles = c("Zen"),
  components = c("streets", "highways"),  # Full road network
  page_size = "A4"
)
```

### Water

**What’s included**: Rivers, streams, lakes, ponds, reservoirs, canals

**Best for**: - Lakefront routes - River trail runs - Routes near water
features

**Rendering**: - Polygon fills for lakes/ponds - Line features for
rivers/streams - Uses water_color from style

``` r
# Emphasize water features
memento_map_series(
  races_path = "lakefront_race.yaml",
  styles = c("Nautical"),
  components = c("water", "coast", "highways"),
  page_size = "A4"
)
```

### Coast

**What’s included**: Coastlines, sea areas, ocean boundaries

**Best for**: - Coastal routes - Beach runs - Ocean-adjacent races

**Rendering**: - Large polygon fills for sea areas - Uses water_color
from style - Complements water component

``` r
# Coastal marathon
memento_map_series(
  races_path = "coastal_marathon.yaml",
  styles = c("Nautical"),
  components = c("coast", "highways", "streets"),
  page_size = "A3"
)
```

## Controlling OSM Features

### Disabling All OSM Features

For minimalist maps showing only your route:

``` r
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Zen"),
  with_OSM = FALSE,  # No background features
  page_size = "A4"
)
```

### Selective Component Display

Choose components based on route characteristics:

``` r
# Urban route: roads only
memento_map_series(
  races_path = "urban_race.yaml",
  components = c("highways", "streets")
)

# Park trail: paths and water
memento_map_series(
  races_path = "park_trail.yaml",
  components = c("streets", "water")
)

# Coastal road race: everything
memento_map_series(
  races_path = "coastal_race.yaml",
  components = c("highways", "streets", "water", "coast")
)
```

## Advanced Features

### Hillshade (Elevation Relief)

Add terrain visualization to show topography:

``` r
# Add hillshade for mountainous routes
memento_map_series(
  races_path = "mountain_race.yaml",
  styles = c("Dark"),
  components = c("highways", "streets", "water"),
  with_hillshade = TRUE,  # Add terrain relief
  page_size = "A4"
)

# Hillshade without OSM features (pure terrain)
memento_map_series(
  races_path = "trail_run.yaml",
  styles = c("Emerald"),
  with_OSM = FALSE,
  with_hillshade = TRUE,
  page_size = "A4"
)
```

**Hillshade characteristics**: - Automatically adjusts to background
color (light/dark) - Alpha transparency blends with OSM features -
Generated from elevation data - Sun angle: 60 degrees (customizable in
low-level functions)

### Fade Directions

Control edge fading for artistic effect:

``` r
# Default: fade top and bottom
memento_map_series(
  races_path = "my_races.yaml",
  fade_directions = c("top", "bottom")
)

# Fade all four sides
memento_map_series(
  races_path = "my_races.yaml",
  fade_directions = c("top", "bottom", "left", "right")
)

# No fading
memento_map_series(
  races_path = "my_races.yaml",
  fade_directions = c()  # Empty vector = no fade
)

# Only vertical fade
memento_map_series(
  races_path = "my_races.yaml",
  fade_directions = c("top", "bottom")
)
```

### Cropping Shapes

Create artistic cropped compositions:

``` r
# Circular crop
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark"),
  crop_shape = "circle",
  components = c("highways", "streets", "water"),
  page_size = "A4"
)

# Ellipse crop
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Nautical"),
  crop_shape = "ellipse",
  page_size = "A4"
)

# No crop (default)
memento_map_series(
  races_path = "my_races.yaml",
  crop_shape = NULL,
  page_size = "A4"
)
```

## Data Caching

### How Caching Works

OSM data is downloaded once and cached locally for faster regeneration:

``` r
# First run: downloads OSM data
memento_map_series(
  races_path = "london_race.yaml",
  cache_data = TRUE  # Default
)

# Subsequent runs: uses cached data (much faster)
memento_map_series(
  races_path = "london_race.yaml",
  styles = c("Dark", "Zen", "Emerald")  # Try different styles quickly
)
```

### Cache Files

Cache files are stored in `data_cache_tmp/`:

    data_cache_tmp/
    ├── highways_london.rds
    ├── streets_london.rds
    ├── water_london.rds
    ├── coastlines_london.rds
    └── hillshade_london.rds

### Refreshing Cache

``` r
# Force fresh download (ignores cache)
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = FALSE
)
```

Or manually delete cache files:

``` r
# R command
unlink("data_cache_tmp/highways_london.rds")
unlink("data_cache_tmp/streets_london.rds")

# Or delete entire cache directory
unlink("data_cache_tmp", recursive = TRUE)
```

### Cache Location Parameter

The cache location is determined by the `location` field in your YAML or
a `cache_string` parameter in low-level functions:

``` r
# Uses location name for cache files
create_memento_map(
  gpx_file = "london.gpx",
  location = "London",  # Creates *_london.rds files
  cache_data = TRUE
)

# Multi-track maps use cache_string
create_multitrack_memento_map(
  gpx_files = c("leg1.gpx", "leg2.gpx"),
  cache_string = "relay_2024",  # Creates *_relay_2024.rds files
  cache_data = TRUE
)
```

## Performance Optimization

### Component Trade-offs

More components = longer processing time + richer detail:

``` r
# Fastest: No OSM features
memento_map_series(
  races_path = "my_races.yaml",
  with_OSM = FALSE
)

# Fast: Highways only
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways")
)

# Moderate: Roads only
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways", "streets")
)

# Slower: All components
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways", "streets", "water", "coast")
)

# Slowest: All components + hillshade
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways", "streets", "water", "coast"),
  with_hillshade = TRUE
)
```

### Batch Processing Strategy

``` r
# Strategy 1: Cache all data first with one style
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark"),
  cache_data = TRUE
)

# Strategy 2: Generate other styles using cached data
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Emerald", "Zen", "Ghost", "Obsidian", "Nautical"),
  cache_data = TRUE  # Will use existing cache
)
```

## Use Case Examples

### Urban Marathon

``` r
memento_map_series(
  races_path = "chicago_marathon.yaml",
  styles = c("Dark"),
  components = c("highways", "streets", "water"),
  with_elevation = TRUE,
  page_size = "A3",
  dpi = 300
)
```

**Why this configuration**: - Full road network shows urban context -
Water features (Chicago River, Lake Michigan) add interest - Elevation
profile shows route topology - Dark style provides modern, professional
look

### Trail Run

``` r
memento_map_series(
  races_path = "mountain_trail.yaml",
  styles = c("Emerald"),
  components = c("streets"),  # Trails classified as streets
  with_hillshade = TRUE,
  with_elevation = TRUE,
  page_size = "A4",
  dpi = 300
)
```

**Why this configuration**: - Streets component includes trail paths -
Hillshade shows terrain and elevation changes - Emerald style
complements natural setting - Elevation chart shows climb profile

### Coastal Half Marathon

``` r
memento_map_series(
  races_path = "great_north_run.yaml",
  styles = c("Nautical"),
  components = c("highways", "streets", "water", "coast"),
  with_elevation = TRUE,
  fade_directions = c("top", "bottom"),
  page_size = "A4",
  dpi = 300
)
```

**Why this configuration**: - All components show coastal context -
Nautical style emphasizes maritime theme - Coast component shows ocean
proximity - Fading creates polished presentation

### Minimalist Design

``` r
memento_map_series(
  races_path = "parkrun.yaml",
  styles = c("Zen"),
  components = c("highways"),  # Only major roads for context
  with_elevation = FALSE,
  crop_shape = "circle",
  page_size = "A5",
  dpi = 300
)
```

**Why this configuration**: - Minimal features for clean design -
Circular crop creates focused composition - Zen style maintains
minimalist aesthetic - Small size suitable for social media or cards

## Troubleshooting

### Missing Features

**Problem**: Expected features don’t appear on map

**Solutions**:

``` r
# 1. Verify components are enabled
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways", "streets", "water", "coast")  # Explicit list
)

# 2. Clear cache and re-download
unlink("data_cache_tmp", recursive = TRUE)
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = TRUE
)

# 3. Check if with_OSM is enabled
memento_map_series(
  races_path = "my_races.yaml",
  with_OSM = TRUE  # Must be TRUE
)
```

### Too Much Detail

**Problem**: Map is cluttered with too many features

**Solutions**:

``` r
# 1. Reduce components
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways")  # Only major roads
)

# 2. Disable OSM entirely
memento_map_series(
  races_path = "my_races.yaml",
  with_OSM = FALSE
)

# 3. Use minimal style with subtle colors
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Zen"),
  components = c("highways")
)
```

### Slow Processing

**Problem**: Map generation takes too long

**Solutions**:

``` r
# 1. Enable caching
memento_map_series(
  races_path = "my_races.yaml",
  cache_data = TRUE  # Should be default
)

# 2. Reduce components
memento_map_series(
  races_path = "my_races.yaml",
  components = c("highways"),  # Fewer components = faster
  with_hillshade = FALSE  # Hillshade is slowest
)

# 3. Process in batches
# First, cache data with one map
memento_map_series(
  races_path = "single_race.yaml",
  styles = c("Dark")
)
# Then, generate multiple styles quickly
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark", "Zen", "Emerald")
)
```

### Color Visibility Issues

**Problem**: OSM features too prominent or invisible

**Solutions**:

``` r
# 1. Adjust style colors
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Custom"),
  custom_styles = list(
    Custom = list(
      route_color = "#d1af82",    # Keep prominent
      bg_color = "#0a0e27",        
      street_color = "#1a1f3a",    # Very subtle
      highway_color = "#2d3250",   # Moderate visibility
      water_color = "#1a2332"      # Subtle
    )
  )
)

# 2. Use appropriate built-in style
memento_map_series(
  races_path = "my_races.yaml",
  styles = c("Dark")  # Known good contrast
)
```

## Best Practices

### Component Selection Guidelines

1.  **Urban routes**: Include `highways` and `streets` for full context
2.  **Trail runs**: Use `streets` (trails) with `hillshade`
3.  **Coastal routes**: Add `water` and `coast` components
4.  **Minimalist designs**: Use only `highways` or disable OSM entirely
5.  **Water-focused**: `water` + `coast` with Nautical style

### Performance Guidelines

1.  Always enable `cache_data = TRUE` (default)
2.  Generate one map first to cache OSM data
3.  Then batch process multiple styles using cached data
4.  Use fewer components for faster processing
5.  Disable `hillshade` unless needed for terrain routes

### Visual Guidelines

1.  Match components to route context (coastal, urban, trail)
2.  Use appropriate styles for feature visibility
3.  Consider final medium (print vs screen) when selecting components
4.  Test with single map before batch processing
5.  Keep `street_color` subtle to avoid clutter

## Conclusion

OpenStreetMap components provide rich contextual backgrounds that
enhance route visualization. By understanding how to control which
features appear and how they’re rendered, you can create maps that
perfectly balance detail and clarity.

**Key Takeaways**:

- Use `components` parameter to control which features appear
- Enable `cache_data` for faster processing
- Match components to your route type and context
- Use `with_hillshade` for terrain visualization
- Consider performance trade-offs when selecting components

For information on styling these components, see the [Built-in
Styles](blenback.github.io/mapMementoR/articles/inbuilt_styles.md)
vignette.
