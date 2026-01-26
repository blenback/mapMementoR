# Get OSM map components (highways, streets, water, sea) for a bounding box and location

Get OSM map components (highways, streets, water, sea) for a bounding
box and location

## Usage

``` r
get_osm_components(
  bbox,
  location,
  cache_data = TRUE,
  components = c("highways", "streets", "water", "coast")
)
```

## Arguments

- bbox:

  Numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)

- location:

  String identifier for caching

- cache_data:

  Logical, whether to cache OSM data locally

- components:

  Character vector specifying which OSM components to include. Any
  combination of 'highways', 'streets', 'water', 'coast'. Defaults to
  all.

## Value

List with selected OSM components
