# Get hillshade elevation data for a bounding box

Get hillshade elevation data for a bounding box

## Usage

``` r
get_hillshade(bbox, location, z = 12, cache_data = TRUE)
```

## Arguments

- bbox:

  Numeric vector: c(xmin, ymin, xmax, ymax)

- location:

  String identifier for caching

- z:

  Zoom level for elevation data (default 12)

- cache_data:

  Logical, whether to cache elevation data locally

## Value

Data frame with x, y, elevation columns
