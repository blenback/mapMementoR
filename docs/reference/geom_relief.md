# Hillshade and shaded relief functions for ggplot2

Provides geom_relief and supporting functions for shaded relief plots.

## Usage

``` r
geom_relief(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  raster = TRUE,
  interpolate = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by `aes()` or `aes_()`.

- data:

  The data to be displayed in this layer. If NULL, the default data
  frame will be used.

- stat:

  The statistical transformation to use on the data for this layer.
  Default is "identity".

- position:

  Position adjustment for overlapping points. Default is "identity".

- ...:

  Other arguments passed on to `layer()`. These are often aesthetics,
  used to set an aesthetic to a fixed value, like `color = "red"` or
  `size = 3`. They may also be parameters to the paired geom/stat.

- raster:

  Logical, whether to use raster rendering (default TRUE).

- interpolate:

  Logical, whether to interpolate raster (default TRUE).

- na.rm:

  Logical, whether to remove missing values (default FALSE).

- show.legend:

  Logical or NA, whether to include this layer in the legends. Default
  is NA.

- inherit.aes:

  Logical, whether to inherit aesthetics from the plot mapping. Default
  is TRUE.

## Value

A ggplot2 layer for shaded relief plotting.

## See also

`.derv`, `.rgb2hex`, `rect_to_poly`
