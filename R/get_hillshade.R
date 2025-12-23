#' Get hillshade elevation data for a bounding box
#'
#' @param bbox Numeric vector: c(xmin, ymin, xmax, ymax)
#' @param location String identifier for caching
#' @param z Zoom level for elevation data (default 12)
#' @param cache_data Logical, whether to cache elevation data locally
#' @return Data frame with x, y, elevation columns
#' @export
get_hillshade <- function(bbox, location, z = 12, cache_data = TRUE) {
  location <- tolower(gsub(" ", "_", location))
  cache_dir <- base::file.path(base::getwd(), "data_cache_tmp")
  if (cache_data && !base::dir.exists(cache_dir)) {
    if (base::interactive()) {
      response <- base::readline(
        paste0(
          "Elevation cache directory '",
          cache_dir,
          "' does not exist.\n",
          "Allow creation for .rds cache files? [y/N]: "
        )
      )
      if (!tolower(response) %in% c("y", "yes")) {
        stop("User did not allow cache directory creation.")
      }
    } else {
      stop(
        "Cache directory does not exist and cannot prompt in non-interactive mode."
      )
    }
    base::dir.create(cache_dir, recursive = TRUE)
  }
  elev_path <- base::file.path(
    cache_dir,
    paste0("hillshade_", location, ".rds")
  )
  if (cache_data && base::file.exists(elev_path)) {
    elevations <- base::readRDS(elev_path)
  } else {
    poly <- sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(
          bbox[1],
          bbox[2],
          bbox[1],
          bbox[4],
          bbox[3],
          bbox[4],
          bbox[3],
          bbox[2],
          bbox[1],
          bbox[2]
        ),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 4326
    )
    poly_sf <- sf::st_sf(geometry = poly)
    elevation_data <- elevatr::get_elev_raster(
      locations = poly_sf,
      z = z,
      clip = "locations",
      verbose = TRUE,
      neg_to_na = TRUE
    )
    elevations <- raster::as.data.frame(elevation_data, xy = TRUE)
    colnames(elevations)[3] <- "elevation"
    elevations <- elevations[stats::complete.cases(elevations), ]
    if (cache_data) base::saveRDS(elevations, elev_path)
  }
  return(elevations)
}

#' Hillshade and shaded relief functions for ggplot2
#'
#' Provides geom_relief and supporting functions for shaded relief plots.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}.
#' @param data The data to be displayed in this layer. If NULL, the default
#' data frame will be used.
#' @param stat The statistical transformation to use on the data for this layer.
#' Default is "identity".
#' @param position Position adjustment for overlapping points. Default is "identity".
#' @param ... Other arguments passed on to \code{layer()}. These are often aesthetics,
#' used to set an aesthetic to a fixed value, like \code{color = "red"} or \code{size = 3}.
#' They may also be parameters to the paired geom/stat.
#' @param raster Logical, whether to use raster rendering (default TRUE).
#' @param interpolate Logical, whether to interpolate raster (default TRUE).
#' @param na.rm Logical, whether to remove missing values (default FALSE).
#' @param show.legend Logical or NA, whether to include this layer in the legends.
#' Default is NA.
#' @param inherit.aes Logical, whether to inherit aesthetics from the plot mapping.
#' Default is TRUE.
#' @return A ggplot2 layer for shaded relief plotting.
#' @export
#' @seealso \code{\link{.derv}}, \code{\link{.rgb2hex}}, \code{\link{rect_to_poly}}
#' @import ggplot2
#' @importFrom grDevices colorRamp rgb
#' @importFrom grid rasterGrob rectGrob gpar
#' @import data.table
#' @importFrom scales alpha
#' @importFrom stats as.formula
#'
geom_relief <- function(
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
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRelief,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      raster = raster,
      interpolate = interpolate,
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomRelief ggproto object
#' @keywords internal
#' @noRd
GeomRelief <- ggplot2::ggproto(
  "GeomRelief",
  GeomTile,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(
    color = NA,
    fill = "grey35",
    size = 0.5,
    linetype = 1,
    alpha = NA,
    light = "white",
    dark = "gray20",
    sun.angle = 60
  ),
  draw_panel = function(data, panel_scales, coord, raster, interpolate) {
    if (!coord$is_linear()) {
      stop(
        "non lineal coordinates are not implemented in GeomRelief",
        call. = FALSE
      )
    } else {
      coords <- data.table::as.data.table(coord$transform(data, panel_scales))
      coords[, sun.angle := (sun.angle + 90) * pi / 180]
      coords[, dx := .derv(z, x), by = y]
      coords[, dy := .derv(z, y), by = x]
      coords[, shade := (cos(atan2(-dy, -dx) - sun.angle) + 1) / 2]
      coords[is.na(shade), shade := 0]
      coords[,
        fill := .rgb2hex(colorRamp(c(dark, light), space = "Lab")(shade)),
        by = .(dark, light)
      ]
      if (raster == TRUE) {
        if (!inherits(coord, "CoordCartesian")) {
          stop(
            "geom_raster only works with Cartesian coordinates",
            call. = FALSE
          )
        }
        x_pos <- as.integer(
          (coords$x - min(coords$x)) / resolution(coords$x, FALSE)
        )
        y_pos <- as.integer(
          (coords$y - min(coords$y)) / resolution(coords$y, FALSE)
        )
        nrow <- max(y_pos) + 1
        ncol <- max(x_pos) + 1
        raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
        raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(
          coords$fill,
          coords$alpha
        )
        x_rng <- c(
          min(coords$xmin, na.rm = TRUE),
          max(coords$xmax, na.rm = TRUE)
        )
        y_rng <- c(
          min(coords$ymin, na.rm = TRUE),
          max(coords$ymax, na.rm = TRUE)
        )
        grid::rasterGrob(
          raster,
          x = mean(x_rng),
          y = mean(y_rng),
          width = diff(x_rng),
          height = diff(y_rng),
          default.units = "native",
          interpolate = interpolate
        )
      } else {
        ggplot2:::ggname(
          "geom_rect",
          grid::rectGrob(
            coords$xmin,
            coords$ymax,
            width = coords$xmax - coords$xmin,
            height = coords$ymax - coords$ymin,
            default.units = "native",
            just = c("left", "top"),
            gp = grid::gpar(
              col = coords$fill,
              fill = alpha(coords$fill, coords$alpha),
              lwd = coords$size * .pt,
              lty = coords$linetype,
              lineend = "butt"
            )
          )
        )
      }
    }
  }
)

#' Convert rectangle to polygon data frame
#' @param xmin Minimum x coordinate
#' @param xmax Maximum x coordinate
#' @param ymin Minimum y coordinate
#' @param ymax Maximum y coordinate
#' @return Data frame with x and y coordinates of polygon
#' @keywords internal
#' @noRd
#'
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}

#' Convert RGB array to hex color
#' @param array Numeric matrix with 3 columns (R, G, B)
#' @return Character vector of hex colors
#' @keywords internal
#' @noRd
.rgb2hex <- function(array) {
  rgb(array[, 1], array[, 2], array[, 3], maxColorValue = 255)
}

#' Numerical derivative calculation
#' @param x Numeric vector of function values
#' @param y Numeric vector of independent variable values
#' @param order Order of derivative (1 or 2)
#' @param cyclical Logical, whether data is cyclical
#' @param fill Logical, whether to fill edge derivatives
#' @return Numeric vector of derivative values
#' @keywords internal
#' @noRd
.derv <- function(x, y, order = 1, cyclical = FALSE, fill = FALSE) {
  N <- length(x)
  if (N < 4) {
    return(rep(NA_real_, N))
  }
  d <- y[2] - y[1]
  if (order >= 3) {
    dxdy <- .derv(
      .derv(x, y, order = 2, cyclical = cyclical, fill = fill),
      y,
      order = order - 2,
      cyclical = cyclical,
      fill = fill
    )
  } else {
    if (order == 1) {
      dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N - 1))]) / (2 * d)
    } else if (order == 2) {
      dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N - 1))] - 2 * x) / d^2
    }
    if (!cyclical) {
      if (!fill) {
        dxdy[c(1, N)] <- NA
      }
      if (fill) {
        dxdy[1] <- (-11 / 6 * x[1] + 3 * x[2] - 3 / 2 * x[3] + 1 / 3 * x[4]) / d
        dxdy[N] <- (11 /
          6 *
          x[N] -
          3 * x[N - 1] +
          3 / 2 * x[N - 2] -
          1 / 3 * x[N - 3]) /
          d
      }
    }
  }
  return(dxdy)
}

#' Choose relief colors for hillshade based on map background color
#' @param bg_color Background color in hex format
#' @return List with light and dark relief colors
#' @export
choose_relief_colors <- function(bg_color) {
  # Compute luminance
  rgb <- grDevices::col2rgb(bg_color) / 255
  lum <- sum(rgb * c(0.2126, 0.7152, 0.0722))
  if (lum > 0.5) {
    # Light background: warm off-white and soft taupe
    relief_light <- "#F5F1E7"
    relief_dark <- "#bfc8d9"
  } else {
    # Dark background: light blue-grey and muted navy
    relief_light <- "#e0e6ef"
    relief_dark <- "#2d3250"
  }
  list(light = relief_light, dark = relief_dark)
}
