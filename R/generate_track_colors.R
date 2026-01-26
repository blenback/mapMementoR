#' Generate visually distinct track colors from a base color
#'
#' Creates a palette of colors that are visually distinct from each other
#' while maintaining harmony with the base route color. Uses HSL color space
#' to vary hue, saturation, and lightness for maximum distinction.
#'
#' @param base_color Character string, a hex color code (e.g., "#d1af82")
#' @param n_colors Integer, number of distinct colors to generate
#' @param method Character, method for generating colors:
#'   - "hue_shift": Varies hue while maintaining similar saturation/lightness (default)
#'   - "complementary": Creates complementary and analogous colors
#'   - "gradient": Creates a gradient with varying lightness and saturation
#' @return Character vector of hex color codes
#' @export
#' @examples
#' # Generate 14 distinct colors from a gold base
#' generate_track_colors("#d1af82", 14)
#'
#' # Use complementary method
#' generate_track_colors("#d1af82", 14, method = "complementary")
generate_track_colors <- function(
  base_color,
  n_colors,
  method = "hue_shift"
) {
  # Convert hex to RGB
  rgb_vals <- grDevices::col2rgb(base_color)[, 1] / 255

  # Convert RGB to HSL
  rgb_to_hsl <- function(r, g, b) {
    # Validate inputs
    r <- max(0, min(1, r))
    g <- max(0, min(1, g))
    b <- max(0, min(1, b))

    max_val <- max(r, g, b)
    min_val <- min(r, g, b)
    delta <- max_val - min_val

    # Lightness
    l <- (max_val + min_val) / 2

    if (delta == 0 || delta < 1e-10) {
      # Achromatic (gray)
      h <- 0
      s <- 0
    } else {
      # Saturation
      s <- if (l < 0.5) {
        delta / (max_val + min_val)
      } else {
        delta / (2 - max_val - min_val)
      }

      # Hue
      if (abs(max_val - r) < 1e-10) {
        h <- ((g - b) / delta + (if (g < b) 6 else 0)) / 6
      } else if (abs(max_val - g) < 1e-10) {
        h <- ((b - r) / delta + 2) / 6
      } else {
        h <- ((r - g) / delta + 4) / 6
      }

      # Ensure hue is in [0, 1]
      h <- h %% 1
    }

    c(h = h, s = s, l = l)
  }

  # Convert HSL to RGB
  hsl_to_rgb <- function(h, s, l) {
    # Validate inputs and handle edge cases
    if (is.na(h) || is.na(s) || is.na(l)) {
      warning("NA values in HSL conversion, returning black")
      return(grDevices::rgb(0, 0, 0))
    }

    # Ensure values are in valid range
    h <- h %% 1 # Hue wraps around
    s <- max(0, min(1, s)) # Saturation between 0 and 1
    l <- max(0, min(1, l)) # Lightness between 0 and 1

    if (s == 0) {
      r <- g <- b <- l
    } else {
      hue_to_rgb <- function(p, q, t) {
        # Normalize t to [0, 1] range
        if (is.na(t)) {
          return(0)
        }
        if (t < 0) {
          t <- t + 1
        }
        if (t > 1) {
          t <- t - 1
        }

        if (t < 1 / 6) {
          return(p + (q - p) * 6 * t)
        }
        if (t < 1 / 2) {
          return(q)
        }
        if (t < 2 / 3) {
          return(p + (q - p) * (2 / 3 - t) * 6)
        }
        return(p)
      }

      q <- if (l < 0.5) l * (1 + s) else l + s - l * s
      p <- 2 * l - q

      r <- hue_to_rgb(p, q, h + 1 / 3)
      g <- hue_to_rgb(p, q, h)
      b <- hue_to_rgb(p, q, h - 1 / 3)
    }

    grDevices::rgb(r, g, b)
  }

  base_hsl <- rgb_to_hsl(rgb_vals[1], rgb_vals[2], rgb_vals[3])

  if (method == "hue_shift") {
    # Vary hue across the color wheel while adjusting saturation and lightness
    # for maximum visual distinction
    colors <- sapply(1:n_colors, function(i) {
      if (i == 1) {
        # First color is always the base color
        return(base_color)
      }

      # Distribute remaining colors across the spectrum
      hue_shift <- ((i - 1) / (n_colors - 1)) * 0.85 # Use 85% of color wheel
      new_hue <- (base_hsl["h"] + hue_shift) %% 1

      # Vary saturation to increase distinction
      sat_variation <- 0.15 * sin((i - 1) / n_colors * pi * 2)
      new_sat <- max(0.3, min(1, base_hsl["s"] + sat_variation))

      # Vary lightness in a wave pattern
      light_variation <- 0.15 * cos((i - 1) / n_colors * pi * 2)
      new_light <- max(0.35, min(0.85, base_hsl["l"] + light_variation))

      hsl_to_rgb(new_hue, new_sat, new_light)
    })
  } else if (method == "complementary") {
    # Create colors using complementary and analogous relationships
    colors <- sapply(1:n_colors, function(i) {
      if (i == 1) {
        # First color is always the base color
        return(base_color)
      } else {
        # Alternate between complementary and analogous hues
        step <- (i - 1)
        if (step %% 3 == 1) {
          # Complementary (opposite side of color wheel)
          new_hue <- (base_hsl["h"] + 0.5 + (step / n_colors) * 0.1) %% 1
        } else if (step %% 3 == 2) {
          # Analogous + (nearby on color wheel)
          new_hue <- (base_hsl["h"] + (step / n_colors) * 0.25) %% 1
        } else {
          # Analogous - (nearby on other side)
          new_hue <- (base_hsl["h"] - (step / n_colors) * 0.25) %% 1
        }

        # Vary saturation and lightness for additional distinction
        new_sat <- max(0.3, min(1, base_hsl["s"] + (step %% 5 - 2) * 0.1))
        new_light <- max(
          0.35,
          min(0.85, base_hsl["l"] + (step %% 4 - 1.5) * 0.12)
        )
      }

      hsl_to_rgb(new_hue, new_sat, new_light)
    })
  } else if (method == "gradient") {
    # Create a gradient with varying lightness and saturation
    # while slightly shifting hue
    colors <- sapply(1:n_colors, function(i) {
      if (i == 1) {
        # First color is always the base color
        return(base_color)
      }

      progress <- (i - 1) / (n_colors - 1)

      # Slight hue shift
      new_hue <- (base_hsl["h"] + progress * 0.15) %% 1

      # Strong saturation gradient
      new_sat <- base_hsl["s"] * (1 - progress * 0.5)

      # Lightness gradient from darker to lighter
      new_light <- max(0.35, min(0.85, base_hsl["l"] - 0.2 + progress * 0.4))

      hsl_to_rgb(new_hue, new_sat, new_light)
    })
  } else {
    stop("Unknown method. Choose 'hue_shift', 'complementary', or 'gradient'")
  }

  return(colors)
}
