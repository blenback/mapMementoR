##' Create a map of a race route with OSM background and elevation chart
##'
##' This function generates a publication-quality map of a race route from a GPX file, with OpenStreetMap features, elevation chart, and customizable appearance. All theme and style arguments are handled directly in the function (no external theme required).
##'
##' @param gpx_file Path to GPX file
##' @param competitor_name Name of the competitor
##' @param entries List of race entries (each entry should have `race_year` and `race_time`)
##' @param location Location string for map title
##' @param route_color Color for the route line
##' @param route_size Size of the route line (overridden by page size)
##' @param bg_color Background color for the map
##' @param street_color Color for streets
##' @param highway_color Color for highways
##' @param water_color Color for water bodies
##' @param text_color Color for title and subtitle text (defaults to route_color)
##' @param font_family Font family for text
##' @param output_dir Directory to save the output map
##' @param with_elevation Boolean to include elevation chart
##' @param dpi DPI for saved image
##' @param page_size Page size (e.g., "A3", "A4")
#' @param orientation Page orientation: "portrait" or "landscape"
##' @param base_size Base font size for plot text
##' @param with_OSM Boolean to include OSM background features
##' @param cache_data Boolean; if TRUE, cache OSM data to disk (default), if FALSE always fetch fresh OSM data
#' @param with_hillshade Boolean to include hillshade (elevation relief) background
##' @param fade_directions Character vector specifying which sides to apply fade gradients to. Any combination of 'top', 'bottom', 'left', 'right'. Defaults to c('top', 'bottom').
#' @param crop_shape Shape to crop the map to. Options: NULL (no cropping, default), "circle", "ellipse". Defaults to NULL.
##' @return Saves the map to the specified output directory (PNG file)
##' @import ggplot2 sf dplyr xml2 grid osmdata gfonts extrafont showtext yaml lwgeom geosphere patchwork poweRof10
##' @export
#' @examples
#' create_memento_map(
#' gpx_file = "data/sample_race.gpx",
#' competitor_name = "John Doe",
#' entries = list(
#'  list(race_year = "2021", race_time = "3:15:30"),
#' list(race_year = "2022", race_time = "3:10:45")
#' ),
#' location = "Sample Marathon",
#' route_color = "#d1af82",
#' bg_color = "#0a0e27",
#' output_dir = "maps",
#' with_elevation = TRUE,
#' dpi = 300,
#' page_size = "A3",
#' orientation = "portrait",
#' base_size = 12,
#' with_OSM = TRUE,
#' cache_data = TRUE,
#' with_hillshade = FALSE
#' )
create_memento_map <- function(
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
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL
) {
  # Set text color to route color if not specified
  if (is.null(text_color)) {
    text_color <- route_color
  }

  dims <- get_page_dimensions(page_size, orientation)
  page_width <- dims$width
  page_height <- dims$height

  # Target aspect ratio
  target_aspect_ratio <- page_height / page_width

  # Parse GPX file
  track_data <- parse_gpx(gpx_file)

  if (with_elevation) {
    # Calculate cumulative distance along the track
    track_data$dist <- c(
      0,
      base::cumsum(
        geosphere::distHaversine(
          base::cbind(
            track_data$lon[-base::nrow(track_data)],
            track_data$lat[-base::nrow(track_data)]
          ),
          base::cbind(track_data$lon[-1], track_data$lat[-1])
        )
      ) /
        1000
    ) # distance in km

    # Use only the current track's max distance and elevation
    max_dist <- base::max(track_data$dist, na.rm = TRUE)
    track_min_elev <- base::min(track_data$ele, na.rm = TRUE)
    track_max_elev <- base::max(track_data$ele, na.rm = TRUE)

    # Set baseline slightly below minimum (e.g., 5% below)
    baseline <- track_min_elev * 0.95

    # Elevation chart (filled line)
    p_elev <- ggplot2::ggplot(track_data, ggplot2::aes(x = dist, y = ele)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = baseline, ymax = ele),
        fill = route_color,
        alpha = 0.35
      ) +
      ggplot2::geom_line(color = route_color, linewidth = 0.7) +
      ggplot2::scale_x_continuous(limits = c(0, max_dist), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(
        limits = c(baseline, track_max_elev),
        expand = c(0, 0)
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = NA, color = NA),
        panel.background = ggplot2::element_rect(fill = NA, color = NA)
      )
  }

  # Calculate bounding box with padding
  lat_range <- base::range(track_data$lat, na.rm = TRUE)
  lon_range <- base::range(track_data$lon, na.rm = TRUE)

  # Calculate initial ranges
  lat_span <- base::diff(lat_range)
  lon_span <- base::diff(lon_range)

  # Calculate current aspect ratio (accounting for latitude distortion)
  lat_center <- base::mean(lat_range)
  # Approximate conversion: 1 degree longitude ≈ cos(latitude) * 111km
  lon_to_lat_ratio <- base::cos(lat_center * base::pi / 180)
  current_aspect_ratio <- lat_span / (lon_span * lon_to_lat_ratio)

  # Adjust bbox to match target aspect ratio
  if (current_aspect_ratio > target_aspect_ratio) {
    # Route is too tall, expand longitude
    needed_lon_span <- lat_span / (target_aspect_ratio * lon_to_lat_ratio)
    lon_expansion <- (needed_lon_span - lon_span) / 2
    lon_padding <- lon_expansion + lon_span * 0.2 # Add 15% extra padding
    lat_padding <- lat_span * 0.2
  } else {
    # Route is too wide, expand latitude
    needed_lat_span <- lon_span * lon_to_lat_ratio * target_aspect_ratio
    lat_expansion <- (needed_lat_span - lat_span) / 2
    lat_padding <- lat_expansion + lat_span * 0.2 # Add 15% extra padding
    lon_padding <- lon_span * 0.2
  }

  bbox <- c(
    lon_range[1] - lon_padding,
    lat_range[1] - lat_padding,
    lon_range[2] + lon_padding,
    lat_range[2] + lat_padding
  )

  # Adjust bbox for crop shape if needed (to maintain true circle on rectangular page)
  if (!is.null(crop_shape)) {
    crop_result <- create_crop_mask(bbox, crop_shape, bg_color)
    crop_mask <- crop_result$mask
    # Don't adjust bbox - we'll use aspect.ratio = 1 instead
  } else {
    crop_mask <- NULL
  }

  # Only get OSM map components if with_OSM is TRUE
  if (with_OSM) {
    osm <- get_osm_components(
      bbox,
      location,
      cache_data = cache_data,
      components = components
    )
    highways <- if ("highways" %in% names(osm)) osm$highways else NULL
    streets <- if ("streets" %in% names(osm)) osm$streets else NULL
    water <- if ("water" %in% names(osm)) osm$water else NULL
    sea <- if ("coast" %in% names(osm)) osm$coast else NULL
  } else {
    highways <- NULL
    streets <- NULL
    water <- NULL
    sea <- NULL
  }

  # Get hillshade if requested
  if (with_hillshade) {
    hillshade <- get_hillshade(bbox, location, cache_data = cache_data)
  } else {
    hillshade <- NULL
  }

  # Decide whether to use circular crop mode
  use_circular_crop <- !is.null(crop_shape) && crop_shape == "circle"

  # Calculate precise positioning in coord space
  x_center <- base::mean(c(bbox[1], bbox[3]))
  y_range <- bbox[4] - bbox[2]

  # Title positioned from top (using coord space units)
  title_y <- bbox[4] - (y_range * 0.04)

  # Subtitle positioned below title, move up if many lines
  n_subtitle_lines <- base::length(entries)
  subtitle_y <- bbox[2] + (y_range * (0.09 + 0.035 * (n_subtitle_lines - 1)))

  # Font sizes for annotations: scale with both base_size and page dimensions
  # This ensures text is proportional to page size while respecting base_size setting
  title_text <- if (base::is.null(location)) {
    competitor_name
  } else {
    base::toupper(location)
  }
  n_title_words <- base::length(base::strsplit(title_text, "[[:space:]]+")[[1]])
  title_scale <- base::max(0.7, 1 - 0.1 * (n_title_words - 1))

  # Scale factor based on page size relative to A4 (210mm width)
  page_scale <- page_width / 210

  # Combine base_size with page scaling
  # Using smaller divisor (0.8) to make text larger
  # If using circular crop, scale up text to compensate for smaller plot area
  crop_text_scale <- if (use_circular_crop) {
    # When plot is square, it's smaller than the page, so increase text size
    if (page_width > page_height) {
      page_width / page_height # Landscape: compensate for width reduction
    } else {
      page_height / page_width # Portrait: compensate for height reduction
    }
  } else {
    1
  }

  title_size <- (base_size / 0.8) * page_scale * title_scale * crop_text_scale
  subtitle_size <- (base_size / 0.8) * page_scale * 0.4 * crop_text_scale

  # adjust route size based on page size
  route_size <- page_width * 0.004 # 0.4% of page width (adjust as needed)
  point_size <- page_width * 0.01 # 1% of page width

  # Create gradient data for fades on specified sides
  fade_height <- y_range * 0.20 # 20% fade for top/bottom
  fade_width <- (bbox[3] - bbox[1]) * 0.20 # 20% fade for left/right
  n_steps <- 75 # Number of gradient steps

  fade_data <- list()
  if ("top" %in% fade_directions) {
    fade_data$top <- base::data.frame(
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[4] -
        fade_height +
        (0:(n_steps - 1)) * (fade_height / n_steps),
      ymax = bbox[4] - fade_height + (1:n_steps) * (fade_height / n_steps),
      alpha = base::seq(0, 0.9, length.out = n_steps)
    )
  }
  if ("bottom" %in% fade_directions) {
    fade_data$bottom <- base::data.frame(
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[2] + (0:(n_steps - 1)) * (fade_height / n_steps),
      ymax = bbox[2] + (1:n_steps) * (fade_height / n_steps),
      alpha = base::seq(0.9, 0, length.out = n_steps)
    )
  }
  if ("left" %in% fade_directions) {
    fade_data$left <- base::data.frame(
      xmin = bbox[1] + (0:(n_steps - 1)) * (fade_width / n_steps),
      xmax = bbox[1] + (1:n_steps) * (fade_width / n_steps),
      ymin = bbox[2],
      ymax = bbox[4],
      alpha = base::seq(0.9, 0, length.out = n_steps)
    )
  }
  if ("right" %in% fade_directions) {
    fade_data$right <- base::data.frame(
      xmin = bbox[3] - fade_width + (0:(n_steps - 1)) * (fade_width / n_steps),
      xmax = bbox[3] - fade_width + (1:n_steps) * (fade_width / n_steps),
      ymin = bbox[2],
      ymax = bbox[4],
      alpha = base::seq(0, 0.9, length.out = n_steps)
    )
  }

  # create subtitle text from entries
  subtitle_lines <- base::sapply(entries, function(entry) {
    base::paste(entry$race_year, entry$race_time, sep = " - ")
  })

  # If more than 4 entries, format into two columns
  if (base::length(subtitle_lines) > 4) {
    n_rows <- base::ceiling(base::length(subtitle_lines) / 2)

    # Split into two columns
    col1_indices <- 1:n_rows
    col2_indices <- (n_rows + 1):base::length(subtitle_lines)

    col1 <- subtitle_lines[col1_indices]
    col2 <- if (
      base::length(col2_indices) > 0 &&
        col2_indices[1] <= base::length(subtitle_lines)
    ) {
      subtitle_lines[col2_indices]
    } else {
      character(0)
    }

    # Pad col2 with empty strings if needed to match col1 length
    if (base::length(col2) < base::length(col1)) {
      col2 <- c(col2, base::rep("", base::length(col1) - base::length(col2)))
    }

    # Combine columns side by side with spacing
    combined_lines <- base::sapply(1:n_rows, function(i) {
      if (col2[i] == "") {
        col1[i]
      } else {
        base::paste(
          base::format(col1[i], width = base::max(base::nchar(col1))),
          "    ", # Add spacing between columns
          col2[i],
          sep = ""
        )
      }
    })

    subtitle_text <- base::paste(combined_lines, collapse = "\n")
  } else {
    subtitle_text <- base::paste(subtitle_lines, collapse = "\n")
  }

  # Build ggplot
  p_map <- ggplot2::ggplot()
  # Add hillshade layer if requested
  if (!is.null(hillshade)) {
    relief_cols <- choose_relief_colors(bg_color)
    relief_alpha <- if (with_OSM) 0.45 else 1
    p_map <- p_map +
      geom_relief(
        data = hillshade,
        ggplot2::aes(
          x = x,
          y = y,
          z = elevation,
          light = relief_cols$light,
          dark = relief_cols$dark
        ),
        raster = TRUE,
        interpolate = TRUE,
        sun.angle = 60,
        alpha = relief_alpha
      )
  }
  # Add OSM layers if requested
  if (with_OSM) {
    # Water bodies (polygons and multipolygons)
    if (!is.null(water$osm_polygons) && nrow(water$osm_polygons) > 0) {
      p_map <- p_map +
        ggplot2::geom_sf(
          data = water$osm_polygons,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
    }
    if (
      !is.null(water$osm_multipolygons) && nrow(water$osm_multipolygons) > 0
    ) {
      p_map <- p_map +
        ggplot2::geom_sf(
          data = water$osm_multipolygons,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
    }
    # Sea (coastline polygons)
    if (!is.null(sea) && length(sea) > 0) {
      p_map <- p_map +
        ggplot2::geom_sf(
          data = sea,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
    }
    p_map <- p_map +
      ggplot2::geom_sf(
        data = streets$osm_lines,
        color = street_color,
        size = 0.3,
        alpha = 0.8
      ) +
      ggplot2::geom_sf(
        data = highways$osm_lines,
        color = highway_color,
        size = 0.6,
        alpha = 0.9
      )
  }
  # Route
  p_map <- p_map +
    ggplot2::geom_path(
      data = track_data,
      ggplot2::aes(x = lon, y = lat),
      color = route_color,
      linewidth = route_size,
      alpha = 0.95,
      lineend = "round"
    ) +
    # Start/Finish
    ggplot2::geom_point(
      data = track_data[1, ],
      ggplot2::aes(x = lon, y = lat),
      color = route_color,
      size = point_size,
      shape = 21,
      fill = route_color,
      stroke = 2
    ) +
    ggplot2::geom_point(
      data = track_data[base::nrow(track_data), ],
      ggplot2::aes(x = lon, y = lat),
      color = route_color,
      size = point_size,
      shape = 21,
      fill = route_color,
      stroke = 2
    )
  # Conditionally add fades for each side
  for (side in names(fade_data)) {
    p_map <- p_map +
      ggplot2::geom_rect(
        data = fade_data[[side]],
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = bg_color,
        alpha = fade_data[[side]]$alpha,
        inherit.aes = FALSE
      )
  }

  # Add crop mask if it was created
  if (!is.null(crop_mask)) {
    p_map <- p_map +
      ggplot2::geom_sf(
        data = crop_mask,
        fill = bg_color,
        color = NA,
        alpha = 1,
        inherit.aes = FALSE
      )
  }

  # Only add title/subtitle annotations if NOT using circular crop
  # (for circular crop, we'll add them as overlays later)
  if (!use_circular_crop) {
    p_map <- p_map +
      # Title annotation (using vjust for precise control)
      ggplot2::annotate(
        "text",
        x = x_center,
        y = title_y,
        label = if (base::is.null(location)) {
          competitor_name
        } else {
          base::toupper(location)
        },
        size = title_size, # Scaled from base_size
        fontface = "bold",
        color = text_color,
        family = font_family,
        vjust = 1 # Anchor text at top
      ) +
      # Subtitle annotation
      ggplot2::annotate(
        "text",
        x = x_center,
        y = subtitle_y,
        label = subtitle_text,
        size = subtitle_size, # Scaled from base_size
        color = text_color,
        family = font_family,
        vjust = 1,
        lineheight = 1.2
      )
  }

  p_map <- p_map +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = FALSE
    ) +
    ggplot2::theme_void(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.margin = if (!is.null(crop_shape) && crop_shape == "circle") {
        # Calculate margins to center a square plot on rectangular page
        if (page_width > page_height) {
          # Landscape: add horizontal margins
          margin_lr <- (page_width - page_height) / 2
          ggplot2::margin(0, margin_lr, 0, margin_lr, unit = "mm")
        } else {
          # Portrait: add vertical margins
          margin_tb <- (page_height - page_width) / 2
          ggplot2::margin(margin_tb, 0, margin_tb, 0, unit = "mm")
        }
      } else {
        ggplot2::margin(0, 0, 0, 0)
      },
      aspect.ratio = if (!is.null(crop_shape) && crop_shape == "circle") {
        1
      } else {
        page_height / page_width
      }
    )

  # Add elevation chart if requested
  if (with_elevation) {
    final_plot <- p_map +
      patchwork::inset_element(
        p_elev,
        left = 0.05, # Start at far left
        bottom = 0.01, # Slightly above bottom
        right = 0.95, # End at far right
        top = 0.06, # Small height (5% of total)
        align_to = 'full'
      )
  } else {
    final_plot <- p_map
  }

  # Add title and subtitle as overlays if using circular crop
  if (use_circular_crop) {
    # Create title as a ggplot with transparent background
    title_plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = if (base::is.null(location)) {
          competitor_name
        } else {
          base::toupper(location)
        },
        size = title_size,
        fontface = "bold",
        color = text_color,
        family = font_family
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)

    # Create subtitle as a ggplot with transparent background
    subtitle_plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = subtitle_text,
        size = subtitle_size,
        color = text_color,
        family = font_family,
        lineheight = 1.2
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(5, 0, 5, 0, unit = "pt")
      ) +
      ggplot2::coord_cartesian(
        xlim = c(0, 1),
        ylim = c(0, 1),
        expand = TRUE,
        clip = "off"
      )

    # Add title and subtitle as inset elements on the full page
    final_plot <- final_plot +
      patchwork::inset_element(
        title_plot,
        left = 0,
        right = 1,
        top = 0.98,
        bottom = 0.92,
        align_to = 'full',
        clip = FALSE,
        on_top = TRUE
      ) +
      patchwork::inset_element(
        subtitle_plot,
        left = 0,
        right = 1,
        top = 0.16,
        bottom = 0.06,
        align_to = 'full',
        clip = FALSE,
        on_top = TRUE
      )
  }

  if (with_elevation || use_circular_crop) {
    base::plot(final_plot)
  }

  save_path <- base::file.path(
    output_dir,
    base::paste0(
      location,
      ".png"
    )
  )

  ggplot2::ggsave(
    filename = save_path,
    plot = final_plot,
    width = page_width,
    height = page_height,
    units = "mm",
    dpi = dpi
  )
}
