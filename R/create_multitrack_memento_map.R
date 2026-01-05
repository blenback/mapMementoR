##' Create a map with multiple race routes from GPX files
##'
##' This function generates a publication-quality map with multiple race routes from GPX files,
##' with optional labels for each route. It uses most of the same functionality as create_memento_map
##' but supports multiple tracks on a single map.
##'
##' @param gpx_files Character vector of paths to GPX files
##' @param track_labels Optional character vector of labels for each track (must match length of gpx_files)
##' @param route_colors Character vector of colors for each route (if single color, applied to all)
##' @param with_labels Boolean to include text labels along routes using geomtextpath
##' @param label_spacing Spacing for labels along the path (default 0.3)
##' @param label_size Size of route labels (default 3)
##' @param location Location string for map title
##' @param route_size Size of the route lines (overridden by page size)
##' @param bg_color Background color for the map
##' @param street_color Color for streets
##' @param highway_color Color for highways
##' @param water_color Color for water bodies
##' @param text_color Color for title text (defaults to first route_color)
##' @param font_family Font family for text
##' @param output_dir Directory to save the output map
##' @param with_elevation Boolean to include elevation chart for first track
##' @param dpi DPI for saved image
##' @param page_size Page size (e.g., "A3", "A4")
##' @param orientation Page orientation: "portrait" or "landscape"
##' @param base_size Base font size for plot text
##' @param with_OSM Boolean to include OSM background features
##' @param cache_data Boolean; if TRUE, cache OSM data to disk (default)
##' @param with_hillshade Boolean to include hillshade (elevation relief) background
##' @param components Character vector of OSM components to include
##' @param fade_directions Character vector specifying fade directions
##' @param crop_shape Shape to crop the map to. Options: NULL, "circle", "ellipse"
##' @return Saves the map to the specified output directory (PNG file)
##' @import ggplot2 sf dplyr xml2 grid osmdata geosphere patchwork geomtextpath
##' @export
##' @examples
##' create_multitrack_memento_map(
##'   gpx_files = c("data/route1.gpx", "data/route2.gpx"),
##'   track_labels = c("2023", "2024"),
##'   route_colors = c("#d1af82", "#82d1af"),
##'   with_labels = TRUE,
##'   location = "Marathon Comparison",
##'   bg_color = "#0a0e27",
##'   output_dir = "maps",
##'   dpi = 300,
##'   page_size = "A4",
##'   orientation = "portrait"
##' )
create_multitrack_memento_map <- function(
  gpx_files,
  track_labels = NULL,
  route_colors = "#d1af82",
  with_labels = FALSE,
  label_spacing = 0.3,
  label_size = 3,
  location = NULL,
  route_size = 1.2,
  bg_color = "#0a0e27",
  street_color = "#1a1f3a",
  highway_color = "#2d3250",
  water_color = "#1a2332",
  text_color = NULL,
  font_family = "Outfit-VariableFont_wght",
  output_dir,
  with_elevation = TRUE,
  dpi = 300,
  page_size = "A4",
  orientation = "portrait",
  base_size = 12,
  with_OSM = TRUE,
  cache_data = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL
) {
  # Validate inputs
  n_tracks <- base::length(gpx_files)
  if (n_tracks == 0) {
    stop("At least one GPX file must be provided")
  }

  # Handle track labels
  if (is.null(track_labels)) {
    track_labels <- base::paste("Track", 1:n_tracks)
  } else if (base::length(track_labels) != n_tracks) {
    stop("track_labels must have the same length as gpx_files")
  }

  # Handle route colors - recycle if needed
  if (base::length(route_colors) == 1) {
    route_colors <- base::rep(route_colors, n_tracks)
  } else if (base::length(route_colors) != n_tracks) {
    stop("route_colors must be length 1 or match the length of gpx_files")
  }

  # Set text color to first route color if not specified
  if (is.null(text_color)) {
    text_color <- route_colors[1]
  }

  dims <- get_page_dimensions(page_size, orientation)
  page_width <- dims$width
  page_height <- dims$height

  # Target aspect ratio
  target_aspect_ratio <- page_height / page_width

  # Parse all GPX files
  all_tracks <- base::lapply(1:n_tracks, function(i) {
    track_data <- parse_gpx(gpx_files[i])
    track_data$track_id <- i
    track_data$track_label <- track_labels[i]
    track_data$track_color <- route_colors[i]
    track_data
  })

  # Calculate bounding box from ALL tracks
  all_lats <- base::unlist(base::lapply(all_tracks, function(x) x$lat))
  all_lons <- base::unlist(base::lapply(all_tracks, function(x) x$lon))

  lat_range <- base::range(all_lats, na.rm = TRUE)
  lon_range <- base::range(all_lons, na.rm = TRUE)

  # Calculate initial ranges
  lat_span <- base::diff(lat_range)
  lon_span <- base::diff(lon_range)

  # Calculate current aspect ratio (accounting for latitude distortion)
  lat_center <- base::mean(lat_range)
  lon_to_lat_ratio <- base::cos(lat_center * base::pi / 180)
  current_aspect_ratio <- lat_span / (lon_span * lon_to_lat_ratio)

  # Adjust bbox to match target aspect ratio
  if (current_aspect_ratio > target_aspect_ratio) {
    needed_lon_span <- lat_span / (target_aspect_ratio * lon_to_lat_ratio)
    lon_expansion <- (needed_lon_span - lon_span) / 2
    lon_padding <- lon_expansion + lon_span * 0.2
    lat_padding <- lat_span * 0.2
  } else {
    needed_lat_span <- lon_span * lon_to_lat_ratio * target_aspect_ratio
    lat_expansion <- (needed_lat_span - lat_span) / 2
    lat_padding <- lat_expansion + lat_span * 0.2
    lon_padding <- lon_span * 0.2
  }

  bbox <- c(
    lon_range[1] - lon_padding,
    lat_range[1] - lat_padding,
    lon_range[2] + lon_padding,
    lat_range[2] + lat_padding
  )

  # Handle crop shape
  if (!is.null(crop_shape)) {
    crop_result <- create_crop_mask(bbox, crop_shape, bg_color)
    crop_mask <- crop_result$mask
  } else {
    crop_mask <- NULL
  }

  # Get OSM data if requested
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

  # Calculate elevation for first track if requested
  if (with_elevation && base::length(all_tracks) > 0) {
    track_data <- all_tracks[[1]]

    # Calculate cumulative distance
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
    )

    max_dist <- base::max(track_data$dist, na.rm = TRUE)
    track_min_elev <- base::min(track_data$ele, na.rm = TRUE)
    track_max_elev <- base::max(track_data$ele, na.rm = TRUE)
    baseline <- track_min_elev * 0.95

    p_elev <- ggplot2::ggplot(track_data, ggplot2::aes(x = dist, y = ele)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = baseline, ymax = ele),
        fill = route_colors[1],
        alpha = 0.35
      ) +
      ggplot2::geom_line(color = route_colors[1], linewidth = 0.7) +
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

  # Calculate positioning and sizes
  use_circular_crop <- !is.null(crop_shape) && crop_shape == "circle"
  x_center <- base::mean(c(bbox[1], bbox[3]))
  y_range <- bbox[4] - bbox[2]
  title_y <- bbox[4] - (y_range * 0.04)

  # Font sizes
  title_text <- if (base::is.null(location)) {
    "Multi-Track Map"
  } else {
    base::toupper(location)
  }
  n_title_words <- base::length(base::strsplit(title_text, "[[:space:]]+")[[1]])
  title_scale <- base::max(0.7, 1 - 0.1 * (n_title_words - 1))
  page_scale <- page_width / 210

  crop_text_scale <- if (use_circular_crop) {
    if (page_width > page_height) {
      page_width / page_height
    } else {
      page_height / page_width
    }
  } else {
    1
  }

  title_size <- (base_size / 0.8) * page_scale * title_scale * crop_text_scale

  # Adjust route and point sizes
  route_size <- page_width * 0.004
  point_size <- page_width * 0.01

  # Create fade data
  fade_height <- y_range * 0.20
  fade_width <- (bbox[3] - bbox[1]) * 0.20
  n_steps <- 75
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

  # Build the map
  p_map <- ggplot2::ggplot()

  # Add hillshade
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

  # Add OSM layers
  if (with_OSM) {
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

  # Add all routes
  for (i in 1:n_tracks) {
    track <- all_tracks[[i]]

    if (with_labels) {
      # Use geomtextpath for labeled routes
      p_map <- p_map +
        geomtextpath::geom_textpath(
          data = track,
          ggplot2::aes(x = lon, y = lat, label = track_label),
          color = track$track_color[1],
          size = label_size,
          linewidth = route_size,
          alpha = 0.95,
          hjust = 0.5,
          vjust = -0.5,
          text_only = FALSE,
          spacing = label_spacing,
          family = font_family
        )
    } else {
      # Regular path without labels
      p_map <- p_map +
        ggplot2::geom_path(
          data = track,
          ggplot2::aes(x = lon, y = lat),
          color = track$track_color[1],
          linewidth = route_size,
          alpha = 0.95,
          lineend = "round"
        )
    }

    # Add start/finish points
    p_map <- p_map +
      ggplot2::geom_point(
        data = track[1, ],
        ggplot2::aes(x = lon, y = lat),
        color = track$track_color[1],
        size = point_size,
        shape = 21,
        fill = track$track_color[1],
        stroke = 2
      ) +
      ggplot2::geom_point(
        data = track[base::nrow(track), ],
        ggplot2::aes(x = lon, y = lat),
        color = track$track_color[1],
        size = point_size,
        shape = 21,
        fill = track$track_color[1],
        stroke = 2
      )
  }

  # Add fades
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

  # Add crop mask
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

  # Add title if not using circular crop
  if (!use_circular_crop) {
    p_map <- p_map +
      ggplot2::annotate(
        "text",
        x = x_center,
        y = title_y,
        label = title_text,
        size = title_size,
        fontface = "bold",
        color = text_color,
        family = font_family,
        vjust = 1
      )
  }

  # Apply theme
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
      plot.margin = if (use_circular_crop) {
        if (page_width > page_height) {
          margin_lr <- (page_width - page_height) / 2
          ggplot2::margin(0, margin_lr, 0, margin_lr, unit = "mm")
        } else {
          margin_tb <- (page_height - page_width) / 2
          ggplot2::margin(margin_tb, 0, margin_tb, 0, unit = "mm")
        }
      } else {
        ggplot2::margin(0, 0, 0, 0)
      },
      aspect.ratio = if (use_circular_crop) 1 else page_height / page_width
    )

  # Add elevation chart
  if (with_elevation && exists("p_elev")) {
    final_plot <- p_map +
      patchwork::inset_element(
        p_elev,
        left = 0.05,
        bottom = 0.01,
        right = 0.95,
        top = 0.06,
        align_to = 'full'
      )
  } else {
    final_plot <- p_map
  }

  # Add title overlay for circular crop
  if (use_circular_crop) {
    title_plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = title_text,
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
      )
  }

  if (with_elevation || use_circular_crop) {
    base::plot(final_plot)
  }

  # Save the plot
  save_path <- base::file.path(
    output_dir,
    base::paste0(if (is.null(location)) "multitrack_map" else location, ".png")
  )

  ggplot2::ggsave(
    filename = save_path,
    plot = final_plot,
    width = page_width,
    height = page_height,
    units = "mm",
    dpi = dpi
  )

  invisible(save_path)
}
