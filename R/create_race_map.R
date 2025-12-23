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
##' @return Saves the map to the specified output directory (PNG file)
##' @import ggplot2 sf dplyr xml2 grid osmdata gfonts extrafont showtext yaml lwgeom geosphere patchwork poweRof10
##' @export
create_race_map <- function(
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
  with_hillshade = FALSE
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

  # Calculate bounding box with padding that respects A3 portrait aspect ratio
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

  # Only get OSM map components if with_OSM is TRUE
  if (with_OSM) {
    osm <- get_osm_components(bbox, location, cache_data = cache_data)
    highways <- osm$highways
    streets <- osm$streets
    water <- osm$water
    sea <- osm$sea
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

  # Calculate precise positioning in coord space
  x_center <- base::mean(c(bbox[1], bbox[3]))
  y_range <- bbox[4] - bbox[2]

  # Title positioned from top (using coord space units)
  title_y <- bbox[4] - (y_range * 0.04)

  # Subtitle positioned below title, move up if many lines
  n_subtitle_lines <- base::length(entries)
  subtitle_y <- bbox[2] + (y_range * (0.09 + 0.035 * (n_subtitle_lines - 1)))

  # Font sizes in coord space units
  # Shrink title size if multiple words
  title_text <- if (base::is.null(location)) {
    competitor_name
  } else {
    base::toupper(location)
  }
  n_title_words <- base::length(base::strsplit(title_text, "[[:space:]]+")[[1]])
  title_scale <- base::max(0.7, 1 - 0.1 * (n_title_words - 1))
  title_size <- page_height * 0.07 * title_scale # 7% of page height, scaled
  subtitle_size <- page_height * 0.035 # 3.5% of page height

  # adjust route size based on page size
  route_size <- page_width * 0.004 # 0.4% of page width (adjust as needed)
  point_size <- page_width * 0.01 # 1% of page width

  # Create gradient data for top and bottom fades
  fade_height <- y_range * 0.20 # 15% fade at top and bottom
  n_steps <- 75 # Number of gradient steps

  # Top fade - create rectangles with increasing alpha
  top_fade <- base::data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[4] - fade_height + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[4] - fade_height + (1:n_steps) * (fade_height / n_steps),
    alpha = base::seq(0, 0.9, length.out = n_steps)
  )

  # Bottom fade - create rectangles with decreasing alpha
  bottom_fade <- base::data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[2] + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[2] + (1:n_steps) * (fade_height / n_steps),
    alpha = base::seq(0.9, 0, length.out = n_steps)
  )

  # create subtitle text from entries
  subtitle_lines <- base::sapply(entries, function(entry) {
    base::paste(entry$race_year, entry$race_time, sep = " - ")
  })
  subtitle_text <- base::paste(subtitle_lines, collapse = "\n")

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
    ) +
    # Top fade gradient
    ggplot2::geom_rect(
      data = top_fade,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = top_fade$alpha,
      inherit.aes = FALSE
    ) +
    # Bottom fade gradient
    ggplot2::geom_rect(
      data = bottom_fade,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = bottom_fade$alpha,
      inherit.aes = FALSE
    ) +
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
      size = title_size, # Convert mm to ggplot size units
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
      size = subtitle_size, # Convert mm to ggplot size units
      color = text_color,
      family = font_family,
      vjust = 1,
      lineheight = 0.7
    ) +
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
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      aspect.ratio = page_height / page_width
    )
  if (with_elevation) {
    final_plot <- p_map +
      patchwork::inset_element(
        p_elev,
        left = 0.05, # Start at far left
        bottom = 0.01, # Slightly above bottom
        right = 0.97, # End at far right
        top = 0.05, # Small height (15% of total)
        align_to = 'full'
      )
    base::plot(final_plot)
  } else {
    final_plot <- p_map
  }

  save_path <- base::file.path(
    output_dir,
    base::paste0(
      base::paste(location, base::gsub(" ", "_", competitor_name), sep = "-"),
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


