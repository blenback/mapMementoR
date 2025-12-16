# Function for multi-day/multi-stage races with multiple GPX files
create_multiday_map <- function(
  segments, # List of segments, each with gpx_file and segment_name
  competitor_name,
  location = NULL,
  route_colors = NULL, # Vector of colors for each segment
  route_size = 1.2,
  bg_color = "#0a0e27",
  street_color = "#1a1f3a",
  highway_color = "#2d3250",
  water_color = "#1a2332",
  text_color = NULL,
  font_family = "Outfit-VariableFont_wght",
  output_dir,
  dpi,
  page_size
) {
  # Default route colors if not provided
  if (is.null(route_colors)) {
    route_colors <- c(
      "#d1af82",
      "#82b4d1",
      "#d182a8",
      "#a8d182",
      "#d1a882",
      "#8282d1"
    )
  }

  # Set text color to first route color if not specified
  if (is.null(text_color)) {
    text_color <- route_colors[1]
  }

  dims <- get_page_dimensions(page_size)
  page_width <- dims$width
  page_height <- dims$height

  # Target aspect ratio
  target_aspect_ratio <- page_height / page_width

  # Parse all GPX files and collect track data
  all_tracks <- list()
  for (i in seq_along(segments)) {
    track_data <- parse_gpx(segments[[i]]$gpx_file)
    track_data$segment_name <- segments[[i]]$segment_name
    track_data$segment_id <- i
    track_data$color <- route_colors[((i - 1) %% length(route_colors)) + 1]
    all_tracks[[i]] <- track_data
  }

  # Combine all tracks
  combined_tracks <- do.call(rbind, all_tracks)

  # Calculate bounding box with padding that respects aspect ratio
  lat_range <- range(combined_tracks$lat, na.rm = TRUE)
  lon_range <- range(combined_tracks$lon, na.rm = TRUE)

  # Calculate initial ranges
  lat_span <- diff(lat_range)
  lon_span <- diff(lon_range)

  # Calculate current aspect ratio (accounting for latitude distortion)
  lat_center <- mean(lat_range)
  lon_to_lat_ratio <- cos(lat_center * pi / 180)
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

  highways_path <- paste0("data/highways_", location, ".rds")
  streets_path <- paste0("data/streets_", location, ".rds")
  water_path <- paste0("data/water_", location, ".rds")
  coast_path <- paste0("data/coastlines_", location, ".rds")

  # Load or fetch OSM data (same as original function)
  if (
    all(
      file.exists(highways_path),
      file.exists(streets_path),
      file.exists(water_path),
      file.exists(coast_path)
    )
  ) {
    highways <- readRDS(highways_path)
    streets <- readRDS(streets_path)
    water <- readRDS(water_path)
    sea <- readRDS(coast_path)
  } else {
    if (!file.exists(highways_path)) {
      highways <- bbox %>%
        opq() %>%
        add_osm_feature(
          key = "highway",
          value = c(
            "motorway",
            "trunk",
            "primary",
            "secondary",
            "tertiary",
            "motorway_link",
            "trunk_link",
            "primary_link",
            "secondary_link",
            "tertiary_link"
          )
        ) %>%
        osmdata_sf()
      saveRDS(highways, highways_path)
    } else {
      highways <- readRDS(highways_path)
    }

    if (!file.exists(streets_path)) {
      streets <- bbox %>%
        opq() %>%
        add_osm_feature(
          key = "highway",
          value = c(
            "residential",
            "living_street",
            "service",
            "unclassified",
            "pedestrian",
            "footway",
            "track",
            "path"
          )
        ) %>%
        osmdata_sf()
      saveRDS(streets, streets_path)
    } else {
      streets <- readRDS(streets_path)
    }

    if (!file.exists(water_path)) {
      water <- bbox %>%
        opq() %>%
        add_osm_feature(key = "natural", value = "water") %>%
        osmdata_sf()
      saveRDS(water, water_path)
    } else {
      water <- readRDS(water_path)
    }

    if (!file.exists(coast_path)) {
      coastlines <- bbox %>%
        opq() %>%
        add_osm_feature(key = "natural", value = "coastline") %>%
        osmdata_sf()

      coastline_lines <- coastlines$osm_lines %>%
        st_cast("LINESTRING")

      if (!is.null(coastlines$osm_lines) && nrow(coastlines$osm_lines) > 0) {
        bb_rect <- data.frame(
          lat = c(bbox[2], bbox[4]),
          lon = c(bbox[1], bbox[3])
        ) %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
          st_bbox() %>%
          st_as_sfc()

        bb_rect_split <- bb_rect %>%
          lwgeom::st_split(coastline_lines) %>%
          st_collection_extract("POLYGON")

        sea <- bb_rect_split[2]
      } else {
        sea <- NULL
      }
      saveRDS(sea, coast_path)
    } else {
      sea <- readRDS(coast_path)
    }
  }

  # Calculate precise positioning in coord space
  x_center <- mean(c(bbox[1], bbox[3]))
  y_range <- bbox[4] - bbox[2]

  # Title positioned from top
  title_y <- bbox[4] - (y_range * 0.04)
  # Subtitle positioned below title
  subtitle_y <- bbox[2] + (y_range * 0.09)

  # Font sizes
  title_size <- page_height * 0.07
  subtitle_size <- page_height * 0.035

  # Adjust route size based on page size
  route_size <- page_width * 0.004
  point_size <- page_width * 0.01

  # Create gradient data for fades
  fade_height <- y_range * 0.20
  n_steps <- 75

  top_fade <- data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[4] - fade_height + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[4] - fade_height + (1:n_steps) * (fade_height / n_steps),
    alpha = seq(0, 0.9, length.out = n_steps)
  )

  bottom_fade <- data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[2] + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[2] + (1:n_steps) * (fade_height / n_steps),
    alpha = seq(0.9, 0, length.out = n_steps)
  )

  # Create subtitle text from segment names
  subtitle_lines <- sapply(segments, function(seg) {
    seg$segment_name
  })
  subtitle_text <- paste(subtitle_lines, collapse = "\n")

  # Build ggplot
  p_map <- ggplot() +
    # Water bodies
    {
      if (!is.null(water$osm_polygons) && nrow(water$osm_polygons) > 0) {
        geom_sf(
          data = water$osm_polygons,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
      }
    } +
    {
      if (
        !is.null(water$osm_multipolygons) && nrow(water$osm_multipolygons) > 0
      ) {
        geom_sf(
          data = water$osm_multipolygons,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
      }
    } +
    # Sea
    {
      if (!is.null(sea) && length(sea) > 0) {
        geom_sf(
          data = sea,
          fill = water_color,
          color = water_color,
          alpha = 0.9
        )
      }
    } +
    geom_sf(
      data = streets$osm_lines,
      color = street_color,
      size = 0.3,
      alpha = 0.8
    ) +
    geom_sf(
      data = highways$osm_lines,
      color = highway_color,
      size = 0.6,
      alpha = 0.9
    )

  # Add each segment's route with its own color
  for (i in seq_along(all_tracks)) {
    track <- all_tracks[[i]]
    track_color <- track$color[1]

    p_map <- p_map +
      geom_path(
        data = track,
        aes(x = lon, y = lat),
        color = track_color,
        linewidth = route_size,
        alpha = 0.95,
        lineend = "round"
      ) +
      # Start point
      geom_point(
        data = track[1, ],
        aes(x = lon, y = lat),
        color = track_color,
        size = point_size,
        shape = 21,
        fill = track_color,
        stroke = 2
      ) +
      # End point
      geom_point(
        data = track[nrow(track), ],
        aes(x = lon, y = lat),
        color = track_color,
        size = point_size,
        shape = 21,
        fill = track_color,
        stroke = 2
      )
  }

  # Add fades and text
  p_map <- p_map +
    geom_rect(
      data = top_fade,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = top_fade$alpha,
      inherit.aes = FALSE
    ) +
    geom_rect(
      data = bottom_fade,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = bottom_fade$alpha,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = x_center,
      y = title_y,
      label = if (is.null(location)) {
        competitor_name
      } else {
        toupper(location)
      },
      size = title_size,
      fontface = "bold",
      color = text_color,
      family = font_family,
      vjust = 1
    ) +
    annotate(
      "text",
      x = x_center,
      y = subtitle_y,
      label = subtitle_text,
      size = subtitle_size,
      color = text_color,
      family = font_family,
      vjust = 1,
      lineheight = 0.7
    ) +
    labs(title = NULL, subtitle = NULL) +
    coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = FALSE
    ) +
    theme_race_maps(bg_color = bg_color, aspect.ratio = target_aspect_ratio)

  save_path <- file.path(
    output_dir,
    paste0(
      paste(location, gsub(" ", "_", competitor_name), "multiday", sep = "-"),
      ".tif"
    )
  )

  ggsave(
    filename = save_path,
    plot = p_map,
    width = page_width,
    height = page_height,
    units = "mm",
    dpi = dpi
  )

  return(p_map)
}
