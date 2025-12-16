# ============================================================================
# Race Route Mapping Script
# Create aesthetic maps from GPX tracks of race routes
#
# This script provides several functions for creating beautiful race maps:
#
# 1. parse_gpx() - Extracts coordinates from GPX files
# 2. create_race_map_simple() - Basic route map without background
# 3. create_race_map_enhanced() - Route map with elevation/distance coloring
# 4. create_race_map_with_background() - Route map with Google Maps background
# 5. create_race_series() - Batch process multiple races
#
# Author: Generated for race route visualization
# ============================================================================

# Load required libraries
library(sf)
library(ggplot2)
library(ggmap) # Optional: for map backgrounds (requires Google API key)
library(dplyr)
library(xml2)
library(grid)
library(osmdata)
library(gfonts)
library(extrafont)
library(showtext)
library(yaml)
library(lwgeom)
library(geosphere)
library(patchwork)

# Method 1: Using extrafont (for TTF/OTF files)
# Uncomment if using extrafont
# font_import(paths = "fonts", prompt = FALSE)
# loadfonts(device = "pdf") # or "win" for Windows, "postscript" for PS

# # Method 2: Using showtext (recommended for ggplot2)
# # This works better with ggsave and is more reliable
# font_dir <- "fonts"
# if (dir.exists(font_dir)) {
#   font_files <- list.files(
#     font_dir,
#     pattern = "\\.(ttf|otf)$",
#     full.names = TRUE,
#     ignore.case = TRUE
#   )

#   for (font_file in font_files) {
#     # Extract font name from filename (without extension)
#     font_name <- tools::file_path_sans_ext(basename(font_file))

#     # Add font to showtext
#     font_add(family = font_name, regular = font_file)
#   }

#   # Enable showtext for rendering
#   showtext_auto()

#   cat("Loaded", length(font_files), "font(s) from 'fonts' directory\n")
#   cat("Available fonts:", paste(font_families(), collapse = ", "), "\n")
# } else {
#   warning("'fonts' directory not found. Using system fonts.")
# }

# ============================================================================
# Functions
# ============================================================================
#remotes::install_github("hfshr/poweRof10")
library(poweRof10)

#' Save Powerof10 athlete race data to YAML
#' @param first_name Athlete's first name
#' @param surname Athlete's surname
#' @param club Athlete's club
#' @param yaml_path Path to save the YAML file (default: "<first_name>_<surname>_races.yaml")
#' @param event Optional vector of events to filter e.g "HM", "Mar", "5K "parkrun" etc.
#' @param year Optional vector of years to filter
save_powerof10_to_yaml <- function(
  first_name,
  surname,
  club,
  yaml_path = NULL,
  event = NULL,
  year = NULL
) {
  # Set default yaml_path if not provided
  if (is.null(yaml_path)) {
    yaml_path <- tolower(paste0(first_name, "_", surname, "_races.yaml"))
  }

  # Get athlete data
  athlete <- get_athlete(fn = first_name, sn = surname, club = club)
  if (is.null(athlete)) {
    stop("No athlete found.")
  }

  df <- athlete

  # Extract year from date string (e.g., "14 May 25" -> 2025)
  df$race_year <- as.integer(paste0("20", sub(".* (\\d{2})$", "\\1", df$date)))

  # Filter by event and year if provided
  if (!is.null(event)) {
    df <- df[df$event %in% event, ]
  }
  if (!is.null(year)) {
    df <- df[df$race_year %in% year, ]
  }

  # Build races list for YAML, grouping by event and location
  df$event_location <- paste(df$event, df$venue, sep = "__SEP__")
  split_list <- split(df, df$event_location)
  race_list <- unname(lapply(split_list, function(event_races) {
    event_name <- unique(event_races$event)[1]
    location_name <- unique(event_races$venue)[1]
    list(
      gpx_file = "", # You will need to fill this in manually
      competitor_name = paste(first_name, surname),
      event = event_name,
      location = location_name,
      entries = lapply(seq_len(nrow(event_races)), function(i) {
        list(
          race_year = as.character(event_races$race_year[i]),
          race_time = as.character(event_races$perf[i])
        )
      })
    )
  }))

  # Write to YAML with 'races:' as the top-level key, as a list
  write_yaml(list(races = race_list), yaml_path)
}
# save_powerof10_to_yaml(
#   first_name = "Alex",
#   surname = "Black",
#   club = "North Shields Poly",
#   event = c("HM", "Mar")
# )
# save_powerof10_to_yaml(
#   first_name = "Emily",
#   surname = "Black",
#   club = "Tyne Bridge",
#   event = c("HM", "Mar")
# )

get_page_dimensions <- function(page_size) {
  # Returns a list with width and height in mm for common page sizes
  sizes <- list(
    A5 = list(width = 148, height = 210),
    A4 = list(width = 210, height = 297),
    A3 = list(width = 297, height = 420),
    A2 = list(width = 420, height = 594),
    A1 = list(width = 594, height = 841),
    A0 = list(width = 841, height = 1189)
  )
  if (page_size %in% names(sizes)) {
    return(sizes[[page_size]])
  } else {
    stop(paste("Unknown page size:", page_size))
  }
}

#' Custom ggplot2 theme for memorable maps
#' @param base_size Base font size
#' @param base_family Base font family
#' @param bg_color Background color
#' @param aspect.ratio Aspect ratio of the plot
#' @return A ggplot2 theme object
#' @examples
theme_memory_maps <- function(
  base_size = 12,
  base_family = "Outfit-VariableFont_wght",
  bg_color = "#0a0e27",
  aspect.ratio
) {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      # A3 portrait dimensions: 297mm x 420mm
      aspect.ratio = 420 / 297
    )
}

#' Function to parse GPX file and extract track points
#' @param gpx_file Path to the GPX file
#' @return Data frame with latitude, longitude, elevation, and point order
parse_gpx <- function(gpx_file) {
  # Read the GPX file
  gpx_data <- read_xml(gpx_file)

  # Extract track points
  trkpts <- xml_find_all(gpx_data, ".//d1:trkpt", ns = xml_ns(gpx_data))

  # Extract coordinates and elevation
  coords_df <- data.frame(
    lat = as.numeric(xml_attr(trkpts, "lat")),
    lon = as.numeric(xml_attr(trkpts, "lon")),
    ele = as.numeric(xml_text(xml_find_first(
      trkpts,
      ".//d1:ele",
      ns = xml_ns(gpx_data)
    )))
  )

  # Remove any rows with missing coordinates
  coords_df <- coords_df[complete.cases(coords_df[, c("lat", "lon")]), ]

  # Add point order for plotting
  coords_df$point_order <- seq_len(nrow(coords_df))

  return(coords_df)
}

#' function to get max and min dists and elevation from multiple gpx files
#' @param gpx_dir Directory containing GPX files
#' @return List with min and max distance and elevation
#'
get_track_stats <- function(
  gpx_dir
) {
  all_dists <- c()
  all_elevs <- c()

  gpx_files <- list.files(
    gpx_dir,
    pattern = "\\.gpx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  for (gpx_file in gpx_files) {
    track_data <- parse_gpx(gpx_file)

    # Calculate cumulative distance along the track
    track_data$dist <- c(
      0,
      cumsum(
        distHaversine(
          cbind(
            track_data$lon[-nrow(track_data)],
            track_data$lat[-nrow(track_data)]
          ),
          cbind(track_data$lon[-1], track_data$lat[-1])
        )
      ) /
        1000
    ) # distance in km
    all_dists <- c(all_dists, track_data$dist)
    all_elevs <- c(all_elevs, track_data$ele)
  }

  return(list(
    min_dist = min(all_dists, na.rm = TRUE),
    max_dist = max(all_dists, na.rm = TRUE),
    min_elev = min(all_elevs, na.rm = TRUE),
    max_elev = max(all_elevs, na.rm = TRUE)
  ))
}

#' Get OSM map components (highways, streets, water, sea) for a bounding box and location
#' @param bbox Numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)
#' @param location String identifier for caching
#' @return List with OSM components
get_osm_components <- function(bbox, location) {
  location <- tolower(gsub(" ", "_", location))
  highways_path <- paste0("data/highways_", location, ".rds")
  streets_path <- paste0("data/streets_", location, ".rds")
  water_path <- paste0("data/water_", location, ".rds")
  coast_path <- paste0("data/coastlines_", location, ".rds")

  # if files exist, read from them
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
  list(highways = highways, streets = streets, water = water, sea = sea)
}

#' Function using OSM with overlaid title
#' @param gpx_file Path to GPX file
#' @param competitor_name Name of the competitor
#' @param entries
#' @param location Location string for map title
#' @param route_color Color for the route line
#' @param route_size Size of the route line
#' @param bg_color Background color for the map
#' @param street_color Color for streets
#' @param highway_color Color for highways
#' @param water_color Color for water bodies
#' @param text_color Color for title and subtitle text
#' @param font_family Font family for text
#' @param output_dir Directory to save the output map
#' @param with_elevation Boolean to include elevation chart
#' @param dpi DPI for saved image
#' @param page_size Page size (e.g., "A3", "A4")
#' @return Saves the map to the specified output directory
create_race_map <- function(
  gpx_file,
  competitor_name,
  entries, # Changed from race_year and race_time
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
  page_size
) {
  # Set text color to route color if not specified
  if (is.null(text_color)) {
    text_color <- route_color
  }

  dims <- get_page_dimensions(page_size)
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
      cumsum(
        distHaversine(
          cbind(
            track_data$lon[-nrow(track_data)],
            track_data$lat[-nrow(track_data)]
          ),
          cbind(track_data$lon[-1], track_data$lat[-1])
        )
      ) /
        1000
    ) # distance in km

    # Use only the current track's max distance and elevation
    max_dist <- max(track_data$dist, na.rm = TRUE)
    track_min_elev <- min(track_data$ele, na.rm = TRUE)
    track_max_elev <- max(track_data$ele, na.rm = TRUE)

    # Set baseline slightly below minimum (e.g., 5% below)
    baseline <- track_min_elev * 0.95

    # Elevation chart (filled line)
    p_elev <- ggplot(track_data, aes(x = dist, y = ele)) +
      geom_ribbon(
        aes(ymin = baseline, ymax = ele),
        fill = route_color,
        alpha = 0.35
      ) +
      geom_line(color = route_color, linewidth = 0.7) +
      scale_x_continuous(limits = c(0, max_dist), expand = c(0, 0)) +
      scale_y_continuous(
        limits = c(baseline, track_max_elev),
        expand = c(0, 0)
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA)
      )
  }

  # Calculate bounding box with padding that respects A3 portrait aspect ratio
  lat_range <- range(track_data$lat, na.rm = TRUE)
  lon_range <- range(track_data$lon, na.rm = TRUE)

  # Calculate initial ranges
  lat_span <- diff(lat_range)
  lon_span <- diff(lon_range)

  # Calculate current aspect ratio (accounting for latitude distortion)
  lat_center <- mean(lat_range)
  # Approximate conversion: 1 degree longitude ≈ cos(latitude) * 111km
  lon_to_lat_ratio <- cos(lat_center * pi / 180)
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

  # Get OSM map components
  osm <- get_osm_components(bbox, location)
  highways <- osm$highways
  streets <- osm$streets
  water <- osm$water
  sea <- osm$sea

  # Calculate precise positioning in coord space
  x_center <- mean(c(bbox[1], bbox[3]))
  y_range <- bbox[4] - bbox[2]

  # Title positioned from top (using coord space units)
  title_y <- bbox[4] - (y_range * 0.04)

  # Subtitle positioned below title, move up if many lines
  n_subtitle_lines <- length(entries)
  subtitle_y <- bbox[2] + (y_range * (0.09 + 0.035 * (n_subtitle_lines - 1)))

  # Font sizes in coord space units
  # Shrink title size if multiple words
  title_text <- if (is.null(location)) competitor_name else toupper(location)
  n_title_words <- length(strsplit(title_text, "[[:space:]]+")[[1]])
  title_scale <- max(0.7, 1 - 0.1 * (n_title_words - 1))
  title_size <- page_height * 0.07 * title_scale # 7% of page height, scaled
  subtitle_size <- page_height * 0.035 # 3.5% of page height

  # adjust route size based on page size
  route_size <- page_width * 0.004 # 0.4% of page width (adjust as needed)
  point_size <- page_width * 0.01 # 1% of page width

  # Create gradient data for top and bottom fades
  fade_height <- y_range * 0.20 # 15% fade at top and bottom
  n_steps <- 75 # Number of gradient steps

  # Top fade - create rectangles with increasing alpha
  top_fade <- data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[4] - fade_height + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[4] - fade_height + (1:n_steps) * (fade_height / n_steps),
    alpha = seq(0, 0.9, length.out = n_steps)
  )

  # Bottom fade - create rectangles with decreasing alpha
  bottom_fade <- data.frame(
    xmin = bbox[1],
    xmax = bbox[3],
    ymin = bbox[2] + (0:(n_steps - 1)) * (fade_height / n_steps),
    ymax = bbox[2] + (1:n_steps) * (fade_height / n_steps),
    alpha = seq(0.9, 0, length.out = n_steps)
  )

  # create subtitle text from entries
  subtitle_lines <- sapply(entries, function(entry) {
    paste(entry$race_year, entry$race_time, sep = " - ")
  })
  subtitle_text <- paste(subtitle_lines, collapse = "\n")

  # Build ggplot
  p_map <- ggplot() +
    # Water bodies (polygons and multipolygons)
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
    # Sea (coastline polygons)
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
    ) +
    # Route
    geom_path(
      data = track_data,
      aes(x = lon, y = lat),
      color = route_color,
      linewidth = route_size,
      alpha = 0.95,
      lineend = "round"
    ) +
    # Start/Finish
    geom_point(
      data = track_data[1, ],
      aes(x = lon, y = lat),
      color = route_color,
      size = point_size,
      shape = 21,
      fill = route_color,
      stroke = 2
    ) +
    geom_point(
      data = track_data[nrow(track_data), ],
      aes(x = lon, y = lat),
      color = route_color,
      size = point_size,
      shape = 21,
      fill = route_color,
      stroke = 2
    ) +
    # Top fade gradient
    geom_rect(
      data = top_fade,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = top_fade$alpha,
      inherit.aes = FALSE
    ) +
    # Bottom fade gradient
    geom_rect(
      data = bottom_fade,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bg_color,
      alpha = bottom_fade$alpha,
      inherit.aes = FALSE
    ) +
    # Title annotation (using vjust for precise control)
    annotate(
      "text",
      x = x_center,
      y = title_y,
      label = if (is.null(location)) {
        competitor_name
      } else {
        #toupper(paste(location, race_year, sep = " "))
        toupper(location)
      },
      size = title_size, # Convert mm to ggplot size units
      fontface = "bold",
      color = text_color,
      family = font_family,
      vjust = 1 # Anchor text at top
    ) +
    # Subtitle annotation
    annotate(
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
    labs(title = NULL, subtitle = NULL) +
    coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = FALSE
    ) +
    theme_memory_maps(bg_color = bg_color, aspect.ratio = target_aspect_ratio)

  # Add elevation chart to the bottom of the map
  # p <- p +
  #   annotation_custom(
  #     elev_grob,
  #     xmin = bbox[1],
  #     xmax = bbox[3],
  #     ymin = bbox[2] - (y_range * 0.18), # slightly below map
  #     ymax = bbox[2] + (y_range * 0.10) # up into map
  #   )

  if (with_elevation) {
    final_plot <- p_map +
      inset_element(
        p_elev,
        left = 0.05, # Start at far left
        bottom = 0.01, # Slightly above bottom
        right = 0.97, # End at far right
        top = 0.05, # Small height (15% of total)
        align_to = 'full'
      )
    plot(final_plot)
  } else {
    final_plot <- p_map
  }

  save_path <- file.path(
    output_dir,
    paste0(
      paste(location, gsub(" ", "_", competitor_name), sep = "-"),
      ".png"
    )
  )

  ggsave(
    filename = save_path,
    plot = final_plot,
    width = page_width,
    height = page_height,
    units = "mm",
    dpi = dpi
  )
}

#' Function to loop over multiple races
#' @param output_dir Directory to save maps
#' @param styles_path Path to styles YAML
#' @param races_path Path to races YAML
#' @return Saves maps for all races and styles
create_race_series <- function(
  output_dir = "maps",
  styles_path = "styles.yaml",
  races_path = "races.yaml"
) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load styles and races from separate YAML files
  styles_data <- yaml::read_yaml(styles_path)
  map_styling <- styles_data$styling
  races_data <- yaml::read_yaml(races_path)
  race_data <- races_data$races

  # outerloop over styles
  for (style in map_styling) {
    cat("Processing style:", style$name, "\n")

    style_dir <- file.path(output_dir, style$name, style$page_size)
    if (!dir.exists(style_dir)) {
      dir.create(style_dir, recursive = TRUE)
    }

    # inner Loop over entries in the list and create maps
    for (race in race_data) {
      cat("Processing race:", race$location, "\n")

      # Build entries list from the race data
      entries <- race$entries

      create_race_map(
        gpx_file = race$gpx_file,
        competitor_name = race$competitor_name,
        location = race$location,
        entries = entries, # Pass entire entries list
        output_dir = style_dir,
        route_color = style$route_color,
        bg_color = style$bg_color,
        street_color = style$street_color,
        highway_color = style$highway_color,
        water_color = style$water_color,
        with_elevation = style$with_elevation,
        dpi = style$dpi,
        page_size = style$page_size
      )
    }
  }
}

# ============================================================================
# Main
# ============================================================================

# Process all races (uncomment to run)
create_race_series(
  races_path = "alex_black_races.yaml",
  styles_path = "styles.yaml",
  output_dir = "outputs/alex"
)
