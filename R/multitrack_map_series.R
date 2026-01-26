#' Loop over multiple track sets and styles to create multitrack maps
#'
#' This function processes multiple track sets (groups of GPX files) and generates
#' maps with different visual styles. If route colors are not specified in the YAML
#' configuration, it automatically generates visually distinct colors using the
#' generate_track_colors() function based on each style's route_color.
#'
#' @param output_dir Directory to save maps
#' @param styles Character vector of style names to use (built-in or custom) see `.mapMementoR_builtin_styles` in R/styles.R for built-in styles
#' @param custom_styles Optional named list of custom styles, each a list of color settings (route_color, bg_color, street_color, highway_color, water_color)
#' @param track_sets_path Path to track sets YAML file
#' @param cache_data Whether to cache OSM and hillshade data
#' @param dpi Image resolution in dots per inch
#' @param page_size Page size (e.g., "A5", "A4")
#' @param base_size Base font size for map text
#' @param orientation Page orientation (e.g., "portrait", "landscape")
#' @param with_elevation Boolean to include elevation charts
#' @param with_labels Boolean to include text labels along routes
#' @param label_spacing Spacing for labels along the path
#' @param label_size Size of route labels
#' @param with_OSM Boolean to include OSM background features
#' @param with_hillshade Boolean to include hillshade (elevation relief) background
#' @param components Character vector specifying which OSM components to include. Any combination of 'highways', 'streets', 'water', 'coast'. Defaults to all.
#' @param fade_directions Character vector specifying which sides to apply fade gradients to. Any combination of 'top', 'bottom', 'left', 'right'. Defaults to c('top', 'bottom').
#' @param crop_shape Optional shape to crop map to. Options: "circle", "ellipse". If NULL, no cropping is applied.
#' @param track_color_method Method for generating track colors when not specified. Options:
#'   - "hue_shift": Varies hue while maintaining similar saturation/lightness (default)
#'   - "complementary": Creates complementary and analogous colors
#'   - "gradient": Creates a gradient with varying lightness and saturation
#' @return Saves maps for all track sets and styles
#' @export
#' @examples
#' multitrack_map_series(
#'   output_dir = "outputs",
#'   styles = c("Dark", "Emerald"),
#'   custom_styles = list(MyStyle = list(route_color = "#123456", ...)),
#'   track_sets_path = "data-raw/track_sets.yaml",
#'   dpi = 300,
#'   page_size = "A4",
#'   base_size = 12,
#'   orientation = "portrait",
#'   with_elevation = TRUE,
#'   with_labels = FALSE,
#'   with_OSM = TRUE,
#'   with_hillshade = FALSE,
#'   cache_data = TRUE,
#'   components = c("highways", "streets", "water", "coast")
#' )
multitrack_map_series <- function(
  output_dir = "outputs",
  styles = c("Dark"),
  custom_styles = NULL,
  track_sets_path = "track_sets.yaml",
  cache_data = TRUE,
  dpi = 300,
  page_size = "A4",
  base_size = 12,
  orientation = "portrait",
  with_elevation = TRUE,
  with_labels = FALSE,
  label_spacing = 0.5,
  label_size = 4,
  with_OSM = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL,
  track_color_method = "hue_shift"
) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load track sets from YAML
  track_sets_data <- yaml::read_yaml(track_sets_path)
  track_sets <- track_sets_data$track_sets

  # Combine built-in and custom styles
  all_styles <- .mapMementoR_builtin_styles
  if (!is.null(custom_styles)) {
    all_styles[names(custom_styles)] <- custom_styles
  }
  # Select only requested styles
  selected_styles <- all_styles[styles]

  # If crop_shape is NULL then add full_page as a dir
  if (is.null(crop_shape)) {
    crop_tag <- "full_page"
  } else {
    crop_tag <- crop_shape
  }

  for (i in seq_along(selected_styles)) {
    style <- selected_styles[[i]]
    style_name <- names(selected_styles)[[i]]
    cat("Processing style:", style_name, "\n")
    style_dir <- file.path(
      output_dir,
      style_name,
      page_size,
      crop_tag
    )
    if (!dir.exists(style_dir)) {
      dir.create(style_dir, recursive = TRUE)
    }

    for (track_set in track_sets) {
      cat("Processing track set:", track_set$map_title, "\n")

      # Generate route colors if not specified in the track set
      n_tracks <- length(track_set$gpx_files)
      if (!is.null(track_set$route_colors)) {
        route_colors <- track_set$route_colors
      } else {
        # Generate visually distinct colors from the style's route color
        route_colors <- generate_track_colors(
          base_color = style$route_color,
          n_colors = n_tracks,
          method = track_color_method
        )
      }

      create_multitrack_memento_map(
        gpx_files = track_set$gpx_files,
        track_labels = track_set$track_labels,
        elev_labels = track_set$elev_labels,
        route_colors = route_colors,
        with_labels = with_labels,
        label_spacing = label_spacing,
        label_size = label_size,
        map_title = track_set$map_title,
        cache_string = "zurich",
        route_size = 1.2,
        bg_color = style$bg_color,
        street_color = style$street_color,
        highway_color = style$highway_color,
        water_color = style$water_color,
        text_color = NULL,
        font_family = track_set$font_family %||% "Outfit-VariableFont_wght",
        output_dir = style_dir,
        with_elevation = with_elevation,
        dpi = dpi,
        page_size = page_size,
        orientation = orientation,
        base_size = base_size,
        with_OSM = with_OSM,
        cache_data = cache_data,
        with_hillshade = with_hillshade,
        components = components,
        fade_directions = fade_directions,
        crop_shape = crop_shape
      )
    }
  }
}

# Helper for null coalescing (for font_family fallback)
`%||%` <- function(a, b) if (!is.null(a)) a else b
