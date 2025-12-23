#' Loop over multiple races and styles to create maps
#' @param output_dir Directory to save maps
#' @param styles Character vector of style names to use (built-in or custom) see `.mapMementoR_builtin_styles` in R/styles.R for built-in styles
#' @param custom_styles Optional named list of custom styles, each a list of color settings (route_color, bg_color, street_color, highway_color, water_color)
#' @param races_path Path to races YAML
#' @param cache_data Whether to cache OSM and hillshade data
#' @param dpi Image resolution in dots per inch
#' @param page_size Page size (e.g., "A5", "A4")
#' @param orientation Page orientation (e.g., "portrait", "landscape")
#' @param with_elevation Boolean to include elevation chart
#' @param with_OSM Boolean to include OSM background features
#' @param with_hillshade Boolean to include hillshade (elevation relief) background
#' @return Saves maps for all races and styles
#' @export
#' @examples
#' create_race_series(
#'   output_dir = "maps",
#'   styles = c("Dark", "Emerald"),
#'   custom_styles = list(MyStyle = list(route_color = "#123456", ...)),
#'   races_path = "data/races.yaml",
#'   dpi = 300,
#'   page_size = "A5",
#'   orientation = "portrait",
#'   with_elevation = TRUE,
#'   with_OSM = TRUE,
#'   with_hillshade = FALSE,
#'   cache_data = TRUE
#' )
create_race_series <- function(
  output_dir = "maps",
  styles = c("Dark"),
  custom_styles = NULL,
  races_path = "races.yaml",
  cache_data = TRUE,
  dpi = 300,
  page_size = "A5",
  orientation = "portrait",
  with_elevation = TRUE,
  with_OSM = TRUE,
  with_hillshade = FALSE
) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load races from YAML
  races_data <- yaml::read_yaml(races_path)
  race_data <- races_data$races

  # Combine built-in and custom styles
  all_styles <- .mapMementoR_builtin_styles
  if (!is.null(custom_styles)) {
    all_styles[names(custom_styles)] <- custom_styles
  }
  # Select only requested styles
  selected_styles <- all_styles[styles]

  for (style in selected_styles) {
    cat("Processing style:", style$name %||% names(style), "\n")
    style_dir <- file.path(
      output_dir,
      style$name %||% names(style),
      page_size
    )
    if (!dir.exists(style_dir)) {
      dir.create(style_dir, recursive = TRUE)
    }
    for (race in race_data) {
      cat("Processing race:", race$location, "\n")
      entries <- race$entries
      create_race_map(
        gpx_file = race$gpx_file,
        competitor_name = race$competitor_name,
        location = race$location,
        entries = entries,
        output_dir = style_dir,
        route_color = style$route_color,
        bg_color = style$bg_color,
        street_color = style$street_color,
        highway_color = style$highway_color,
        water_color = style$water_color,
        with_elevation = with_elevation,
        dpi = dpi,
        page_size = page_size,
        orientation = orientation,
        with_OSM = with_OSM,
        with_hillshade = with_hillshade,
        cache_data = cache_data
      )
    }
  }
}

# Helper for null coalescing (for style$name fallback)
`%||%` <- function(a, b) if (!is.null(a)) a else b
