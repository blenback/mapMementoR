#' Loop over multiple races and styles to create maps
#' @param output_dir Directory to save maps
#' @param styles_path Path to styles YAML
#' @param races_path Path to races YAML
#' @param cache_data Whether to cache OSM and hillshade data
#' @return Saves maps for all races and styles
#' @export
#' @examples
#' #' # Create race series
#' create_race_series(
#'   output_dir = "maps",
#'  styles_path = "data/styles.yaml",
#'  races_path = "data/races.yaml",
#'  cache_data = TRUE
#' )
create_race_series <- function(
  output_dir = "maps",
  styles_path = "styles.yaml",
  races_path = "races.yaml",
  cache_data = TRUE
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
        page_size = style$page_size,
        orientation = style$orientation,
        with_OSM = style$with_OSM,
        with_hillshade = style$with_hillshade,
        cache_data = cache_data
      )
    }
  }
}
