# Example script to create multitrack maps for multiple track sets and styles
library(mapMementoR)

# Create maps for all track sets using the Dark style
multitrack_map_series(
  output_dir = "outputs",
  styles = c("Dark", "Emerald", "Ghost", "Nautical", "Zen", "Obsidian"),
  track_sets_path = "data-raw/track_sets.yaml",
  dpi = 300,
  page_size = "A4",
  base_size = 12,
  orientation = "portrait",
  with_elevation = TRUE,
  with_labels = TRUE,
  label_spacing = 0.5,
  label_size = 4,
  with_OSM = TRUE,
  cache_data = TRUE,
  with_hillshade = FALSE,
  components = c("highways", "streets", "water", "coast"),
  fade_directions = c("top", "bottom"),
  crop_shape = NULL,
  track_color_method = "gradient"
)
