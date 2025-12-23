#' Get max/min distance and elevation from multiple GPX files
#' @param gpx_dir Directory containing GPX files
#' @return List with min and max distance and elevation
#' @export
get_track_stats <- function(gpx_dir) {
  all_dists <- c()
  all_elevs <- c()
  gpx_files <- base::list.files(
    gpx_dir,
    pattern = "\\.gpx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  for (gpx_file in gpx_files) {
    track_data <- parse_gpx(gpx_file)
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
    all_dists <- base::c(all_dists, track_data$dist)
    all_elevs <- base::c(all_elevs, track_data$ele)
  }
  return(base::list(
    min_dist = base::min(all_dists, na.rm = TRUE),
    max_dist = base::max(all_dists, na.rm = TRUE),
    min_elev = base::min(all_elevs, na.rm = TRUE),
    max_elev = base::max(all_elevs, na.rm = TRUE)
  ))
}
