#' Parse GPX file and extract track points
#' @param gpx_file Path to the GPX file
#' @return Data frame with latitude, longitude, elevation, and point order
#' @export
parse_gpx <- function(gpx_file) {
  if (!file.exists(gpx_file)) {
    stop("GPX file does not exist: ", gpx_file)
  }
  gpx_data <- xml2::read_xml(gpx_file)
  trkpts <- xml2::xml_find_all(
    gpx_data,
    ".//d1:trkpt",
    ns = xml2::xml_ns(gpx_data)
  )
  coords_df <- base::data.frame(
    lat = base::as.numeric(xml2::xml_attr(trkpts, "lat")),
    lon = base::as.numeric(xml2::xml_attr(trkpts, "lon")),
    ele = base::as.numeric(xml2::xml_text(xml2::xml_find_first(
      trkpts,
      ".//d1:ele",
      ns = xml2::xml_ns(gpx_data)
    )))
  )
  coords_df <- coords_df[stats::complete.cases(coords_df[, c("lat", "lon")]), ]
  coords_df$point_order <- base::seq_len(base::nrow(coords_df))
  return(coords_df)
}
