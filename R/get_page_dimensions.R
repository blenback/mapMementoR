#' Returns width and height in mm for common page sizes
#' @param page_size Page size string (e.g., "A5", "A4")
#' @param orientation Page orientation: "portrait" (default) or "landscape"
#' @return List with width and height
#' @export
get_page_dimensions <- function(page_size, orientation = "portrait") {
  sizes <- list(
    A5 = list(width = 148, height = 210),
    A4 = list(width = 210, height = 297),
    A3 = list(width = 297, height = 420),
    A2 = list(width = 420, height = 594),
    A1 = list(width = 594, height = 841),
    A0 = list(width = 841, height = 1189)
  )
  if (page_size %in% base::names(sizes)) {
    dims <- sizes[[page_size]]
    if (orientation == "landscape") {
      dims <- list(width = dims$height, height = dims$width)
    }
    return(dims)
  } else {
    base::stop(base::paste("Unknown page size:", page_size))
  }
}
