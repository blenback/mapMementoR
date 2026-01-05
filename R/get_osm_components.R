#' Get OSM map components (highways, streets, water, sea) for a bounding box and location
#' @param bbox Numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)
#' @param location String identifier for caching
#' @param cache_data Logical, whether to cache OSM data locally
#' @param components Character vector specifying which OSM components to include. Any combination of 'highways', 'streets', 'water', 'coast'. Defaults to all.
#' @return List with selected OSM components
#' @export
get_osm_components <- function(
  bbox,
  location,
  cache_data = TRUE,
  components = c("highways", "streets", "water", "coast")
) {
  location <- tolower(gsub(" ", "_", location))
  cache_dir <- base::file.path(base::getwd(), "data_cache_tmp")
  if (cache_data && !base::dir.exists(cache_dir)) {
    if (base::interactive()) {
      response <- base::readline(
        paste0(
          "OSM cache directory '",
          cache_dir,
          "' does not exist.\n",
          "Allow creation for .rds cache files? [y/N]: "
        )
      )
      if (!tolower(response) %in% c("y", "yes")) {
        stop("User did not allow cache directory creation.")
      }
    } else {
      stop(
        "Cache directory does not exist and cannot prompt in non-interactive mode."
      )
    }
    base::dir.create(cache_dir, recursive = TRUE)
  }
  highways_path <- base::file.path(
    cache_dir,
    paste0("highways_", location, ".rds")
  )
  streets_path <- base::file.path(
    cache_dir,
    paste0("streets_", location, ".rds")
  )
  water_path <- base::file.path(cache_dir, paste0("water_", location, ".rds"))
  coast_path <- base::file.path(
    cache_dir,
    paste0("coastlines_", location, ".rds")
  )

  # Initialize output list
  out <- list()

  if (cache_data) {
    # Read or create cache for each requested component
    if ("highways" %in% components) {
      if (base::file.exists(highways_path)) {
        highways <- base::readRDS(highways_path)
      } else {
        highways <- bbox %>%
          osmdata::opq() %>%
          osmdata::add_osm_feature(
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
          osmdata::osmdata_sf()
        base::saveRDS(highways, highways_path)
      }
      out$highways <- highways
    }
    if ("streets" %in% components) {
      if (base::file.exists(streets_path)) {
        streets <- base::readRDS(streets_path)
      } else {
        streets <- bbox %>%
          osmdata::opq() %>%
          osmdata::add_osm_feature(
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
          osmdata::osmdata_sf()
        base::saveRDS(streets, streets_path)
      }
      out$streets <- streets
    }
    if ("water" %in% components) {
      if (base::file.exists(water_path)) {
        water <- base::readRDS(water_path)
      } else {
        water <- bbox %>%
          osmdata::opq() %>%
          osmdata::add_osm_feature(key = "natural", value = "water") %>%
          osmdata::osmdata_sf()
        base::saveRDS(water, water_path)
      }
      out$water <- water
    }
    if ("coast" %in% components) {
      if (base::file.exists(coast_path)) {
        sea <- base::readRDS(coast_path)
      } else {
        coastlines <- bbox %>%
          osmdata::opq() %>%
          osmdata::add_osm_feature(key = "natural", value = "coastline") %>%
          osmdata::osmdata_sf()
        if (
          !is.null(coastlines$osm_lines) && base::nrow(coastlines$osm_lines) > 0
        ) {
          coastline_lines <- coastlines$osm_lines %>% sf::st_cast("LINESTRING")
          bb_rect <- base::data.frame(
            lat = c(bbox[2], bbox[4]),
            lon = c(bbox[1], bbox[3])
          ) %>%
            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc()
          bb_rect_split <- bb_rect %>%
            lwgeom::st_split(coastline_lines) %>%
            sf::st_collection_extract("POLYGON")
          sea <- bb_rect_split[2]
        } else {
          sea <- NULL
        }
        base::saveRDS(sea, coast_path)
      }
      out$coast <- sea
    }
  } else {
    # Always fetch OSM data, do not use cache
    if ("highways" %in% components) {
      highways <- bbox %>%
        osmdata::opq() %>%
        osmdata::add_osm_feature(
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
        osmdata::osmdata_sf()
      out$highways <- highways
    }
    if ("streets" %in% components) {
      streets <- bbox %>%
        osmdata::opq() %>%
        osmdata::add_osm_feature(
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
        osmdata::osmdata_sf()
      out$streets <- streets
    }
    if ("water" %in% components) {
      water <- bbox %>%
        osmdata::opq() %>%
        osmdata::add_osm_feature(key = "natural", value = "water") %>%
        osmdata::osmdata_sf()
      out$water <- water
    }
    if ("coast" %in% components) {
      coastlines <- bbox %>%
        osmdata::opq() %>%
        osmdata::add_osm_feature(key = "natural", value = "coastline") %>%
        osmdata::osmdata_sf()
      if (
        !is.null(coastlines$osm_lines) && base::nrow(coastlines$osm_lines) > 0
      ) {
        coastline_lines <- coastlines$osm_lines %>% sf::st_cast("LINESTRING")
        bb_rect <- base::data.frame(
          lat = c(bbox[2], bbox[4]),
          lon = c(bbox[1], bbox[3])
        ) %>%
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
          sf::st_bbox() %>%
          sf::st_as_sfc()
        bb_rect_split <- bb_rect %>%
          lwgeom::st_split(coastline_lines) %>%
          sf::st_collection_extract("POLYGON")
        sea <- bb_rect_split[2]
      } else {
        sea <- NULL
      }
      out$coast <- sea
    }
  }
  out
}
