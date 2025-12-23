#' Get OSM map components (highways, streets, water, sea) for a bounding box and location
#' @param bbox Numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)
#' @param location String identifier for caching
#' @param cache_data Logical, whether to cache OSM data locally
#' @return List with OSM components
#' @export
get_osm_components <- function(bbox, location, cache_data = TRUE) {
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

  if (cache_data) {
    if (
      all(
        base::file.exists(highways_path),
        base::file.exists(streets_path),
        base::file.exists(water_path),
        base::file.exists(coast_path)
      )
    ) {
      highways <- base::readRDS(highways_path)
      streets <- base::readRDS(streets_path)
      water <- base::readRDS(water_path)
      sea <- base::readRDS(coast_path)
    } else {
      if (!base::file.exists(highways_path)) {
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
      } else {
        highways <- base::readRDS(highways_path)
      }
      if (!base::file.exists(streets_path)) {
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
      } else {
        streets <- base::readRDS(streets_path)
      }
      if (!base::file.exists(water_path)) {
        water <- bbox %>%
          osmdata::opq() %>%
          osmdata::add_osm_feature(key = "natural", value = "water") %>%
          osmdata::osmdata_sf()
        base::saveRDS(water, water_path)
      } else {
        water <- base::readRDS(water_path)
      }
      if (!base::file.exists(coast_path)) {
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
      } else {
        sea <- base::readRDS(coast_path)
      }
    }
  } else {
    # Always fetch OSM data, do not use cache
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
    water <- bbox %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(key = "natural", value = "water") %>%
      osmdata::osmdata_sf()
    coastlines <- bbox %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(key = "natural", value = "coastline") %>%
      osmdata::osmdata_sf()
    coastline_lines <- coastlines$osm_lines %>% sf::st_cast("LINESTRING")
    if (
      !is.null(coastlines$osm_lines) && base::nrow(coastlines$osm_lines) > 0
    ) {
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
  }
  list(highways = highways, streets = streets, water = water, sea = sea)
}
