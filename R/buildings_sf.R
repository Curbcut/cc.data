#' Build buildings and place each Province's processed data in a folder
#'
#' `buildings_sf()` uses a downloads building data from
#' Open Street Map and Microsoft's Canadian Building Footprints, processed is
#' and save each province's dataset in a destination folder.
#'
#' @param DA_processed_table <`sf data.frame`> A \code{DA} sf data.frame from
#' which the DA ID will be attached to every single building.
#' @param dest_folder <`character`> Destination folder where each processed
#' building dataset will be saved.
#' @param OSM_cache <`logical`> Should the OSM building data be downloaded
#' and processed with the `osmextract` package, or should it be read from the
#' cache. The cache needs to be set as a variable environment
#' `OSMEXT_DOWNLOAD_DIRECTORY`.
#'
#' @return Returns an error or nothing if ran successfully. The buildings of each
#' province are saved in the destination folder.
#' @export
buildings_sf <- function(DA_processed_table, dest_folder, OSM_cache = TRUE) {

  # If retrieve from cache, have a cache destination set --------------------

  if (OSM_cache && Sys.getenv("OSMEXT_DOWNLOAD_DIRECTORY") == "") {
    stop(paste0("You set to retrieve OSM data from the cache but you do not ",
                "have an `osmextract` cache directory. Set it using ",
                "the `OSMEXT_DOWNLOAD_DIRECTORY` variable environment in ",
                ".Renviron (Read the `osmextract::oe_read` doc)."))
  }


  # Check if destination folder exists --------------------------------------

  if (!file.exists(dest_folder)) dir.create(dest_folder)


  # Process for each province -----------------------------------------------

  pb <- progressr::progressor(steps = nrow(cc.data::buildings_osm_ms_keys))

  future.apply::future_lapply(seq_len(nrow(cc.data::buildings_osm_ms_keys)), \(table_n) {
    osm_pbf <- cc.data::buildings_osm_ms_keys$osm_link[table_n]
    ms_key <- cc.data::buildings_osm_ms_keys$ms_code[table_n]


    # Download OSM pbf (buildings) --------------------------------------------

    build <- osmextract::oe_read(osm_pbf, layer = "multipolygons",
                                 quiet = TRUE, force_download = !OSM_cache)

    # Only keep buildings and only keep valid spatial features
    building <- build[!is.na(build$building), ]
    building <- sf::st_make_valid(building)
    building <- building[sf::st_is_valid(building), ]
    building <- building[!sf::st_is_empty(building), ]
    building <- sf::st_transform(building, 3347)
    building <- sf::st_cast(building, "MULTIPOLYGON")

    # Subset
    building <- building[, c("osm_id", "geometry")]
    names(building) <- c("osm_ID", "geometry")

    # Add ID
    building$ID <- paste0("b_", seq_along(building$osm_ID))
    building <- building[, c("ID", "osm_ID", "geometry")]

    # Get centroid for self-intersection, and find self-intersections
    building_centroid <- suppressWarnings(sf::st_centroid(building))
    self_intersects <- sf::st_intersects(building_centroid, building)
    to_merge <- self_intersects[lengths(self_intersects) > 1]
    # Reducer function, and get reduced lists of intersections
    reduce <- function(x) {
      Reduce(function(a, b) {
        merge_index <- lapply(a, intersect, b)
        if (sum(lengths(merge_index)) > 0) {
          merge_index <- which(lengths(merge_index) > 0)
          merged <- a[merge_index]
          merged <- unlist(merged)
          merged <- union(merged, b)
          merged <- list(sort(merged))
          not_merged <- a[-merge_index]
          out <- c(merged, not_merged)
        } else {
          out <- c(a, list(b))
        }
      }, x, init = list())
    }
    merged <- reduce(to_merge)
    # Buildings that will not be merged
    building_remaining <- building[!building$ID %in% unlist(merged), ]
    # Merge buildings that have self-intersection
    building_merged <- lapply(merged, \(x) {
      z <- building[building$ID %in% paste0("b_", x), ]
      geo <- sf::st_union(z)
      z <- z[1, ]
      z$geometry <- geo
      sf::st_cast(z, "MULTIPOLYGON")
    })
    building_merged <- data.table::rbindlist(building_merged)
    building_merged <- tibble::as_tibble(building_merged)
    building_merged <- sf::st_as_sf(building_merged)
    # Bind everything together
    building_merged <- sf::st_cast(building_merged, "MULTIPOLYGON")
    building_merged <- sf::st_make_valid(building_merged)
    building_merged <- building_merged[!sf::st_is_empty(building_merged$geometry), ]
    building <- rbind(building_remaining, building_merged)
    building <- sf::st_set_agr(building, "constant")


    # Get Microsoft buildings -------------------------------------------------
    # Source <`https://github.com/Microsoft/CanadianBuildingFootprints`>

    # Download building json from the source
    province_url <-
      paste0(
        "https://usbuildingdata.blob.core.windows.net/canadian-buildings-v2/",
        ms_key, ".zip"
      )
    tmp <- tempfile(pattern = ms_key, fileext = ".zip")
    utils::download.file(province_url, destfile = tmp)
    # Manipulation to get the data from the zip file
    geojson_tmp <- tempfile(pattern = "ms_buildings", fileext = ".geojson")
    connection_to_geojson <- unz(tmp, paste0(ms_key, ".geojson"))
    writeLines(readLines(connection_to_geojson), geojson_tmp)
    ms_building <- geojsonsf::geojson_sf(geojson_tmp)
    close(connection_to_geojson)

    # Only keep polygons which don't intersect with buildings
    ms_building <- sf::st_make_valid(ms_building)
    ms_building <- sf::st_transform(ms_building, 3347)
    ms_building <- ms_building[
      lengths(sf::st_intersects(ms_building$geometry, building$geometry)) == 0,
    ]

    # Make valid and add temporary ID
    ms_building <- sf::st_cast(ms_building, "MULTIPOLYGON")
    ms_building <- ms_building[!sf::st_is_empty(ms_building$geometry), ]
    ms_building$ID <- as.character(seq_along(ms_building$geometry))

    # Bind OSM buildings and MS buildings
    ms_building$osm_ID <- NA
    building <- rbind(building, ms_building)
    building_area <- get_area(building$geometry)
    building <- building[building_area > 10, ]
    building$ID <- paste0("b", ms_key, seq_along(building$geometry))
    building <- sf::st_set_agr(building, "constant")
    building <- sf::st_make_valid(building)


    # Add DA ID ---------------------------------------------------------------

    DA_table <- DA_processed_table[, "ID"]
    names(DA_table)[1] <- "DA_ID"

    building_centroids <- suppressWarnings(sf::st_centroid(building))
    da_joined <- sf::st_join(building_centroids, DA_table)
    da_joined <- sf::st_drop_geometry(da_joined[, c("ID", "DA_ID")])
    building <- merge(building, da_joined, by = "ID")


    # Consolidate and clean output --------------------------------------------

    building$name <- NA_character_
    building <- building[, c(
      "ID", "name", "DA_ID", "osm_ID", "geometry"
    )]
    building <- sf::st_cast(building, "MULTIPOLYGON")
    building <- sf::st_make_valid(building)
    building <- building[!sf::st_is_empty(building$geometry), ]
    building <- sf::st_set_agr(building, "constant")
    building <- tibble::as_tibble(building)
    building <- sf::st_as_sf(building)


    # Save iteration in the destination folder --------------------------------

    if (!grepl("/$", dest_folder)) dest_folder <- paste0(dest_folder, "/")
    qs::qsave(building, file = paste0(dest_folder, ms_key, ".qs"))
    pb()

  }, future.seed = NULL) |> invisible()

}
