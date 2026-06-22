################################################################################
## Residential zoning — intersection with dissemination areas (DA)
##
## Reusable functions to intersect a zoning dataset (polygons with a usage
## classification) against census DAs, compute clipped area per usage, and
## produce df_long tables ready for cc.pipe. The heavy intersection step runs
## in parallel via mirai, following the cc.cmhc make_par_env() pattern.
################################################################################

#' Default residential usage types
#'
#' Character vector of usage classes used by default across this module's
#' functions. Override via each function's `usage_types` argument.
ZONING_USAGE_TYPES <- c(
  "single_detached", "semi_detached", "duplex", "triplex",
  "townhouse", "row_house", "mid_rise", "high_rise",
  "collective", "mobile_home", "accessory", "other"
)

#' Read a zoning dataset, validated and projected to metres
#'
#' Reads a GeoJSON (or any format readable by `sf::st_read`), repairs invalid
#' geometries, and reprojects to a metric CRS. Adds a stable row key `zone_row`
#' so usage flags can be re-joined to the intersection pieces without carrying
#' the list-column through mirai.
#'
#' @param path Character. Path to the zoning file.
#' @param proj_crs Integer. Target (metric) projection CRS. Default 3347.
#' @return An sf object in `proj_crs`, with an integer `zone_row` key.
#' @export
zoning_read_zones <- function(path, proj_crs = 3347L) {
  zones <- sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(proj_crs)
  zones$zone_row <- seq_len(nrow(zones))
  message(sprintf("Loaded %d zoning features", nrow(zones)))
  zones
}

#' Read DA polygons from cc.data, validated and projected to metres
#'
#' @param census_year Character. Census year. Default "2021".
#' @param proj_crs Integer. Target (metric) projection CRS. Default 3347.
#' @return An sf object with columns `id` + `geometry` in `proj_crs`.
#' @export
zoning_read_da <- function(census_year = "2021", proj_crs = 3347L) {
  da_raw <- cc.data::census_empty_geometries(
    census_scales = "DA",
    census_years  = census_year
  )
  da <- da_raw$DA[[census_year]] |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    sf::st_transform(proj_crs) |>
    dplyr::rename(id = ID)
  message(sprintf("Loaded %d DA polygons", nrow(da)))
  da
}

#' Build a usage-flag lookup: one logical column per usage type, keyed by row
#'
#' Flattens the usage list-column into a plain data.frame of TRUE/FALSE flags.
#' No geometry and no list-column, so it joins fast and serializes cleanly for
#' mirai.
#'
#' @param zones An sf zoning object with `zone_row` + the usage list-column.
#' @param usage_col Character. Name of the usage list-column.
#'   Default "usage_classification".
#' @param usage_types Character. Vector of usage flags.
#' @return A data.frame with `zone_row` + one logical column per usage.
zoning_usage_flags <- function(zones,
                               usage_col = "usage_classification",
                               usage_types = ZONING_USAGE_TYPES) {
  vals <- zones[[usage_col]]
  flags <- lapply(usage_types, function(u) {
    vapply(vals, function(v) u %in% v, logical(1))
  })
  names(flags) <- usage_types
  cbind(data.frame(zone_row = zones$zone_row), as.data.frame(flags))
}

#' Intersect one chunk of zones with the DA layer, return pieces as a table
#'
#' Each output row is one (zone piece x DA) with its DA id, the originating
#' `zone_row`, and the clipped area in m2. No geometry is returned (dropped
#' after area is computed) so results are light to collect.
#'
#' @param zone_rows Integer. `zone_row` values for this chunk.
#' @param zones An sf zoning object (metric CRS, with `zone_row`).
#' @param da An sf DA object (metric CRS, with `id`).
#' @return A data.frame (zone_row, id, area_m2), or NULL if no overlap.
zoning_intersect_chunk <- function(zone_rows, zones, da) {
  z <- zones[zones$zone_row %in% zone_rows, c("zone_row")]

  pieces <- suppressWarnings(sf::st_intersection(z, da))
  pieces <- pieces[!sf::st_is_empty(sf::st_geometry(pieces)), ]
  if (nrow(pieces) == 0) return(NULL)

  pieces$area_m2 <- as.numeric(sf::st_area(pieces))

  pieces |>
    sf::st_drop_geometry() |>
    dplyr::select(zone_row, id, area_m2) |>
    dplyr::filter(area_m2 > 0)
}

#' Build a self-contained environment for mirai daemon serialization
#'
#' Creates an environment holding the named objects passed in, with
#' `globalenv()` as parent, and rebinds every function's enclosing environment
#' to this environment so workers are self-contained.
#'
#' @param ... Named objects (functions, data, constants) the worker needs.
#' @return An environment with parent `globalenv()`.
zoning_make_par_env <- function(...) {
  objs <- list(...)
  env <- list2env(objs, parent = globalenv())
  for (nm in names(objs)) {
    if (is.function(env[[nm]])) {
      environment(env[[nm]]) <- env
    }
  }
  env
}

#' Build the chunk task grid — one plain integer row per chunk
#'
#' Passes only a chunk index to each worker (a simple integer column, which
#' `mirai_map` handles cleanly). The worker derives its own slice of zone_rows
#' from chunk_id + chunk_size, so no list-column crosses the daemon boundary.
#'
#' @param zones An sf zoning object with `zone_row`.
#' @param chunk_size Integer. Zones per chunk. Default 1000.
#' @return A data.frame with one `chunk_id` per chunk.
zoning_build_chunks <- function(zones, chunk_size = 1000L) {
  n_chunks <- ceiling(nrow(zones) / chunk_size)
  data.frame(chunk_id = seq_len(n_chunks))
}

#' Run the intersection over all chunks, in parallel with progress
#'
#' The caller is responsible for starting mirai daemons
#' (`mirai::daemons(n)`) before the call and stopping them afterwards.
#'
#' @param zones An sf zoning object (metric CRS).
#' @param da An sf DA object (metric CRS).
#' @param chunk_size Integer. Zones per chunk. Default 1000.
#' @return A data.frame (zone_row, id, area_m2) for every piece, all chunks.
#' @export
zoning_intersect_all <- function(zones, da, chunk_size = 1000L) {
  chunks <- zoning_build_chunks(zones, chunk_size)
  message(sprintf(
    "Intersecting %d zones in %d chunks of %d...",
    nrow(zones), nrow(chunks), chunk_size
  ))

  .worker <- function(chunk_id) {
    suppressPackageStartupMessages({
      library(sf)
      library(dplyr)
    })
    start <- (chunk_id - 1L) * chunk_size + 1L
    end   <- min(chunk_id * chunk_size, nrow(zones))
    zone_rows <- zones$zone_row[start:end]
    zoning_intersect_chunk(zone_rows, zones = zones, da = da)
  }

  par_env <- zoning_make_par_env(
    zones                  = zones,
    da                     = da,
    chunk_size             = chunk_size,
    zoning_intersect_chunk = zoning_intersect_chunk
  )
  environment(.worker) <- par_env

  mp <- mirai::mirai_map(chunks, .worker)
  res <- mp[.progress]

  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    stop(sprintf(
      "Intersection failed in %d chunk(s). First: chunk %d\n%s",
      length(idx), idx[1], as.character(res[[idx[1]]])
    ), call. = FALSE)
  }

  dplyr::bind_rows(res)
}

#' Turn intersection pieces into one df_long table per usage type
#'
#' Joins the usage flags onto the pieces, then for each usage sums m2 by DA.
#' Pure data-frame work — no spatial ops.
#'
#' @param pieces A data.frame (zone_row, id, area_m2) from `zoning_intersect_all()`.
#' @param flags A data.frame (zone_row + one logical column per usage).
#' @param time Character. Time-period value. Default "2026".
#' @param geo_vintage Character. Boundary vintage. Default "2021".
#' @param usage_types Character. Vector of usage flags.
#' @return A named list of df_long tibbles (id, time, geo_vintage, value).
#' @export
zoning_sum_by_usage <- function(pieces, flags,
                                time = "2026",
                                geo_vintage = "2021",
                                usage_types = ZONING_USAGE_TYPES) {
  pf <- dplyr::left_join(pieces, flags, by = "zone_row")

  tables <- lapply(usage_types, function(u) {
    df <- pf |>
      dplyr::filter(.data[[u]]) |>
      dplyr::group_by(id) |>
      dplyr::summarise(value = sum(area_m2), .groups = "drop") |>
      dplyr::filter(value > 0) |>
      dplyr::mutate(time = time, geo_vintage = geo_vintage) |>
      dplyr::select(id, time, geo_vintage, value) |>
      tibble::as_tibble()
    if (nrow(df) == 0) NULL else df
  })
  names(tables) <- usage_types
  Filter(Negate(is.null), tables)
}

#' Save the per-usage df_long tables to a .qs file
#'
#' @param tables A named list of df_long tibbles.
#' @param path Character. Full output path for the .qs file.
#' @return The path, invisibly.
#' @export
zoning_save_tables <- function(tables, path) {
  qs::qsave(tables, path)
  message(sprintf("Saved %d usage tables to %s", length(tables), path))
  invisible(path)
}

#' Print a per-usage summary: DA count and total km2
#'
#' @param tables A named list of df_long tibbles.
#' @return The summary tibble, invisibly.
#' @export
zoning_summary <- function(tables) {
  summ <- tibble::tibble(
    usage     = names(tables),
    n_da      = vapply(tables, nrow, integer(1)),
    total_km2 = vapply(tables, function(df) sum(df$value) / 1e6, numeric(1))
  )
  print(summ, n = nrow(summ))
  invisible(summ)
}