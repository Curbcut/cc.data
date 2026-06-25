#' Residential usage types carried by the zoning dataset
#'
#' The 12 residential usage categories encoded in the `usage_classification`
#' list-column of the zoning GeoJSON. Each zone permits one or more of these.
#'
#' @keywords internal
ZONING_USAGE_TYPES <- c(
  "single_detached", "semi_detached", "duplex", "triplex",
  "townhouse", "row_house", "mid_rise", "high_rise",
  "collective", "mobile_home", "accessory", "other"
)

#' Read the zoning polygons
#'
#' Reads the zoning GeoJSON, repairs geometries, reprojects to the working CRS,
#' and keeps only the most recent year of each zone. The GeoJSON stores one row
#' per (zone, year); since a zone's permitted usages change over time, only the
#' latest year is kept so the result reflects current zoning rather than a mix
#' of historical states. A zone is identified by `csd_id` + `zone_code`.
#'
#' @param path Character. Path to the zoning GeoJSON file.
#' @param proj_crs Integer. Target projected CRS (default 3347, Canada Lambert).
#'
#' @return An `sf` object of zoning polygons in `proj_crs`, one row per zone.
#' @export
zoning_read_zones <- function(path, proj_crs = 3347L) {
  zones <- sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(proj_crs)
  
  ## Keep only the most recent year per zone (csd_id + zone_code).
  zones |>
    dplyr::group_by(csd_id, zone_code) |>
    dplyr::filter(year == max(year)) |>
    dplyr::ungroup()
}

#' Read dissemination-area geometries
#'
#' Fetches dissemination-area boundaries for a given census year via
#' `cc.data::census_empty_geometries()`, repairs geometries, and reprojects to
#' the working CRS.
#'
#' @param proj_crs Integer. Target projected CRS (default 3347).
#' @param census_year Integer. Census year (default 2021).
#'
#' @return An `sf` object of dissemination areas with an `id` column.
#' @export
zoning_read_da <- function(proj_crs = 3347L, census_year = 2021L) {
  geos <- cc.data::census_empty_geometries(
    census_scales = "DA",
    census_years = census_year
  )
  da <- geos[["DA"]][[as.character(census_year)]]
  names(da)[names(da) == "ID"] <- "id"
  da$id <- as.character(da$id)
  ## census_empty_geometries already returns valid geometries in EPSG:3347,
  ## so no st_make_valid / st_transform is needed here.
  da
}

#' Compute per-DA land area for a single usage
#'
#' Zones permitting `usage` are dissolved into one geometry (`st_union`) BEFORE
#' intersecting with the dissemination areas. Dissolving first is essential:
#' when two zones permitting the same usage overlap, summing per-zone
#' intersections counts the shared ground more than once. The union collapses
#' overlaps so each parcel of ground is counted exactly once per usage.
#'
#' @param zones An `sf` object of zoning polygons (with `usage_classification`).
#' @param da An `sf` object of dissemination areas with an `id` column.
#' @param usage Character. The usage type to compute.
#'
#' @return A tibble with columns `id` and `value` (land area in m2). Returns an
#'   empty tibble when no zone permits the usage or no DA intersects it.
#' @keywords internal
zoning_area_for_usage <- function(zones, da, usage) {
  ## usage_classification is a list-column: each zone holds a vector of the
  ## usages it permits. Keep zones whose vector contains `usage`. Inlined here
  ## (rather than calling a separate helper) so the function is self-contained
  ## when shipped to a mirai worker.
  keep <- vapply(
    zones$usage_classification,
    function(v) usage %in% v,
    logical(1)
  )
  z <- zones[keep, ]
  
  if (nrow(z) == 0) {
    return(tibble::tibble(id = character(0), value = numeric(0)))
  }
  
  ## mirai serialization can leave the sf_column attribute pointing to a
  ## column that is no longer a live sfc after subsetting. Rather than rely
  ## on st_geometry (which reads that attribute), grab the geometry column
  ## directly by detecting the sfc column, and union that.
  geom_col <- names(z)[vapply(z, function(col) inherits(col, "sfc"), logical(1))]
  if (length(geom_col) == 0) {
    return(tibble::tibble(id = character(0), value = numeric(0)))
  }
  zgeom <- z[[geom_col[1]]]
  
  dissolved <- sf::st_union(zgeom)
  dissolved <- sf::st_make_valid(dissolved)
  
  ## Same precaution for `da`: re-assert its geometry column so st_filter /
  ## st_intersection work after serialization.
  da_geom_col <- names(da)[vapply(da, function(col) inherits(col, "sfc"), logical(1))]
  if (length(da_geom_col) > 0) {
    da <- sf::st_set_geometry(da, da_geom_col[1])
  }
  
  da_hit <- sf::st_filter(da, dissolved)
  if (nrow(da_hit) == 0) {
    return(tibble::tibble(id = character(0), value = numeric(0)))
  }
  
  pieces <- suppressWarnings(sf::st_intersection(da_hit, dissolved))
  if (nrow(pieces) == 0) {
    return(tibble::tibble(id = character(0), value = numeric(0)))
  }
  
  pieces$area_m2 <- as.numeric(sf::st_area(pieces))
  
  tibble::as_tibble(sf::st_drop_geometry(pieces)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(value = sum(area_m2, na.rm = TRUE), .groups = "drop")
}

#' Build a self-contained environment for mirai daemon serialization
#'
#' When a closure is serialized for mirai, package-namespace functions resolve
#' to the INSTALLED version on the daemon, not the `load_all()` dev version.
#' This helper creates an environment holding all needed objects and rebinds
#' every function's enclosing environment so internal references resolve from
#' the environment rather than the installed namespace.
#'
#' @param ... Named objects (functions, data) the worker needs.
#' @return An environment with parent `globalenv()`.
#' @keywords internal
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

#' Collect mirai_map results with a progress bar and error check
#'
#' Drives the `[.progress` ETA progress bar, then verifies no worker returned
#' an error value before returning the named list of result tibbles.
#'
#' @param mp A mirai_map promise object.
#' @param usages Character vector of usage names, used to name the results.
#' @return A named list of tibbles (`id`, `value`), one per usage.
#' @keywords internal
zoning_collect_results <- function(mp, usages) {
  res <- mp[.progress]
  
  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    stop(
      sprintf(
        "mirai_map had %d failure(s). First failure:\nindex=%d\n%s",
        length(idx), idx[1], as.character(res[[idx[1]]])
      ),
      call. = FALSE
    )
  }
  
  stats::setNames(res, usages)
}

#' Compute per-DA land area for every usage
#'
#' Computes per-DA area for each usage in parallel, one mirai task per usage,
#' with a native ETA progress bar. Requires active mirai daemons (set with
#' `mirai::daemons()` beforehand).
#'
#' @param zones An `sf` object of zoning polygons (with `usage_classification`).
#' @param da An `sf` object of dissemination areas with an `id` column.
#' @param usages Character vector of usage types (default `ZONING_USAGE_TYPES`).
#'
#' @return A named list of tibbles (`id`, `value`), one per usage.
#' @export
zoning_area_all_usages <- function(zones, da, usages = ZONING_USAGE_TYPES) {
  ## Build a self-contained env so the worker resolves zoning_area_for_usage
  ## (and its dependencies) from the env rather than the installed namespace.
  par_env <- zoning_make_par_env(
    zones = zones,
    da = da,
    zoning_area_for_usage = zoning_area_for_usage
  )
  
  .worker <- function(usage) {
    zoning_area_for_usage(zones, da, usage)
  }
  environment(.worker) <- par_env
  
  mp <- mirai::mirai_map(usages, .worker)
  zoning_collect_results(mp, usages)
}

#' Reshape usage areas into long-format upload tables
#'
#' Converts the named list of per-usage area tibbles into the long format
#' expected by the upload pipeline: one table per usage with columns `id`,
#' `time`, `geo_vintage`, and `value`.
#'
#' @param areas A named list of tibbles (`id`, `value`), one per usage.
#' @param time Character. The time period label (default "2026").
#' @param geo_vintage Character. The census boundary vintage (default "2021").
#'
#' @return A named list of long-format tibbles, one per usage.
#' @export
zoning_to_df_long <- function(areas, time = "2026", geo_vintage = "2021") {
  lapply(areas, function(tbl) {
    if (!is.data.frame(tbl) || nrow(tbl) == 0) {
      return(tibble::tibble(
        id = character(0), time = character(0),
        geo_vintage = character(0), value = numeric(0)
      ))
    }
    tbl |>
      dplyr::mutate(time = time, geo_vintage = geo_vintage) |>
      dplyr::select(id, time, geo_vintage, value)
  })
}

#' Save usage tables to a qs file
#'
#' @param tables A named list of long-format tibbles.
#' @param path Character. Output path for the `.qs` file.
#'
#' @return The output path, invisibly.
#' @export
zoning_save_tables <- function(tables, path) {
  qs::qsave(tables, path)
  invisible(path)
}

#' Print a per-usage summary
#'
#' Prints one line per usage: the number of DAs and the total land area.
#'
#' @param tables A named list of long-format tibbles.
#'
#' @return `tables`, invisibly.
#' @export
zoning_summary <- function(tables) {
  for (usage in names(tables)) {
    tbl <- tables[[usage]]
    message(sprintf(
      "%-18s %6d DAs   total = %.0f m2",
      usage, nrow(tbl), sum(tbl$value, na.rm = TRUE)
    ))
  }
  invisible(tables)
}