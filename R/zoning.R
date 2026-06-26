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
  ## Workers are separate R processes and do not inherit sf_use_s2() from the
  ## main session. Force planar geometry here so st_union / st_intersection
  ## behave consistently (data is projected, EPSG:3347).
  suppressMessages(sf::sf_use_s2(FALSE))

  ## usage_classification can be either a list-column (vector of usages per
  ## zone) or, after the mirai-safe conversion below, a pipe-delimited string
  ## like "single_detached|duplex". List-columns can be mangled when
  ## serialized to a worker, so a flat string is used across the mirai
  ## boundary; handle both here.
  uc <- zones$usage_classification
  if (is.list(uc)) {
    keep <- vapply(uc, function(v) usage %in% v, logical(1))
  } else {
    keep <- grepl(paste0("(^|\\|)", usage, "($|\\|)"), uc)
  }
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

#' Collect mirai_map results with the native green ETA progress bar
#'
#' @param mp A mirai_map promise object.
#' @return The list of worker results.
#' @keywords internal
zoning_collect_results <- function(mp) {
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

  res
}

#' Collect mirai_map results with per-task text messages (CMHC-style)
#'
#' Polls the running tasks and prints one line each time a task finishes,
#' naming the (usage x CMA) pair and showing the running count and elapsed
#' time. Unlike the green `[.progress` bar, this reveals WHICH task is slow or
#' stuck — useful when the bar sits frozen at one percentage.
#'
#' @param mp A mirai_map object (from `zoning_launch_usages()`).
#' @param tasks The task data.frame (columns `usage`, `cma`).
#' @param poll Numeric seconds between status checks (default 0.5).
#' @return The list of worker results, in task order.
#' @export
zoning_collect_verbose <- function(mp, tasks, poll = 0.5) {
  n <- nrow(tasks)
  done <- rep(FALSE, n)
  t0 <- Sys.time()
  cat(sprintf("[zoning] %d tasks launched (usage x CMA)\n", n))

  repeat {
    ## mirai marks each element unresolved while still running
    unres <- mirai::unresolved(mp)
    finished_now <- which(!unres & !done)
    for (i in finished_now) {
      done[i] <- TRUE
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf(
        "  [%3d/%3d] done: %-18s CMA %-6s  (%.0fs elapsed)\n",
        sum(done), n, tasks$usage[i], tasks$cma[i], el
      ))
      utils::flush.console()
    }
    if (all(done)) break
    Sys.sleep(poll)
  }

  res <- mp[]  # collect (already resolved)

  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    stop(sprintf(
      "mirai_map had %d failure(s). First failure:\nindex=%d\n%s",
      length(idx), idx[1], as.character(res[[idx[1]]])
    ), call. = FALSE)
  }
  res
}

#' Compute per-DA land area for every usage (chunked by CMA)
#' Computes per-DA area for each usage in parallel. The work is split into one
#' task per (usage x CMA) pair — e.g. 12 usages x 11 CMAs = 132 small tasks.
#'
#' IMPORTANT: the native `[.progress` green ETA bar only renders when the
#' `mp[.progress]` call happens at the TOP LEVEL of an interactive script, not
#' when buried inside nested function calls. So this function is split in two:
#'   1. `zoning_launch_usages()` builds the tasks and returns the live
#'      `mirai_map` object `mp` (plus the task table).
#'   2. the caller runs `res <- mp[.progress]` at top level (bar shows here).
#'   3. `zoning_combine_usages(res, tasks, usages)` reassembles per-usage.
#' `zoning_area_all_usages()` is kept as a convenience wrapper for when the
#' progress bar is not needed.
#'
#' @param zones An `sf` object of zoning polygons. Must have a `cma_id` column.
#' @param da An `sf` object of dissemination areas with `id` and `cma_id`.
#' @param usages Character vector of usage types (default `ZONING_USAGE_TYPES`).
#'
#' @return A list with `mp` (mirai_map object), `tasks` (data.frame), and
#'   `usages`. Run `res <- mp[.progress]` then
#'   `zoning_combine_usages(res, tasks, usages)`.
#' @export
zoning_launch_usages <- function(zones, da, usages = ZONING_USAGE_TYPES) {
  if (!"cma_id" %in% names(zones) || !"cma_id" %in% names(da)) {
    stop("`zones` and `da` must both have a `cma_id` column (see zoning_attach_cma).")
  }

  ## Flatten the usage_classification list-column to a pipe-delimited string
  ## BEFORE sending to workers (list-columns can be mangled by serialization).
  if (is.list(zones$usage_classification)) {
    zones$usage_classification <- vapply(
      zones$usage_classification,
      function(v) paste(v, collapse = "|"),
      character(1)
    )
  }

  cmas <- sort(unique(as.character(da$cma_id)))
  tasks <- expand.grid(
    usage = usages,
    cma   = cmas,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  par_env <- zoning_make_par_env(
    zones = zones,
    da = da,
    zoning_area_for_usage = zoning_area_for_usage
  )

  .worker <- function(usage, cma) {
    z <- zones[as.character(zones$cma_id) == cma, ]
    d <- da[as.character(da$cma_id) == cma, ]
    if (nrow(z) == 0 || nrow(d) == 0) {
      return(tibble::tibble(id = character(0), value = numeric(0)))
    }
    zoning_area_for_usage(z, d, usage)
  }
  environment(.worker) <- par_env

  mp <- mirai::mirai_map(tasks, .worker)
  list(mp = mp, tasks = tasks, usages = usages)
}

#' Reassemble per-(usage x CMA) results into one tibble per usage
#'
#' @param res The collected result list (from `mp[.progress]`).
#' @param tasks The task data.frame returned by `zoning_launch_usages()`.
#' @param usages Character vector of usage names.
#' @return A named list of tibbles (`id`, `value`), one per usage.
#' @export
zoning_combine_usages <- function(res, tasks, usages) {
  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    stop(sprintf(
      "mirai_map had %d failure(s). First failure:\nindex=%d\n%s",
      length(idx), idx[1], as.character(res[[idx[1]]])
    ), call. = FALSE)
  }

  out <- stats::setNames(vector("list", length(usages)), usages)
  for (i in seq_len(nrow(tasks))) {
    u <- tasks$usage[i]
    out[[u]] <- dplyr::bind_rows(out[[u]], res[[i]])
  }
  out
}

#' Compute per-DA land area for every usage, in chunks (CMHC-style)
#'
#' Runs the (usage x CMA) tasks in chunks. Each chunk gets its own native green
#' `[.progress` bar with ETA, and a one-line summary is printed after each chunk
#' (how many done, elapsed time) — the same pattern as the CMHC pipeline. A
#' `gc()` runs between chunks to keep memory flat. Combines results per usage at
#' the end.
#'
#' Because the green bar only renders at top level, this function calls
#' `mp[.progress]` directly (it is itself meant to be called at the top level of
#' a script, which is the normal case).
#'
#' @param zones An `sf` object of zoning polygons (needs `cma_id`).
#' @param da An `sf` object of dissemination areas (needs `id`, `cma_id`).
#' @param usages Character vector of usage types (default `ZONING_USAGE_TYPES`).
#' @param chunk_size Integer. Tasks per chunk (default 48).
#' @return A named list of tibbles (`id`, `value`), one per usage.
#' @export
zoning_area_chunked <- function(zones, da, usages = ZONING_USAGE_TYPES,
                                chunk_size = 48L) {
  if (!"cma_id" %in% names(zones) || !"cma_id" %in% names(da)) {
    stop("`zones` and `da` must both have a `cma_id` column (see zoning_attach_cma).")
  }

  if (is.list(zones$usage_classification)) {
    zones$usage_classification <- vapply(
      zones$usage_classification,
      function(v) paste(v, collapse = "|"),
      character(1)
    )
  }

  cmas <- sort(unique(as.character(da$cma_id)))
  tasks <- expand.grid(
    usage = usages, cma = cmas,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  n_total <- nrow(tasks)
  n_chunks <- ceiling(n_total / chunk_size)

  par_env <- zoning_make_par_env(
    zones = zones, da = da,
    zoning_area_for_usage = zoning_area_for_usage
  )
  .worker <- function(usage, cma) {
    z <- zones[as.character(zones$cma_id) == cma, ]
    d <- da[as.character(da$cma_id) == cma, ]
    if (nrow(z) == 0 || nrow(d) == 0) {
      return(tibble::tibble(id = character(0), value = numeric(0)))
    }
    zoning_area_for_usage(z, d, usage)
  }
  environment(.worker) <- par_env

  out <- stats::setNames(vector("list", length(usages)), usages)
  t0 <- Sys.time()
  done <- 0L

  cat(sprintf("[zoning] %d tasks (%d usages x %d CMA) in %d chunks of %d\n",
              n_total, length(usages), length(cmas), n_chunks, chunk_size))

  for (ch in seq_len(n_chunks)) {
    lo <- (ch - 1L) * chunk_size + 1L
    hi <- min(ch * chunk_size, n_total)
    chunk_tasks <- tasks[lo:hi, , drop = FALSE]

    cat(sprintf("\n[zoning] chunk %d/%d  (tasks %d-%d)\n", ch, n_chunks, lo, hi))

    ## Green bar + ETA for THIS chunk (top-level [.progress)
    mp <- mirai::mirai_map(chunk_tasks, .worker)
    res <- mp[.progress]

    bad <- vapply(res, mirai::is_error_value, logical(1))
    if (any(bad)) {
      idx <- which(bad)
      stop(sprintf(
        "chunk %d: mirai_map had %d failure(s). First:\n%s",
        ch, length(idx), as.character(res[[idx[1]]])
      ), call. = FALSE)
    }

    ## Accumulate this chunk's results per usage
    for (k in seq_len(nrow(chunk_tasks))) {
      u <- chunk_tasks$usage[k]
      out[[u]] <- dplyr::bind_rows(out[[u]], res[[k]])
    }

    done <- done + nrow(chunk_tasks)
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("[zoning] chunk %d/%d done — %d/%d tasks total (%.0fs elapsed)\n",
                ch, n_chunks, done, n_total, el))

    rm(res, mp)
    gc()
  }

  cat(sprintf("\n[zoning] ALL DONE — %d tasks in %.0fs\n",
              n_total, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

#' Compute per-DA land area for every usage (wrapper, no live progress bar)
#'
#' Convenience wrapper around `zoning_launch_usages()` +
#' `zoning_combine_usages()`. The green ETA bar will NOT show through this
#' wrapper because `[.progress` is not at top level — use the two-step form in
#' a script when you want the bar.
#'
#' @inheritParams zoning_launch_usages
#' @return A named list of tibbles (`id`, `value`), one per usage.
#' @export
zoning_area_all_usages <- function(zones, da, usages = ZONING_USAGE_TYPES) {
  launched <- zoning_launch_usages(zones, da, usages)
  res <- launched$mp[.progress]
  zoning_combine_usages(res, launched$tasks, launched$usages)
}

#' Attach the parent CMA id to zones and DAs
#'
#' Adds a `cma_id` column to both the zones and the DA `sf` objects, using the
#' DA-universe (cancensus) CSD/CMA hierarchy. DAs are matched on `id`; zones on
#' `csd_id`. This is what lets `zoning_area_all_usages()` chunk by CMA without
#' any spatial join.
#'
#' @param zones An `sf` object with a `csd_id` column.
#' @param da An `sf` object with an `id` column.
#' @param cma_ids Character vector of in-scope CMA UIDs.
#' @param dataset Character. cancensus dataset code (default "CA21").
#' @return A list with `zones` and `da`, each gaining a `cma_id` column.
#' @export
zoning_attach_cma <- function(zones, da, cma_ids, dataset = "CA21") {
  hier <- cancensus::get_census(
    dataset = dataset,
    regions = list(CMA = cma_ids),
    level = "DA",
    geo_format = NA,
    use_cache = TRUE,
    quiet = TRUE
  )
  da_cma  <- stats::setNames(as.character(hier$CMA_UID), as.character(hier$GeoUID))
  csd_cma <- tapply(
    as.character(hier$CMA_UID),
    as.character(hier$CSD_UID),
    function(x) x[1]
  )

  da$cma_id    <- unname(da_cma[as.character(da$id)])
  zones$cma_id <- unname(csd_cma[as.character(zones$csd_id)])

  list(zones = zones, da = da)
}

#' Build the DA universe for the in-scope CMAs
#'
#' Returns every dissemination area in the given CMAs, with its parent CSD,
#' via the cancensus hierarchy (no spatial join). This is the universe over
#' which results are completed: DAs whose CSD was collected get 0 for absent
#' usages, DAs whose CSD was not collected get NA.
#'
#' @param cma_ids Character vector of CMA UIDs (the zoning scope).
#' @param dataset Character. cancensus dataset code (default "CA21").
#'
#' @return A tibble with columns `id` (DAUID) and `csd_id`.
#' @export
zoning_da_universe <- function(cma_ids, dataset = "CA21") {
  da_hier <- cancensus::get_census(
    dataset = dataset,
    regions = list(CMA = cma_ids),
    level = "DA",
    geo_format = NA,
    use_cache = TRUE,
    quiet = TRUE
  )
  tibble::tibble(
    id     = as.character(da_hier$GeoUID),
    csd_id = as.character(da_hier$CSD_UID)
  )
}

#' Complete usage results with 0 (collected) or NA (not collected)
#'
#' For each usage, expands results to the full DA universe of the in-scope
#' CMAs. A DA whose CSD appears in the collected zoning (so the city was
#' processed) gets 0 when the usage is absent — the absence is real. A DA
#' whose CSD was not collected gets NA — the data is missing. DAs outside the
#' universe (outside the scope CMAs) are not included.
#'
#' @param areas A named list of tibbles (`id`, `value`), one per usage, as
#'   returned by `zoning_area_all_usages()`.
#' @param da_universe A tibble (`id`, `csd_id`) from `zoning_da_universe()`.
#' @param collected_csds Character vector of CSD UIDs present in the zoning
#'   data (i.e. `unique(zones$csd_id)`).
#'
#' @return A named list of tibbles (`id`, `value`), one per usage, completed
#'   over the universe with 0 / NA.
#' @export
zoning_complete_universe <- function(areas, da_universe, collected_csds) {
  da_universe$covered <- da_universe$csd_id %in% as.character(collected_csds)

  lapply(areas, function(tbl) {
    if (!is.data.frame(tbl)) {
      tbl <- tibble::tibble(id = character(0), value = numeric(0))
    }
    da_universe |>
      dplyr::left_join(dplyr::select(tbl, id, value), by = "id") |>
      dplyr::mutate(
        ## CSD membership (from cancensus) is the source of truth, not geometry.
        ## A DA whose CSD was NOT collected is NA even if a neighbouring
        ## collected city's zone spills a sliver across the boundary onto it.
        ## So test !covered FIRST, before keeping any computed value.
        value = dplyr::case_when(
          !covered      ~ NA_real_,   # CSD not collected -> no data, full stop
          !is.na(value) ~ value,      # collected + has area -> keep
          TRUE          ~ 0           # collected + usage absent -> real 0
        )
      ) |>
      dplyr::select(id, value)
  })
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