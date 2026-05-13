## ============================================================================
##  osm.R — OpenStreetMap POI extraction (Geofabrik)
##
##  Public functions:
##    osm_categories_default()   — default per-category extraction config
##    osm_ensure_pbf()           — download Geofabrik .osm.pbf for a province
##    osm_ensure_gpkg()          — convert .osm.pbf to .gpkg (cached)
##    osm_fetch_raw()            — read all matching features (4 layers)
##    osm_strategy_polys_only()  — keep polygons only
##    osm_strategy_pt_in_poly()  — drop point-in-polygon duplicates
##    osm_to_centroid()          — collapse polygons to centroids
##    osm_extract_province()     — full extraction for one province
##    osm_extract_canada()       — parallel extraction for all provinces
##    osm_combine_provinces()    — combine per-province files into one per category
## ============================================================================


#' Canadian provinces (Geofabrik slugs)
#'
#' The 13 provinces/territories as named on the Geofabrik download server,
#' lowercase with hyphens.
#'
#' @return A character vector.
#' @export
osm_provinces <- function() {
  c(
    "alberta", "british-columbia", "manitoba", "new-brunswick",
    "newfoundland-and-labrador", "northwest-territories", "nova-scotia",
    "nunavut", "ontario", "prince-edward-island", "quebec",
    "saskatchewan", "yukon"
  )
}


#' Default OSM category configuration
#'
#' Returns the list of categories used by [osm_extract_canada()]. Each entry
#' is a list with:
#' \describe{
#'   \item{type_id}{character — category id, used in filenames and the
#'                  returned data}
#'   \item{where}{character — OGR SQL `WHERE` clause filtering features}
#'   \item{tags}{character vector — extra OSM tags to retain in the .gpkg}
#'   \item{strategy}{`"polys_only"` or `"pt_in_poly"` — how to deduplicate
#'                   when the same feature appears as both a point and a
#'                   polygon}
#' }
#'
#' Users may override entries or extend the list to add their own categories.
#'
#' @return A list of category specifications.
#' @export
osm_categories_default <- function() {
  list(
    list(type_id  = "school",
         where    = "amenity = 'school'",
         tags     = c("amenity", "operator", "operator:type"),
         strategy = "polys_only"),

    list(type_id  = "fire_station",
         where    = "amenity = 'fire_station'",
         tags     = c("amenity", "operator"),
         strategy = "pt_in_poly")
    ## NOTE: extend this list with your full Curbcut categories.
  )
}


#' Default OSM extra tags
#'
#' The union of tags requested across all default categories — passed to
#' `osmextract::oe_vectortranslate()` so each layer's GPKG carries the
#' columns required by any subsequent category query.
#'
#' @return A character vector of OSM tags.
#' @export
osm_extra_tags_default <- function() {
  c("amenity", "leisure", "tourism", "sport",
    "operator", "operator:type", "access", "healthcare")
}


#' Download a Geofabrik .osm.pbf file
#'
#' Downloads the latest Geofabrik extract for one Canadian province if the
#' local copy is missing or implausibly small (< 5 MB).
#'
#' @param province Province slug (lowercase, hyphenated) or a province name
#'   with spaces — both are normalized.
#' @param out_dir Directory where the `.osm.pbf` file should live.
#'
#' @return The local file path (character).
#' @export
osm_ensure_pbf <- function(province, out_dir) {
  slug     <- gsub(" ", "-", tolower(province))
  pbf_url  <- sprintf(
    "https://download.geofabrik.de/north-america/canada/%s-latest.osm.pbf",
    slug
  )
  pbf_path <- file.path(out_dir, sprintf("geofabrik_%s-latest.osm.pbf", slug))

  if (!file.exists(pbf_path) || file.info(pbf_path)$size < 5e6) {
    cmd <- sprintf(
      'curl -L --retry 5 --retry-delay 10 -o "%s" "%s"',
      pbf_path, pbf_url
    )
    system(cmd)
  }
  pbf_path
}


#' Convert .osm.pbf to .gpkg (cached)
#'
#' Vector-translates the 4 OSM layers (`points`, `lines`, `multipolygons`,
#' `other_relations`) from a Geofabrik `.osm.pbf` into a GeoPackage, retaining
#' `extra_tags` as columns. Skips translation if the resulting `.gpkg`
#' already contains all 4 layers.
#'
#' @param pbf_path Path to a `.osm.pbf` file (see [osm_ensure_pbf()]).
#' @param extra_tags Character vector of OSM tags to retain as GPKG columns.
#'   Defaults to [osm_extra_tags_default()].
#'
#' @return Path to the `.gpkg`.
#' @export
osm_ensure_gpkg <- function(pbf_path, extra_tags = osm_extra_tags_default()) {
  gpkg_path <- sub("\\.osm\\.pbf$", ".gpkg", pbf_path)
  layers <- if (file.exists(gpkg_path)) sf::st_layers(gpkg_path)$name else character(0)

  if (length(layers) < 4) {
    for (lyr in c("points", "lines", "multipolygons", "other_relations")) {
      osmextract::oe_vectortranslate(
        pbf_path,
        layer                = lyr,
        extra_tags           = extra_tags,
        force_vectortranslate = TRUE,
        quiet                = TRUE
      )
    }
  }
  gpkg_path
}


#' Read one layer from a GPKG with an OGR SQL filter
#'
#' Internal helper — returns `NULL` on read failure or empty result.
#'
#' @param gpkg_path Path to a `.gpkg`.
#' @param layer Layer name (one of `points`, `lines`, `multipolygons`,
#'   `other_relations`).
#' @param where_sql OGR SQL `WHERE` clause (without the keyword).
#' @param extra_tags Character vector of tags to read.
#'
#' @return An `sf` object with an added `layer_src` column, or `NULL`.
#' @keywords internal
osm_read_layer <- function(gpkg_path, layer, where_sql, extra_tags) {
  q <- sprintf("SELECT * FROM %s WHERE %s", layer, where_sql)
  out <- tryCatch(
    osmextract::oe_read(
      gpkg_path,
      layer      = layer,
      extra_tags = extra_tags,
      query      = q,
      quiet      = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out$layer_src <- layer
  out
}


#' Fetch all OSM features matching a WHERE clause
#'
#' Reads `points`, `lines`, `multipolygons`, and `other_relations` from a
#' GPKG, intersects them on common columns, and stacks them into a single
#' `sf` data frame.
#'
#' @param gpkg_path Path to a `.gpkg`.
#' @param where_sql OGR SQL `WHERE` clause (without the keyword).
#' @param extra_tags Tags requested at translation time.
#'
#' @return An `sf` object combining all 4 layers, with a `layer_src` column.
#'   `NULL` if no features matched.
#' @export
osm_fetch_raw <- function(gpkg_path, where_sql, extra_tags) {
  parts <- lapply(
    c("points", "lines", "multipolygons", "other_relations"),
    function(l) osm_read_layer(gpkg_path, l, where_sql, extra_tags)
  )
  parts <- parts[!sapply(parts, is.null)]
  if (length(parts) == 0) return(NULL)

  common <- Reduce(intersect, lapply(parts, names))
  parts  <- lapply(parts, function(x) x[, common])
  do.call(rbind, parts)
}


#' Strategy: keep polygons only
#'
#' For categories where OSM tagging is reliably polygonal (e.g. schools,
#' parks), drops the point/line duplicates entirely.
#'
#' @param d An `sf` data frame with a `layer_src` column from [osm_fetch_raw()].
#' @return A filtered `sf` data frame.
#' @export
osm_strategy_polys_only <- function(d) {
  d[d$layer_src %in% c("multipolygons", "other_relations"), ]
}


#' Strategy: point-in-polygon deduplication
#'
#' Keeps all polygons; drops points that fall inside one of those polygons
#' (because they refer to the same feature, tagged twice).
#'
#' @param d An `sf` data frame with a `layer_src` column from [osm_fetch_raw()].
#' @return A filtered `sf` data frame.
#' @export
osm_strategy_pt_in_poly <- function(d) {
  is_poly <- d$layer_src %in% c("multipolygons", "other_relations")
  is_pt   <- d$layer_src %in% c("points", "lines")
  if (sum(is_poly) == 0) return(d[is_pt, ])
  if (sum(is_pt)   == 0) return(d[is_poly, ])

  polys <- d[is_poly, ]
  pts   <- d[is_pt, ]
  sf::sf_use_s2(FALSE)
  inside <- suppressWarnings(sf::st_within(pts, polys, sparse = TRUE))
  sf::sf_use_s2(TRUE)
  rbind(polys, pts[lengths(inside) == 0, ])
}


#' Collapse polygons to centroids
#'
#' Ensures the resulting `sf` is pure POINT geometry (so it round-trips through
#' `upload_destination_pois()` and joins cleanly with other POI sources).
#'
#' @param x An `sf` data frame.
#' @return An `sf` data frame with POINT geometry.
#' @export
osm_to_centroid <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(x)
  if (any(sf::st_geometry_type(x) != "POINT")) {
    sf::sf_use_s2(FALSE)
    x <- suppressWarnings(sf::st_centroid(x))
    sf::sf_use_s2(TRUE)
  }
  x
}


#' Extract all categories for one province
#'
#' Downloads the Geofabrik extract, ensures the GPKG cache, and runs each
#' category in `categories`. One `.qs` file is written per (category, province)
#' in `out_dir`, with filename pattern `<type_id>_<province-slug>.qs`.
#'
#' @param province Province slug or full name.
#' @param out_dir Output directory.
#' @param categories A list of category specs (see [osm_categories_default()]).
#' @param extra_tags Tags to retain in the GPKG (must be a superset of those
#'   referenced by any category). Defaults to [osm_extra_tags_default()].
#'
#' @return A named integer vector: per-category POI counts.
#' @export
osm_extract_province <- function(province,
                                 out_dir,
                                 categories = osm_categories_default(),
                                 extra_tags = osm_extra_tags_default()) {
  slug      <- gsub(" ", "-", tolower(province))
  pbf_path  <- osm_ensure_pbf(province, out_dir)
  gpkg_path <- osm_ensure_gpkg(pbf_path, extra_tags)

  strategies <- list(
    polys_only = osm_strategy_polys_only,
    pt_in_poly = osm_strategy_pt_in_poly
  )

  totals <- integer(length(categories))
  names(totals) <- vapply(categories, `[[`, character(1), "type_id")

  for (cat in categories) {
    raw <- osm_fetch_raw(gpkg_path, cat$where, cat$tags)
    if (is.null(raw)) {
      totals[cat$type_id] <- 0L
      next
    }
    deduped <- strategies[[cat$strategy]](raw)
    result  <- osm_to_centroid(deduped)
    result$category <- cat$type_id
    result$province <- province
    qs::qsave(result, file.path(out_dir, sprintf("%s_%s.qs", cat$type_id, slug)))
    totals[cat$type_id] <- nrow(result)
  }
  totals
}


#' Extract OSM POIs for all of Canada (parallel)
#'
#' Runs [osm_extract_province()] across all provinces. Uses `mirai` daemons
#' when `n_workers > 1`; serial otherwise. Output is one `.qs` per
#' (category, province) under `out_dir`.
#'
#' @param out_dir Output directory.
#' @param provinces Character vector of provinces. Defaults to all 13 via
#'   [osm_provinces()].
#' @param categories List of category specs (see [osm_categories_default()]).
#' @param extra_tags Extra OSM tags to retain.
#' @param n_workers Number of parallel workers. Set to 1 to run serially.
#'
#' @return A data frame: one row per province, with category counts and a
#'   `total` column.
#' @export
osm_extract_canada <- function(out_dir,
                               provinces  = osm_provinces(),
                               categories = osm_categories_default(),
                               extra_tags = osm_extra_tags_default(),
                               n_workers  = 4) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  options(timeout = 7200)

  results <- if (n_workers <= 1) {
    lapply(provinces, function(p) {
      tot <- osm_extract_province(p, out_dir, categories, extra_tags)
      list(province = p, totals = tot)
    })
  } else {
    if (!requireNamespace("mirai", quietly = TRUE)) {
      stop("Package 'mirai' is required when n_workers > 1.")
    }

    mirai::daemons(0)
    mirai::daemons(n_workers)
    on.exit(mirai::daemons(0), add = TRUE)

    ## Build the worker environment so each daemon can see the helpers.
    par_env <- new.env(parent = globalenv())
    helpers <- c("osm_ensure_pbf", "osm_ensure_gpkg", "osm_read_layer",
                 "osm_fetch_raw", "osm_strategy_polys_only",
                 "osm_strategy_pt_in_poly", "osm_to_centroid",
                 "osm_extract_province")
    for (h in helpers) {
      assign(h, get(h), envir = par_env)
      environment(par_env[[h]]) <- par_env
    }
    assign("categories", categories, envir = par_env)
    assign("extra_tags", extra_tags, envir = par_env)
    assign("out_dir",    out_dir,    envir = par_env)

    worker <- function(province) {
      list(
        province = province,
        totals   = osm_extract_province(province, out_dir, categories, extra_tags)
      )
    }
    environment(worker) <- par_env

    tasks <- data.frame(province = provinces, stringsAsFactors = FALSE)
    mp <- mirai::mirai_map(tasks, worker)
    mp[mirai::.progress]
  }

  ok <- !sapply(results, function(r) is.null(r) ||
                  (requireNamespace("mirai", quietly = TRUE) &&
                     mirai::is_error_value(r)))
  results <- results[ok]

  do.call(rbind, lapply(results, function(r) {
    data.frame(
      province = r$province,
      t(r$totals),
      total    = sum(r$totals),
      check.names = FALSE
    )
  }))
}


#' Combine per-province OSM extracts into per-category files
#'
#' After [osm_extract_canada()], `osm_dir` contains one `.qs` per
#' (category, province) — e.g. ~154 files for 13 provinces × 12 categories.
#' This function rebinds them into one `.qs` per category, normalizing each
#' to `(name, geometry)`, so the destination integration script can read
#' them the same way Foursquare files are read.
#'
#' @param osm_dir Directory containing the per-(category, province) `.qs`
#'   files written by [osm_extract_canada()].
#' @param out_dir Directory where the combined per-category `.qs` files
#'   should be written. Created if it doesn't exist.
#' @param provinces Province slugs used to strip suffixes. Defaults to
#'   [osm_provinces()].
#'
#' @return A named integer vector of POI counts per category (invisibly).
#' @export
osm_combine_provinces <- function(osm_dir,
                                  out_dir,
                                  provinces = osm_provinces()) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  files  <- list.files(osm_dir, pattern = "\\.qs$", full.names = TRUE)
  fnames <- tools::file_path_sans_ext(basename(files))

  strip_province <- function(fname) {
    for (p in provinces) {
      suf <- paste0("_", p)
      if (endsWith(fname, suf)) {
        return(substr(fname, 1, nchar(fname) - nchar(suf)))
      }
    }
    fname
  }
  categories <- vapply(fnames, strip_province, character(1), USE.NAMES = FALSE)

  message(sprintf("Found %d files across %d categories",
                  length(files), length(unique(categories))))

  normalize_one <- function(file_path) {
    df <- qs::qread(file_path)

    if (!"name" %in% names(df)) {
      alt <- intersect(c("osm_name", "Name", "NAME"), names(df))
      df$name <- if (length(alt) > 0) df[[alt[[1]]]] else NA_character_
    }

    if (!inherits(df, "sf")) df <- sf::st_as_sf(df)
    geom_col <- attr(df, "sf_column")
    df <- df[, c("name", geom_col)]
    if (geom_col != "geometry") df <- sf::st_set_geometry(df, "geometry")
    df
  }

  counts <- integer(length(unique(categories)))
  names(counts) <- unique(categories)

  for (cat in unique(categories)) {
    matching_idx <- which(categories == cat)
    message(sprintf("--- %s (%d province files) ---", cat, length(matching_idx)))

    parts    <- lapply(files[matching_idx], normalize_one)
    combined <- do.call(rbind, parts)

    out_path <- file.path(out_dir, paste0(cat, ".qs"))
    qs::qsave(combined, out_path)

    counts[cat] <- nrow(combined)
    message(sprintf("  Saved %s (%s POIs)",
                    out_path, format(nrow(combined), big.mark = ",")))

    rm(parts, combined); invisible(gc())
  }

  invisible(counts)
}