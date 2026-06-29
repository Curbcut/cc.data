# R/cmhc_update.R
#
# Incremental monthly update for the CMHC Starts and Completions Survey (Scss).
#
# Companion to cmhc_data.R: where cmhc_data.R holds the low-level fetch, clean
# and reshape helpers, this module orchestrates the *incremental* refresh. It
# pulls only the periods missing from the database, for every CMHC Scss variable
# at every geographic scale (CMA, CSD, CT, Survey Zone, Neighbourhood), and
# returns upload-ready long frames. It performs NO upload and creates NO
# variables, dimensions or tables; those must already exist.
#
# Everything that is identical across scales (period math, DB-state lookups, gap
# window, planning, running, orchestration) is written once. The only per-scale
# differences live in `cmhc_levels()`:
#   - geo_level_id : dictionary geo level used for DB-state lookups
#   - geo_vintage  : vintage tag written on fetched rows (matches the DB)
#   - unit         : "self"   -> one task per geo (CMA/CSD), fetched by year
#                    "parent" -> one task per CMA parent (CT/ZONE/NBHD), by
#                                year [x month], returning all children
#   - uids         : function(con) -> the geo (or parent) UIDs to iterate
#   - needs        : shared references to build once (cma_to_met, zones_lookup,
#                    nbhd_matching) and inject into the worker
#   - build_worker : function(spec, refs) -> list(worker, par_env_objs)
#
# Typical workflow (the whole public API):
#     con  <- cc.pipe::db_connect()
#     plan <- cmhc_refresh(con, "csd", dry_run = TRUE)$plan   # one scale, plan
#     mirai::daemons(18)
#     out  <- cmhc_refresh(con, "csd")                        # fetch one scale
#     all  <- cmhc_refresh_all(con)                           # fetch all scales
#     mirai::daemons(0)
#
# Daemons MUST be running for a real fetch: the cc.data engine only ships the
# worker's helper functions to workers in parallel mode.
#
# geo_vintage note (matches the DB): CMA/CSD = "2021", ZONE/NBHD = "2026",
# CT = "2021" for this incremental use (only the current year is fetched, and
# 2021+ maps to "2021", the latest CT vintage in the DB). Older CT vintages
# (2006/2011/2016) only matter for historical backfill, which this updater does
# not perform.
#
# Dependencies: cc.data (engine + fetch/clean/reshape/matching + geo uids),
#   cmhc, DBI, cc.pipe. ZONE/NBHD also need the .qsm bundles under
#   CURBCUT_DATA_SHARING_PATH and (NBHD) the cmhc_nbhd_matching dataset.

#' Null-or-NA coalescing operator
#'
#' Returns `b` when `a` is `NULL` or a length-one `NA`, otherwise `a`. Used to
#' supply defaults throughout the updater.
#'
#' @param a Value to test.
#' @param b Fallback returned when `a` is `NULL` or `NA`.
#' @return `a` if it is neither `NULL` nor a scalar `NA`; otherwise `b`.
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a


# =============================================================================
# 1. Period helpers (pure) — written once, used by every scale
# =============================================================================

#' Normalise a CMHC period label to a canonical form
#'
#' Accepts the varied period strings CMHC returns (a 6-digit `YYYYMM`, a 4-digit
#' `YYYY`, a `"YYYY Mon"` / `"Mon YYYY"` label, or an ISO date) and coerces them
#' to either `"YYYYMM"` (monthly) or `"YYYY"` (annual).
#'
#' @param x Character scalar. A single raw period label.
#' @return A normalised `"YYYYMM"` or `"YYYY"` string, or `NA_character_` if the
#'   input cannot be parsed.
#' @noRd
.cmhc_norm_period <- function(x) {
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NA_character_)
  if (grepl("^[0-9]{6}$", x)) return(x)
  if (grepl("^[0-9]{4}$", x)) return(x)
  month_lab <- NULL; year_lab <- NULL
  if (grepl("^[0-9]{4}[ -][A-Za-z]+$", x)) {
    p <- strsplit(x, "[ -]")[[1]]; year_lab <- p[1]; month_lab <- p[2]
  } else if (grepl("^[A-Za-z]+[ -][0-9]{4}$", x)) {
    p <- strsplit(x, "[ -]")[[1]]; month_lab <- p[1]; year_lab <- p[2]
  }
  if (!is.null(month_lab)) {
    m <- match(tolower(substr(month_lab, 1, 3)), tolower(month.abb))
    if (!is.na(m)) return(sprintf("%s%02d", year_lab, m))
  }
  iso <- tryCatch(as.Date(x), error = function(e) NA)
  if (!is.na(iso)) return(format(iso, "%Y%m"))
  NA_character_
}

#' Convert a `YYYYMM` label to a monotone month index
#'
#' @param ym Character scalar in `"YYYYMM"` form.
#' @return Integer month index (`year * 12 + month - 1`), suitable for ranges.
#' @noRd
.cmhc_ym_to_index <- function(ym) as.integer(substr(ym, 1, 4)) * 12L + (as.integer(substr(ym, 5, 6)) - 1L)

#' Convert a monotone month index back to a `YYYYMM` label
#'
#' @param idx Integer month index as produced by [.cmhc_ym_to_index()].
#' @return Character scalar in `"YYYYMM"` form.
#' @noRd
.cmhc_index_to_ym <- function(idx) sprintf("%04d%02d", idx %/% 12L, idx %% 12L + 1L)

#' Enumerate every month between two `YYYYMM` bounds (inclusive)
#'
#' @param start_ym,end_ym Character scalars in `"YYYYMM"` form.
#' @return Character vector of `"YYYYMM"` labels from `start_ym` to `end_ym`, or
#'   an empty vector when `start_ym` is after `end_ym`.
#' @noRd
.cmhc_enum_months <- function(start_ym, end_ym) {
  if (.cmhc_ym_to_index(start_ym) > .cmhc_ym_to_index(end_ym)) return(character(0))
  vapply(.cmhc_ym_to_index(start_ym):.cmhc_ym_to_index(end_ym), .cmhc_index_to_ym, character(1))
}


# =============================================================================
# 2. Registry of CMHC requests — same 8 requests at every scale
# =============================================================================

#' Registry of CMHC Scss requests refreshed at every scale
#'
#' Returns the eight distinct CMHC Scss requests this updater keeps current.
#' Each entry pairs a dictionary `variable_id` with the survey/series/dimension
#' triple needed to query the CMHC API, and the frequencies to refresh.
#'
#' @return A list of request specs. Each spec is a list with `variable_id`,
#'   `survey`, `series`, `dimension`, and `frequencies` (annual and monthly).
#' @export
cmhc_registry <- function() {
  spec <- function(variable_id, series, dimension) {
    list(variable_id = variable_id, survey = "Scss",
         series = series, dimension = dimension,
         frequencies = c("annual", "monthly"))
  }
  list(
    spec("starts_dwelling_type_all", "Starts", "Dwelling Type"),
    spec("starts_intended_market_all", "Starts", "Intended Market"),
    spec("completions_dwelling_type_all", "Completions", "Dwelling Type"),
    spec("completions_intended_market_all", "Completions", "Intended Market"),
    spec("under_construction_dwelling_type_all", "Under Construction", "Dwelling Type"),
    spec("under_construction_intended_market_all", "Under Construction", "Intended Market"),
    spec("absorbed_completion_dwelling_type_all", "Absorbed Units", "Dwelling Type"),
    spec("length_of_construction_dwelling_type_all", "Length of Construction", "Dwelling Type")
  )
}


# =============================================================================
# 3. DB / CMHC state — written once, parameterised by geo_level_id
# =============================================================================

#' Latest stored period for a variable at a given geographic level
#'
#' Reads `dictionary.variable_time_vintage` and returns the most recent period
#' already present for one variable at one geographic level and frequency.
#'
#' @param con A live DBI connection.
#' @param variable_id Character. Dictionary variable identifier.
#' @param freq Either `"monthly"` or `"annual"`.
#' @param geo_level_id Character. Dictionary geo level (e.g. `"cma"`, `"ct"`).
#' @return The latest period as a character scalar (`"YYYYMM"` or `"YYYY"`), or
#'   `NA_character_` when nothing is stored yet.
#' @noRd
.cmhc_db_latest <- function(con, variable_id, freq, geo_level_id) {
  width <- if (freq == "monthly") 6L else 4L
  sql <- paste(
    "SELECT max(time) AS mx",
    "  FROM dictionary.variable_time_vintage",
    " WHERE variable_id = $1 AND geo_level_id = $2 AND length(time) = $3"
  )
  mx <- DBI::dbGetQuery(con, sql,
                        params = list(variable_id, geo_level_id, width))$mx[1]
  if (is.null(mx) || is.na(mx)) NA_character_ else as.character(mx)
}

#' Process-level cache of the latest period published by CMHC
#' @noRd
.cmhc_remote_cache <- new.env(parent = emptyenv())

#' Latest period published by CMHC for the survey (cached, national probe)
#'
#' Issues one national `geo_uid = "01"` probe to learn the most recent period
#' CMHC has published for the survey at the given frequency. The survey
#' publishes on a single schedule, so the result is shared across all scales and
#' memoised in [.cmhc_remote_cache] for the session.
#'
#' @param freq Either `"monthly"` or `"annual"`.
#' @param survey Character. CMHC survey code. Defaults to `"Scss"`.
#' @return The latest published period (`"YYYYMM"` or `"YYYY"`), or
#'   `NA_character_` if the probe returns nothing.
#' @noRd
.cmhc_remote_latest <- function(freq, survey = "Scss") {
  key <- paste(survey, freq, sep = "||")
  if (!exists(key, envir = .cmhc_remote_cache, inherits = FALSE)) {
    frequency <- if (freq == "monthly") "Monthly" else "Annual"
    width <- if (freq == "monthly") 6L else 4L
    raw <- tryCatch(
      cmhc::get_cmhc(survey = survey, series = "Starts", dimension = "Dwelling Type",
                     breakdown = "Historical Time Periods", geo_uid = "01",
                     frequency = frequency),
      error = function(e) NULL
    )
    latest <- NA_character_
    if (!is.null(raw) && nrow(raw) && "DateString" %in% names(raw)) {
      p <- vapply(raw$DateString, .cmhc_norm_period, character(1), USE.NAMES = FALSE)
      p <- p[!is.na(p) & nchar(p) == width]
      if (length(p)) latest <- max(p)
    }
    assign(key, latest, envir = .cmhc_remote_cache)
  }
  get(key, envir = .cmhc_remote_cache)
}

#' Clear the cached CMHC availability lookups
#'
#' Empties the session cache populated by [.cmhc_remote_latest()] so the next
#' plan re-probes CMHC for the latest published periods.
#'
#' @return `NULL`, invisibly.
#' @export
cmhc_clear_cache <- function() {
  rm(list = ls(.cmhc_remote_cache), envir = .cmhc_remote_cache)
  invisible(NULL)
}


# =============================================================================
# 4. Gap window — written once. The missing periods to fetch.
# =============================================================================

#' Compute the periods missing from the database for one variable/frequency
#'
#' Determines exactly which periods to fetch by comparing the latest stored
#' period with the latest published period. Monthly: every month after
#' `db_latest` up to `remote_latest`. Annual: every year after `db_latest` up to
#' `remote_latest`. When nothing is stored yet, falls back to a bounded backfill
#' window. The returned `months`/`wanted` fields drive per-(CMA x year x month)
#' task expansion downstream.
#'
#' @param db_latest Latest period in the DB (`"YYYYMM"`/`"YYYY"`), or `NA`.
#' @param remote_latest Latest period CMHC has published, or `NA`.
#' @param freq Either `"monthly"` or `"annual"`.
#' @param backfill_months Months to look back when `db_latest` is `NA`.
#' @param backfill_years Years to look back when `db_latest` is `NA`.
#' @return A list describing the window: `to_fetch`, `freq`, `years`, `months`,
#'   `wanted` (the exact periods), and `start`/`end` bounds.
#' @noRd
.cmhc_window <- function(db_latest, remote_latest, freq,
                         backfill_months = 24L, backfill_years = 5L) {
  empty <- list(to_fetch = FALSE, freq = freq, years = integer(0),
                months = character(0), wanted = character(0),
                start = NA_character_, end = NA_character_)
  if (is.na(remote_latest)) return(empty)

  if (freq == "monthly") {
    end <- remote_latest
    start <- if (is.na(db_latest)) .cmhc_index_to_ym(.cmhc_ym_to_index(end) - backfill_months)
             else                  .cmhc_index_to_ym(.cmhc_ym_to_index(db_latest) + 1L)
    months <- .cmhc_enum_months(start, end)
    if (!length(months)) return(empty)
    list(to_fetch = TRUE, freq = freq,
         years = sort(unique(as.integer(substr(months, 1, 4)))),
         months = sort(unique(substr(months, 5, 6))),
         wanted = months, start = start, end = end)
  } else {
    end_year <- as.integer(substr(remote_latest, 1, 4))
    if (!is.na(db_latest) && as.integer(substr(db_latest, 1, 4)) >= end_year) return(empty)
    start_year <- if (is.na(db_latest)) end_year - backfill_years
                  else as.integer(substr(db_latest, 1, 4)) + 1L
    if (start_year > end_year) return(empty)
    years <- start_year:end_year
    list(to_fetch = TRUE, freq = freq, years = years, months = character(0),
         wanted = as.character(years),
         start = as.character(start_year), end = as.character(end_year))
  }
}


# =============================================================================
# 5. .qsm path helpers
# =============================================================================

#' Default path to the CMHC zone `.qsm` bundle
#'
#' @return Character path to `cmhc_zone_shp.qsm` under
#'   `CURBCUT_DATA_SHARING_PATH`.
#' @export
cmhc_zone_qsm_path <- function() {
  paste0(Sys.getenv("CURBCUT_DATA_SHARING_PATH"), "cc.pipe/cmhc_zone_shp.qsm")
}

#' Default path to the CMHC neighbourhood `.qsm` bundle
#'
#' @return Character path to `cmhc_nbhd_shp.qsm` under
#'   `CURBCUT_DATA_SHARING_PATH`.
#' @export
cmhc_nbhd_qsm_path <- function() {
  paste0(Sys.getenv("CURBCUT_DATA_SHARING_PATH"), "cc.pipe/cmhc_nbhd_shp.qsm")
}

#' Default cache directory for per-task `.qs` files (resumable runs)
#'
#' Tasks are cached under
#' `<cache_dir>/<geo_level_id>/<variable_id>/<freq>/` so a re-run skips what is
#' already fetched and only retries transient failures.
#'
#' @return Character path to the cache root under `CURBCUT_DATA_SHARING_PATH`.
#' @export
cmhc_cache_dir <- function() {
  paste0(Sys.getenv("CURBCUT_DATA_SHARING_PATH"), "cc.pipe/cmhc_cache_update")
}


# =============================================================================
# 6. Per-scale configuration — the ONLY place scales differ
# =============================================================================

#' Build the per-scale configuration table
#'
#' Returns the configuration that drives each geographic scale. Everything the
#' updater needs to treat a scale uniformly is captured here, so the rest of the
#' module stays scale-agnostic.
#'
#' @param zone_qsm_path Path to the zone `.qsm` bundle. Defaults to
#'   [cmhc_zone_qsm_path()].
#' @param nbhd_qsm_path Path to the neighbourhood `.qsm` bundle. Defaults to
#'   [cmhc_nbhd_qsm_path()].
#' @return A named list of five scale configs (`cma`, `csd`, `ct`, `zone`,
#'   `nbhd`). Each carries `geo_level_id`, `geo_vintage`, `unit`, `uids`,
#'   `needs`, and `build_worker`; ZONE/NBHD also carry `qsm_path`.
#' @export
cmhc_levels <- function(zone_qsm_path = cmhc_zone_qsm_path(),
                        nbhd_qsm_path = cmhc_nbhd_qsm_path()) {

  # Shared par_env_objs for the plain fetch->clean->reshape scales (CMA/CSD/CT).
  base_env <- list(
    cmhc_with_retry      = cc.data:::cmhc_with_retry,
    cmhc_clean_results   = cc.data:::cmhc_clean_results,
    cmhc_reshape_results = cc.data:::cmhc_reshape_results,
    cmhc_clean_names     = cc.data:::cmhc_clean_names,
    CMHC_PCT_SERIES      = cc.data:::CMHC_PCT_SERIES
  )

  list(
    # ---------------------------------------------------------------- CMA ----
    cma = list(
      geo_level_id = "cma", geo_vintage = "2021", unit = "self",
      uids = function(con) cc.data::cmhc_get_geo_uids("cma"),
      needs = character(0),
      build_worker = function(spec, refs) list(
        worker = function(geo_uid, req_idx, year) {
          req <- requests[[req_idx]]
          raw <- cmhc_with_retry(
            function() cmhc::get_cmhc(
              survey = req$survey, series = req$series, dimension = req$dimension,
              breakdown = "Historical Time Periods", geo_uid = geo_uid,
              year = year, frequency = .frequency),
            label = sprintf("CMA %s %s", geo_uid, year))
          if (is.null(raw) || nrow(raw) == 0) return(NULL)
          cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
          if (is.null(cleaned)) return(NULL)
          reshaped <- cmhc_reshape_results(cleaned, req$dimension)
          reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
          if (length(reshaped) == 0) return(NULL)
          lapply(reshaped, function(df) { df$geo_vintage <- "2021"; df })
        },
        par_env_objs = base_env
      )
    ),

    # ---------------------------------------------------------------- CSD ----
    csd = list(
      geo_level_id = "csd", geo_vintage = "2021", unit = "self",
      uids = function(con) cc.data::cmhc_get_geo_uids("csd"),
      needs = character(0),
      build_worker = function(spec, refs) list(
        worker = function(geo_uid, req_idx, year) {
          req <- requests[[req_idx]]
          raw <- cmhc_with_retry(
            function() cmhc::get_cmhc(
              survey = req$survey, series = req$series, dimension = req$dimension,
              breakdown = "Historical Time Periods", geo_uid = geo_uid,
              year = year, frequency = .frequency),
            label = sprintf("CSD %s %s", geo_uid, year))
          if (is.null(raw) || nrow(raw) == 0) return(NULL)
          cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
          if (is.null(cleaned)) return(NULL)
          reshaped <- cmhc_reshape_results(cleaned, req$dimension)
          reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
          if (length(reshaped) == 0) return(NULL)
          lapply(reshaped, function(df) { df$geo_vintage <- "2021"; df })
        },
        par_env_objs = base_env
      )
    ),

    # ----------------------------------------------------------------- CT ----
    ct = list(
      geo_level_id = "ct", geo_vintage = "2021", unit = "parent",
      uids = function(con) cc.data::cmhc_get_geo_uids("cma"),
      needs = character(0),
      build_worker = function(spec, refs) list(
        worker = function(geo_uid, req_idx, year, month = NULL) {
          req <- requests[[req_idx]]
          raw <- cmhc_fetch_ct_data(
            req$survey, req$series, req$dimension,
            geo_uid = geo_uid, year = year, month = month)
          if (is.null(raw) || nrow(raw) == 0) return(NULL)
          cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
          if (is.null(cleaned)) return(NULL)
          reshaped <- cmhc_reshape_results(cleaned, req$dimension)
          reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
          if (length(reshaped) == 0) return(NULL)
          # Incremental: only the current year is fetched; 2021+ -> "2021".
          lapply(reshaped, function(df) { df$geo_vintage <- "2021"; df })
        },
        par_env_objs = c(base_env,
                         list(cmhc_fetch_ct_data = cc.data:::cmhc_fetch_ct_data))
      )
    ),

    # --------------------------------------------------------------- ZONE ----
    zone = list(
      geo_level_id = "cmhc_zone", geo_vintage = "2026", unit = "parent",
      uids = function(con) cc.data::cmhc_get_geo_uids("cma"),
      needs = c("cma_to_met", "zones_lookup"),
      qsm_path = zone_qsm_path,
      build_worker = function(spec, refs) {
        cma_to_met    <- refs$cma_to_met
        zones_lookup  <- refs$zones_lookup
        zone_qsm_path <- refs$zone_qsm_path
        list(
          worker = function(geo_uid, req_idx, year, month = NULL) {
            req <- requests[[req_idx]]
            raw <- cmhc_fetch_zone_data(
              req$survey, req$series, req$dimension,
              geo_uid = geo_uid, year = year, month = month)
            if (is.null(raw) || nrow(raw) == 0) return(NULL)
            met_row <- cma_to_met[cma_to_met$cma_uid == geo_uid, , drop = FALSE]
            if (nrow(met_row) == 0) return(NULL)
            raw$met_code <- met_row$met_code[1]
            raw <- cmhc_attach_zone_id(raw, year = year,
                                       zones_lookup = zones_lookup,
                                       zone_qsm_path = zone_qsm_path)
            if (is.null(raw) || nrow(raw) == 0) return(NULL)
            cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
            if (is.null(cleaned)) return(NULL)
            reshaped <- cmhc_reshape_results(cleaned, req$dimension)
            reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
            if (length(reshaped) == 0) return(NULL)
            lapply(reshaped, function(df) { df$geo_vintage <- "2026"; df })
          },
          par_env_objs = c(base_env, list(
            cma_to_met = cma_to_met,
            zones_lookup = zones_lookup,
            zone_qsm_path = zone_qsm_path,
            cmhc_fetch_zone_data = cc.data:::cmhc_fetch_zone_data,
            cmhc_normalize_zone_name = cc.data:::cmhc_normalize_zone_name,
            cmhc_match_zone_name = cc.data:::cmhc_match_zone_name,
            cmhc_attach_zone_id = cc.data:::cmhc_attach_zone_id
          ))
        )
      }
    ),

    # --------------------------------------------------------------- NBHD ----
    nbhd = list(
      geo_level_id = "cmhc_nbhd", geo_vintage = "2026", unit = "parent",
      uids = function(con) cc.data::cmhc_get_geo_uids("cma"),
      needs = c("cma_to_met", "nbhd_matching"),
      qsm_path = nbhd_qsm_path,
      build_worker = function(spec, refs) {
        cma_to_met    <- refs$cma_to_met
        nbhd_matching <- refs$nbhd_matching
        qsm_path      <- refs$nbhd_qsm_path
        list(
          worker = function(geo_uid, req_idx, year, month = NULL) {
            req <- requests[[req_idx]]
            raw <- cmhc_fetch_nbhd_data(
              req$survey, req$series, req$dimension,
              geo_uid = geo_uid, year = year, month = month)
            if (is.null(raw) || nrow(raw) == 0) return(NULL)
            raw <- cmhc_apply_nbhd_renames(raw)
            raw <- cmhc_attach_nbhd_id(raw, year = year, geo_uid = geo_uid,
                                       cma_to_met = cma_to_met,
                                       qsm_path = qsm_path)
            if (is.null(raw) || nrow(raw) == 0) return(NULL)
            cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
            if (is.null(cleaned)) return(NULL)
            reshaped <- cmhc_reshape_results(cleaned, req$dimension)
            reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
            if (length(reshaped) == 0) return(NULL)
            lapply(reshaped, function(df) { df$geo_vintage <- "2026"; df })
          },
          par_env_objs = c(base_env, list(
            cma_to_met = cma_to_met,
            qsm_path = qsm_path,
            cmhc_nbhd_matching = nbhd_matching,   # dataset needed by renames
            cmhc_fetch_nbhd_data = cc.data:::cmhc_fetch_nbhd_data,
            cmhc_apply_nbhd_renames = cc.data:::cmhc_apply_nbhd_renames,
            cmhc_attach_nbhd_id = cc.data:::cmhc_attach_nbhd_id
          ))
        )
      }
    )
  )
}


# =============================================================================
# 7. Shared references — built once, only when a scale needs them
# =============================================================================

#' Build the shared reference objects required by the matching scales
#'
#' Constructs only the references named in `needs`, each at most once, reusing
#' any caller-supplied objects. ZONE needs `cma_to_met` + `zones_lookup`; NBHD
#' needs `cma_to_met` + `nbhd_matching`; CMA/CSD/CT need none.
#'
#' @param needs Character vector of reference names to build.
#' @param zone_qsm_path,nbhd_qsm_path Paths carried through to the worker.
#' @param cma_to_met,zones_lookup,nbhd_matching Optional pre-built references;
#'   built on demand when `NULL`.
#' @return A named list of references, always including the two `.qsm` paths.
#' @noRd
.cmhc_build_refs <- function(needs, zone_qsm_path, nbhd_qsm_path,
                             cma_to_met = NULL, zones_lookup = NULL,
                             nbhd_matching = NULL) {
  refs <- list(zone_qsm_path = zone_qsm_path, nbhd_qsm_path = nbhd_qsm_path)
  if ("cma_to_met" %in% needs)
    refs$cma_to_met <- cma_to_met %||% cc.data:::cmhc_get_cma_to_met()
  if ("zones_lookup" %in% needs)
    refs$zones_lookup <- zones_lookup %||% cc.data:::cmhc_zone_lookup()
  if ("nbhd_matching" %in% needs)
    refs$nbhd_matching <- nbhd_matching %||% cc.data::cmhc_nbhd_matching
  refs
}


# =============================================================================
# 8. Core fetch — written once. Builds tasks, runs the engine, filters.
# =============================================================================

#' Fetch the missing periods for one request at one scale
#'
#' Builds tasks from the scale's `unit` (`"self"` -> per geo x year;
#' `"parent"` -> per CMA x year [x month]), assembles the scale's worker via
#' `build_worker`, and runs everything through cc.data's engine. When
#' `cache_dir` is set, the engine writes one `.qs` per task and, on re-run, skips
#' tasks already cached and retries only transient failures; results are then
#' read back from the cache (CT passes `geo_level = "ct"` so omitted CTs are
#' NA-supplemented and IDs validated against `geography.ct`). When `cache_dir`
#' is `NULL`, results accumulate in RAM with no resume on crash. Either way the
#' output is finalised (GeoUID -> id) and trimmed to the wanted periods.
#'
#' @param level One scale config from [cmhc_levels()].
#' @param spec One request spec from [cmhc_registry()].
#' @param window The gap window from [.cmhc_window()].
#' @param uids Geo (or CMA parent) UIDs to iterate.
#' @param refs Shared references from [.cmhc_build_refs()].
#' @param cache_dir Optional per-task `.qs` cache directory.
#' @return A named list of long frames (`id`, `time`, `geo_vintage`, `value`),
#'   keyed by reshaped CMHC variable name. Empty list when nothing to fetch.
#' @noRd
.cmhc_fetch <- function(level, spec, window, uids, refs, cache_dir = NULL) {
  if (!window$to_fetch) return(list())

  monthly <- window$freq == "monthly"

  if (level$unit == "parent" && monthly) {
    requests <- list(list(survey = spec$survey, series = spec$series,
                          dimension = spec$dimension,
                          years = as.character(window$years),
                          months = window$months))
    tasks <- cc.data:::cmhc_build_tasks(uids, requests,
                                        expand_years = TRUE, expand_months = TRUE)
  } else {
    requests <- list(list(survey = spec$survey, series = spec$series,
                          dimension = spec$dimension,
                          years = as.character(window$years)))
    tasks <- cc.data:::cmhc_build_tasks(uids, requests, expand_years = TRUE)
  }

  frequency <- if (monthly) "Monthly" else "Annual"

  built  <- level$build_worker(spec, refs)
  worker <- built$worker

  # Bind ALL helper objects (requests, .frequency, and every cc.data function
  # in par_env_objs) DIRECTLY into the worker's environment. This makes the
  # worker self-contained so it resolves everything in BOTH modes — sequential
  # (where the engine does NOT apply par_env_objs) and parallel (where the
  # daemon also gets them via par_env_objs below).
  per_fetch <- list(requests = requests, .frequency = frequency)
  worker_env <- list2env(c(per_fetch, built$par_env_objs),
                         parent = environment(worker))
  environment(worker) <- worker_env

  # The engine ships par_env_objs to daemons (parallel mode).
  par_env_objs <- c(per_fetch, built$par_env_objs)

  # Per (scale / variable / freq) cache subfolder so tasks never mix.
  out_dir <- if (!is.null(cache_dir)) {
    d <- file.path(cache_dir, level$geo_level_id, spec$variable_id, window$freq)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    d
  } else NULL

  ran <- cc.data:::cmhc_run_and_collect(
    tasks, worker,
    par_env_objs = par_env_objs,
    output_dir = out_dir,
    label = sprintf("cmhc_fetch[%s/%s]", level$geo_level_id, spec$variable_id)
  )

  # With output_dir, the engine returns the dir path: read each reshaped key
  # back from the cache. Without it, `ran` is already the collected list.
  if (is.character(ran)) {
    keys <- cc.data:::cmhc_list_variables(ran, n_sample = Inf)
    # For CT, pass geo_level = "ct" so cmhc_read_variable supplements the CTs
    # CMHC omitted with NA rows and validates IDs against geography.ct — exactly
    # like the original cmhc_get_*_ct pipeline. Other scales skip this (the
    # special NA-fill only triggers for "ct"); passing NULL keeps them unchanged.
    rv_geo_level <- if (identical(level$geo_level_id, "ct")) "ct" else NULL
    collected <- stats::setNames(
      lapply(keys, function(k) {
        cc.data:::cmhc_read_variable(ran, k, geo_level = rv_geo_level)
      }),
      keys
    )
    collected <- Filter(function(df) !is.null(df) && nrow(df) > 0, collected)
    finalized <- collected  # cmhc_read_variable already renames GeoUID -> id
  } else {
    finalized <- cc.data:::cmhc_finalize_results(ran)  # GeoUID -> id
  }

  lapply(finalized, function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    df <- df[df$time %in% window$wanted, , drop = FALSE]
    df$geo_vintage <- level$geo_vintage
    df[, c("id", "time", "geo_vintage", "value"), drop = FALSE]
  })
}


# =============================================================================
# 9. Plan + run for ONE scale — written once
# =============================================================================

#' Build the refresh plan for one scale (read-only)
#'
#' Computes, for every variable x frequency at one scale, the latest stored and
#' published periods, the resulting gap window, and an estimate of the API calls
#' a real fetch would make. Performs no network fetch beyond the cached national
#' availability probe and no upload.
#'
#' @param con A live DBI connection.
#' @param scale One of `"cma"`, `"csd"`, `"ct"`, `"zone"`, `"nbhd"`.
#' @param uids Optional geo (or CMA parent) UIDs. Defaults to the scale's own.
#' @param registry Request registry. Defaults to [cmhc_registry()].
#' @param levels Scale config table. Defaults to [cmhc_levels()].
#' @return A data frame, one row per variable x frequency, with `scale`,
#'   `variable_id`, `freq`, `db_latest`, `remote_latest`, `to_fetch`, `window`,
#'   and `est_calls`.
#' @export
cmhc_plan <- function(con, scale, uids = NULL,
                      registry = cmhc_registry(),
                      levels = cmhc_levels()) {
  level <- levels[[scale]]
  if (is.null(level)) stop(sprintf("Unknown scale '%s'.", scale))
  if (is.null(uids)) uids <- level$uids(con)
  n_uid <- length(uids)

  rows <- list()
  for (spec in registry) {
    for (freq in spec$frequencies) {
      db_latest <- .cmhc_db_latest(con, spec$variable_id, freq, level$geo_level_id)
      remote_latest <- .cmhc_remote_latest(freq, spec$survey)
      window <- .cmhc_window(db_latest, remote_latest, freq)
      est_calls <- if (!window$to_fetch) 0L
                   else if (level$unit == "parent" && freq == "monthly")
                     n_uid * length(window$wanted)
                   else n_uid * length(window$years)
      rows[[length(rows) + 1L]] <- data.frame(
        scale = scale, variable_id = spec$variable_id, freq = freq,
        db_latest = db_latest %||% NA_character_,
        remote_latest = remote_latest %||% NA_character_,
        to_fetch = window$to_fetch,
        window = paste0(window$start %||% "-", "..", window$end %||% "-"),
        est_calls = as.integer(est_calls),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Run the incremental refresh for ONE scale (fetch only — no upload)
#'
#' Builds the plan, then fetches every period flagged `to_fetch`, returning
#' upload-ready long frames. Requires running mirai daemons for a real parallel
#' fetch. Performs no upload.
#'
#' @param con A live DBI connection (e.g. `cc.pipe::db_connect()`).
#' @param scale One of `"cma"`, `"csd"`, `"ct"`, `"zone"`, `"nbhd"`.
#' @param uids Optional geo (or CMA parent) UIDs. Defaults to the scale's own.
#' @param registry Request registry. Defaults to [cmhc_registry()].
#' @param levels Scale config table. Defaults to [cmhc_levels()].
#' @param refs Optional pre-built shared references. Built on demand when `NULL`.
#' @param cache_dir Optional directory for per-task `.qs` caching. When set, a
#'   re-run skips tasks already cached and retries only transient failures, so a
#'   crash mid-run is resumable. Defaults to [cmhc_cache_dir()].
#' @param dry_run If `TRUE`, return only the plan.
#' @return A list with `plan` and `data`, where
#'   `data[[variable_id]][[freq]]` is a named list of long frames
#'   (`id`, `time`, `geo_vintage`, `value`).
#' @export
cmhc_refresh <- function(con, scale, uids = NULL,
                         registry = cmhc_registry(),
                         levels = cmhc_levels(),
                         refs = NULL, cache_dir = cmhc_cache_dir(),
                         dry_run = FALSE) {
  level <- levels[[scale]]
  if (is.null(level)) stop(sprintf("Unknown scale '%s'.", scale))
  if (is.null(uids)) uids <- level$uids(con)

  plan <- cmhc_plan(con, scale, uids, registry, levels)
  message(sprintf("[%s] plan: %d combinations, %d to fetch, ~%s estimated API calls.",
                  scale, nrow(plan), sum(plan$to_fetch),
                  format(sum(plan$est_calls), big.mark = " ")))
  if (dry_run) return(list(plan = plan, data = NULL))

  if (is.null(refs)) {
    refs <- .cmhc_build_refs(
      level$needs %||% character(0),
      zone_qsm_path = level$qsm_path %||% cmhc_zone_qsm_path(),
      nbhd_qsm_path = level$qsm_path %||% cmhc_nbhd_qsm_path())
  }

  by_var <- stats::setNames(registry, vapply(registry, `[[`, character(1), "variable_id"))

  data <- list()
  for (i in which(plan$to_fetch)) {
    variable_id <- plan$variable_id[i]; freq <- plan$freq[i]
    window <- .cmhc_window(plan$db_latest[i], plan$remote_latest[i], freq)
    message(sprintf("[fetch] %-44s | %-9s | %-7s | %s",
                    variable_id, level$geo_level_id, freq, plan$window[i]))
    fetched <- tryCatch(
      .cmhc_fetch(level, by_var[[variable_id]], window, uids, refs,
                  cache_dir = cache_dir),
      error = function(e) {
        warning(sprintf("FAILED %s / %s / %s: %s",
                        variable_id, scale, freq, conditionMessage(e)))
        NULL
      }
    )
    data[[variable_id]][[freq]] <- fetched
  }
  list(plan = plan, data = data)
}


# =============================================================================
# 10. Orchestrator — run every scale, written once
# =============================================================================

#' Geographic scales handled, in run order (cheapest first)
#'
#' @return Character vector `c("cma", "csd", "ct", "zone", "nbhd")`.
#' @export
cmhc_all_scales <- function() c("cma", "csd", "ct", "zone", "nbhd")

#' Run the incremental refresh across all (or some) scales — fetch only
#'
#' Refreshes each requested scale in turn, building the shared references once
#' and reusing them across scales (`cma_to_met` by ZONE+NBHD, `zones_lookup` by
#' ZONE, `nbhd_matching` by NBHD). Performs no upload.
#'
#' @param con A live DBI connection.
#' @param scales Scales to run. Defaults to [cmhc_all_scales()].
#' @param dry_run If `TRUE`, only build each scale's plan.
#' @param zone_qsm_path,nbhd_qsm_path Optional explicit `.qsm` paths.
#' @param cma_to_met,zones_lookup,nbhd_matching Optional pre-built references.
#' @param cache_dir Optional per-task `.qs` cache directory. Defaults to
#'   [cmhc_cache_dir()].
#' @param continue_on_error If `TRUE` (default), keep going after a scale fails.
#' @return A list with `plans`, `data`, and `errors`, each keyed by scale.
#'   `data` is `NULL` when `dry_run = TRUE`.
#' @export
cmhc_refresh_all <- function(con,
                             scales = cmhc_all_scales(),
                             dry_run = FALSE,
                             zone_qsm_path = cmhc_zone_qsm_path(),
                             nbhd_qsm_path = cmhc_nbhd_qsm_path(),
                             cma_to_met = NULL,
                             zones_lookup = NULL,
                             nbhd_matching = NULL,
                             cache_dir = cmhc_cache_dir(),
                             continue_on_error = TRUE) {
  levels <- cmhc_levels(zone_qsm_path = zone_qsm_path,
                        nbhd_qsm_path = nbhd_qsm_path)

  needs <- unique(unlist(lapply(scales,
                                function(s) levels[[s]]$needs %||% character(0))))
  refs <- if (dry_run) NULL else .cmhc_build_refs(
    needs, zone_qsm_path, nbhd_qsm_path,
    cma_to_met = cma_to_met, zones_lookup = zones_lookup,
    nbhd_matching = nbhd_matching)

  plans <- list(); data <- list(); errors <- list()

  for (scale in scales) {
    message(sprintf("\n========== SCALE: %s ==========", toupper(scale)))
    res <- tryCatch(
      cmhc_refresh(con, scale, registry = cmhc_registry(),
                   levels = levels, refs = refs, cache_dir = cache_dir,
                   dry_run = dry_run),
      error = function(e) {
        msg <- conditionMessage(e)
        warning(sprintf("[%s] FAILED: %s", scale, msg), call. = FALSE)
        structure(list(error = msg), class = "cmhc_scale_error")
      }
    )
    if (inherits(res, "cmhc_scale_error")) {
      errors[[scale]] <- res$error
      if (!continue_on_error) break
      next
    }
    plans[[scale]] <- res$plan
    if (!dry_run) data[[scale]] <- res$data
  }

  if (length(plans)) {
    message("\n========== SUMMARY ==========")
    for (scale in names(plans)) {
      p <- plans[[scale]]
      message(sprintf("%-5s | %2d to fetch | ~%s API calls%s",
                      scale, sum(p$to_fetch),
                      format(sum(p$est_calls), big.mark = " "),
                      if (!is.null(errors[[scale]])) "  [ERROR]" else ""))
    }
  }
  if (length(errors)) message("\nScales with errors: ",
                              paste(names(errors), collapse = ", "))

  list(plans = plans,
       data  = if (dry_run) NULL else data,
       errors = errors)
}