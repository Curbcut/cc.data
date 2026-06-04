## ============================================================================
##  poi_evolution.R — POI evolution over time (per geography, optionally per
##                    category)
##
##  Public functions:
##    poi_data()                  — assign POIs to a geo layer, compute yearly
##                                  cumulative counts (total or per category)
##    poi_run_evolution()         — wrap poi_data() across a list of geo layers,
##                                  pulling POIs from Snowflake
##    poi_run_evolution_local()   — same, but reading POIs from a local file
## ============================================================================


#' Spatially join POIs to a geography
#'
#' Internal helper. Converts `poi_df` (with `LONGITUDE`/`LATITUDE`) to `sf`,
#' harmonizes CRS with `geo_sf`, fixes invalid geometries, and assigns each
#' POI the id of the (first) intersecting geographic unit.
#'
#' Looks for the geographic id column in this order: `geouid`, `ID`, `id`.
#'
#' @param poi_df A data frame with `LONGITUDE` and `LATITUDE` columns.
#' @param geo_sf An `sf` polygon layer with `geouid`, `ID`, or `id`.
#'
#' @return An `sf` data frame (geometry dropped) with the assigned `id`.
#'   POIs that fall outside `geo_sf` have `id = NA` and are kept; callers
#'   typically filter them out.
#' @keywords internal
poi_join_to_geo <- function(poi_df, geo_sf) {
  poi_sf <- poi_df |>
    dplyr::filter(!is.na(.data$LONGITUDE), !is.na(.data$LATITUDE)) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

  if (sf::st_crs(poi_sf) != sf::st_crs(geo_sf)) {
    geo_sf <- sf::st_transform(geo_sf, sf::st_crs(poi_sf))
  }
  if (any(!sf::st_is_valid(geo_sf))) geo_sf <- sf::st_make_valid(geo_sf)
  if (any(!sf::st_is_valid(poi_sf))) poi_sf <- sf::st_make_valid(poi_sf)

  matched <- sf::st_intersects(poi_sf, geo_sf)

  id_col <- intersect(c("geouid", "ID", "id"), names(geo_sf))[1]
  if (is.na(id_col)) {
    stop("geo_sf must have one of: 'geouid', 'ID', or 'id' columns.")
  }

  poi_sf$id <- sapply(matched, function(x) {
    if (length(x) > 0) geo_sf[[id_col]][x[1]] else NA
  })
  sf::st_drop_geometry(poi_sf)
}


#' Compute yearly cumulative POI counts for one geography
#'
#' Spatially assigns each POI to a geographic unit, then computes a yearly
#' cumulative count: `poi_added − poi_closed`, accumulated over time.
#'
#' Two modes:
#' \itemize{
#'   \item \strong{Total mode} (default): one column per year, named
#'         \code{point_of_interest_<year>}. Shape: \code{id × years}.
#'   \item \strong{Per-category mode} (when \code{poi_df} has a
#'         \code{CURBCUT_TYPE} or \code{category} column, and
#'         \code{categorize = TRUE}): an extra \code{category} column;
#'         one row per (id, category). Shape: \code{(id × category) × years}.
#' }
#'
#' Required columns in \code{poi_df}: \code{LONGITUDE}, \code{LATITUDE},
#' \code{DATE_CREATED}, \code{DATE_CLOSED}. Closure dates may be \code{NA}
#' for still-active POIs.
#'
#' @param poi_df A data frame of POIs (typically from
#'   [foursquare_get_canada()] in legacy mode).
#' @param geo_sf An `sf` polygon layer with `geouid`, `ID`, or `id`.
#' @param categorize Logical. If TRUE and \code{poi_df} has a category
#'   column (`CURBCUT_TYPE` or `category`), counts are computed per
#'   category. Default FALSE.
#'
#' @return A wide-format data frame: \code{id}, optionally \code{category},
#'   then one column per year named \code{point_of_interest_<year>}.
#' @export
poi_data <- function(poi_df, geo_sf, categorize = FALSE) {

  ## Detect a category column if the caller asked for per-category counts.
  cat_col <- NULL
  if (categorize) {
    cat_col <- intersect(c("CURBCUT_TYPE", "curbcut_type", "category"),
                         names(poi_df))[1]
    if (is.na(cat_col)) {
      warning("categorize=TRUE but no category column found. Falling back to total mode.")
      categorize <- FALSE
    }
  }

  poi_joined <- poi_join_to_geo(poi_df, geo_sf)

  poi_long <- poi_joined |>
    dplyr::filter(!is.na(.data$id)) |>
    dplyr::mutate(
      year_created = lubridate::year(lubridate::ymd(.data$DATE_CREATED)),
      year_closed  = lubridate::year(lubridate::ymd(.data$DATE_CLOSED))
    )

  group_cols <- if (categorize) c("id", cat_col) else "id"

  ## Added per (id [, category], year)
  added <- poi_long |>
    dplyr::filter(!is.na(.data$year_created)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "year_created")))) |>
    dplyr::summarise(poi_added = dplyr::n(), .groups = "drop") |>
    dplyr::rename(year = "year_created")

  ## Closed per (id [, category], year)
  closed <- poi_long |>
    dplyr::filter(!is.na(.data$year_closed)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "year_closed")))) |>
    dplyr::summarise(poi_closed = dplyr::n(), .groups = "drop") |>
    dplyr::rename(year = "year_closed")

  evo <- dplyr::full_join(added, closed,
                          by = c(group_cols, "year")) |>
    dplyr::mutate(dplyr::across(c("poi_added", "poi_closed"),
                                ~ replace(., is.na(.), 0))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "year")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(poi_total = cumsum(.data$poi_added) - cumsum(.data$poi_closed)) |>
    dplyr::ungroup()

  evo |>
    dplyr::select(dplyr::all_of(c(group_cols, "year")), "poi_total") |>
    dplyr::mutate(year = paste0("point_of_interest_", .data$year)) |>
    tidyr::pivot_wider(
      names_from  = "year",
      values_from = "poi_total",
      values_fill = list(poi_total = 0)
    )
}


#' Compute POI evolution for multiple geographies (from Snowflake)
#'
#' Connects to Foursquare via [foursquare_connect()], retrieves Canadian
#' POIs via [foursquare_get_canada()], and applies [poi_data()] to each
#' geographic layer in `geo_list` (typically the output of
#' \code{cc.pipe::get_census_digital_scales()}).
#'
#' @param geo_list A named list of `sf` polygon layers.
#' @param categorize Logical. If TRUE, each result is split by Curbcut
#'   category (requires the categorized Foursquare extract).
#' @param passcode Optional MFA passcode for Snowflake.
#'
#' @return A named list of data frames, one per geography (see [poi_data()]).
#' @export
poi_run_evolution <- function(geo_list, categorize = FALSE, passcode = NULL) {
  message("Starting POI evolution processing...")

  conn <- foursquare_connect(passcode = passcode)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  poi_df <- foursquare_get_canada(conn, categorize = categorize)

  results <- lapply(names(geo_list), function(name) {
    message(paste0("Processing layer: ", name))
    poi_data(poi_df = poi_df, geo_sf = geo_list[[name]], categorize = categorize)
  })

  names(results) <- names(geo_list)
  message("All layers processed successfully.")
  results
}


#' Compute POI evolution for multiple geographies (from a local file)
#'
#' Same as [poi_run_evolution()] but reads POIs from a local `.rds` or `.qs`
#' file rather than Snowflake. Useful for re-running with a frozen snapshot.
#'
#' @param geo_list A named list of `sf` polygon layers.
#' @param poi_path Path to a local POI file. `.rds` is read via
#'   [base::readRDS()]; `.qs` is read via `qs::qread()`.
#' @param categorize Logical. If TRUE, expects a category column in the
#'   loaded POI data.
#'
#' @return A named list of data frames, one per geography.
#' @export
poi_run_evolution_local <- function(geo_list, poi_path, categorize = FALSE) {
  message("Loading local POI data...")

  ext <- tolower(tools::file_ext(poi_path))
  poi_df <- switch(ext,
    "rds" = readRDS(poi_path),
    "qs"  = {
      if (!requireNamespace("qs2", quietly = TRUE)) stop("Package 'qs2' required.")
      qs2::qs_read(poi_path)
    },
    stop("Unsupported file extension: ", ext, ". Expected .rds or .qs.")
  )

  results <- lapply(names(geo_list), function(name) {
    message(paste0("Processing layer: ", name))
    poi_data(poi_df = poi_df, geo_sf = geo_list[[name]], categorize = categorize)
  })

  names(results) <- names(geo_list)
  message("All layers processed successfully.")
  results
}


## ============================================================================
##  foursquare.R — Foursquare POI extraction (Snowflake)
##
##  Public functions:
##    foursquare_connect()         — open a Snowflake connection
##    foursquare_categorize_sql()  — SQL CASE expression for Curbcut categories
##    foursquare_get_canada()      — fetch CA POIs (with or without categories)
## ============================================================================


#' Connect to the Foursquare Snowflake database
#'
#' Opens an ODBC connection to the Foursquare Open Source Places dataset on
#' Snowflake. Credentials are read from environment variables. The MFA passcode
#' (if MFA is enabled on the account) can be supplied via `passcode`.
#'
#' Required environment variables:
#' \itemize{
#'   \item \code{SNOWFLAKE_SERVER}
#'   \item \code{SNOWFLAKE_UID}
#'   \item \code{SNOWFLAKE_PWD}
#'   \item \code{SNOWFLAKE_DB}
#'   \item \code{SNOWFLAKE_SCHEMA}
#' }
#'
#' @param passcode Optional MFA passcode (character). If `NULL`, the connection
#'   is opened without an `Authenticator`/`PASSCODE` field — appropriate for
#'   accounts without MFA.
#' @param warehouse Snowflake warehouse name. Defaults to `"COMPUTE_WH"`.
#'
#' @return A `DBI` connection object.
#' @export
foursquare_connect <- function(passcode = NULL, warehouse = "COMPUTE_WH") {
  message("Connecting to Foursquare Snowflake database...")

  args <- list(
    drv       = odbc::odbc(),
    Driver    = "SnowflakeDSIIDriver",
    Server    = Sys.getenv("SNOWFLAKE_SERVER"),
    UID       = Sys.getenv("SNOWFLAKE_UID"),
    PWD       = Sys.getenv("SNOWFLAKE_PWD"),
    Database  = Sys.getenv("SNOWFLAKE_DB"),
    Schema    = Sys.getenv("SNOWFLAKE_SCHEMA"),
    Warehouse = warehouse
  )

  if (!is.null(passcode)) {
    args$Authenticator <- "snowflake"
    args$PASSCODE      <- passcode
  }

  do.call(DBI::dbConnect, args)
}


#' Foursquare → Curbcut category SQL expression
#'
#' Returns the SQL `CASE` expression that assigns each Foursquare POI to
#' exactly one Curbcut destination category, based on its
#' `fsq_category_labels` array. The expression is intended to be embedded
#' inside a `SELECT` statement (see [foursquare_get_canada()]).
#'
#' Categories, in priority order (first match wins):
#' \enumerate{
#'   \item dentist
#'   \item pharmacy
#'   \item gas_station
#'   \item cinema
#'   \item bank
#'   \item cafe
#'   \item bakery
#'   \item bar
#'   \item restaurant
#'   \item gym
#'   \item clinic
#'   \item grocery_store
#' }
#'
#' POIs that do not match any of these patterns get `NULL`.
#'
#' @return A character string (one SQL CASE expression).
#' @export
foursquare_categorize_sql <- function() {
  "CASE
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Dentist%'
      THEN 'dentist'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Pharmacy%'
      OR ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Drugstore%'
      THEN 'pharmacy'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Fuel Station%'
      THEN 'gas_station'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Movie Theater%'
      THEN 'cinema'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Banking and Finance > Bank%'
      OR ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Banking and Finance > Credit Union%'
      THEN 'bank'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Cafe, Coffee, and Tea House%'
      THEN 'cafe'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Bakery%'
      THEN 'bakery'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Dining and Drinking > Bar%'
      THEN 'bar'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Restaurant%'
      OR ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Food Truck%'
      THEN 'restaurant'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Gym and Studio%'
      OR ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Martial Arts Dojo%'
      OR ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%> Gymnastics%'
      THEN 'gym'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Health and Medicine%'
      AND ARRAY_TO_STRING(fsq_category_labels, ' || ') NOT ILIKE '%> Dentist%'
      AND ARRAY_TO_STRING(fsq_category_labels, ' || ') NOT ILIKE '%> Hospital%'
      AND ARRAY_TO_STRING(fsq_category_labels, ' || ') NOT ILIKE '%> Veterinarian%'
      AND ARRAY_TO_STRING(fsq_category_labels, ' || ') NOT ILIKE '%> Spa%'
      THEN 'clinic'
    WHEN ARRAY_TO_STRING(fsq_category_labels, ' || ') ILIKE '%Food and Beverage Retail%'
      THEN 'grocery_store'
    ELSE NULL
  END"
}


#' Retrieve Foursquare POIs for Canada
#'
#' Pulls all Points of Interest from `FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES`
#' where `country = 'CA'`. Two modes:
#'
#' \itemize{
#'   \item \code{categorize = FALSE} (legacy mode): returns every Canadian POI
#'         with `date_created`, `date_refreshed`, `date_closed` — suitable for
#'         time-series counts via [poi_data()].
#'   \item \code{categorize = TRUE} (recommended): adds a `curbcut_type` column
#'         (one of the 12 Curbcut categories) using [foursquare_categorize_sql()],
#'         filters to POIs assigned a category, and excludes closed POIs.
#'         Suitable for per-category counts or static destination uploads.
#' }
#'
#' @param conn A `DBI` connection from [foursquare_connect()].
#' @param categorize Logical. If TRUE, assigns each POI to a Curbcut category
#'   and drops uncategorized rows.
#' @param active_only Logical. If TRUE, excludes POIs with a non-NULL
#'   `date_closed`. Defaults to TRUE when `categorize = TRUE` (matches the new
#'   destination-extraction workflow) and FALSE when `categorize = FALSE`
#'   (matches legacy evolution workflow, which needs closure dates).
#'
#' @return A data frame. Always includes `fsq_place_id`, `name`, `latitude`,
#'   `longitude`, `locality`, `region`, `country`. When `categorize = FALSE`,
#'   adds `fsq_category_labels`, `date_created`, `date_refreshed`,
#'   `date_closed`. When `categorize = TRUE`, adds `address`, `postcode`,
#'   `fsq_category_labels`, `curbcut_type`.
#' @export
foursquare_get_canada <- function(conn,
                                  categorize  = FALSE,
                                  active_only = categorize) {

  if (categorize) {
    where_active <- if (active_only) "  AND date_closed IS NULL" else ""
    query <- sprintf("
      SELECT
        fsq_place_id, name, latitude, longitude,
        address, locality, region, postcode, country,
        fsq_category_labels,
        %s AS curbcut_type
      FROM FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES
      WHERE country = 'CA'
        AND latitude IS NOT NULL
        AND longitude IS NOT NULL
%s",
      foursquare_categorize_sql(),
      where_active
    )

    message("Retrieving categorized Foursquare POIs for Canada...")
    df <- DBI::dbGetQuery(conn, query)
    df <- df[!is.na(df$CURBCUT_TYPE) | !is.na(df$curbcut_type), , drop = FALSE]
    return(df)
  }

  ## Legacy mode — every Canadian POI, with creation/closure dates.
  where_active <- if (active_only) "  AND date_closed IS NULL" else ""
  query <- sprintf("
    SELECT fsq_place_id, name, fsq_category_labels, latitude, longitude,
           locality, region, country,
           date_created, date_refreshed, date_closed
    FROM FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES
    WHERE country = 'CA'%s",
    where_active
  )

  message("Retrieving Foursquare POIs for Canada (legacy / time-series mode)...")
  DBI::dbGetQuery(conn, query)
}