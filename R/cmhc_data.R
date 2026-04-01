#' Fetch CMHC Data for a Specific CMA
#'
#' Downloads CMHC data for a given CMA using specified parameters. Handles API errors
#' gracefully using `tryCatch`, and returns `NULL` if the query fails.
#'
#' @param survey Character. The CMHC survey code (e.g., "Rms", "Scss").
#' @param series Character. The series to extract (e.g., "Vacancy Rate").
#' @param dimension Character. The breakdown dimension (e.g., "Bedroom Type"). Can be `NULL`.
#' @param geo_uid Character. Geographic identifier of the CMA (e.g., "24462").
#' @param frequency Character. Must be either "Monthly" or "Annual".
#'
#' @return A raw CMHC `data.frame`, or `NULL` if an error occurs.
#' Retry wrapper for CMHC API calls
#'
#' Retries a CMHC API call with exponential backoff when transient errors occur
#' (HTTP 500, rate limiting, connection timeouts). Non-retryable errors (e.g.
#' "No data available") are returned as-is on the first attempt.
#'
#' @param expr An expression (typically a `cmhc::get_cmhc()` call).
#' @param max_retries Integer. Maximum number of retry attempts (default 1).
#' @param base_delay Numeric. Initial delay in seconds before first retry (default 2).
#' @param label Character. Label for warning messages (e.g. "CMA 24462").
#'
#' @return The result of `expr`, or `NULL` if all retries fail.
cmhc_with_retry <- function(.f, max_retries = 1, base_delay = 2, label = "") {
  # cmhc::get_cmhc() catches HTTP errors internally and returns NULL with a
  # warning() instead of throwing an error. So we must intercept warnings too.

  # Patterns that mean "server overloaded / transient" — worth retrying,
  # and if all retries fail we must NOT cache the result as "no data".
  transient_patterns <- c(
    "status 502",
    "status 503",
    "status 429",
    "timed out",
    "timeout",
    "connection reset",
    "could not resolve"
  )

  # 500 is retryable (could be transient) but after exhausting retries

  # it means "genuinely no data" — safe to cache as empty.
  retryable_patterns <- c("status 500", transient_patterns)

  is_retryable_msg <- function(msg) {
    msg <- tolower(msg)
    any(vapply(
      retryable_patterns,
      function(p) grepl(p, msg, fixed = TRUE),
      logical(1)
    ))
  }

  is_transient_msg <- function(msg) {
    msg <- tolower(msg)
    any(vapply(
      transient_patterns,
      function(p) grepl(p, msg, fixed = TRUE),
      logical(1)
    ))
  }

  last_warns <- character(0)

  for (attempt in seq_len(max_retries + 1)) {
    warns <- character(0)

    result <- tryCatch(
      withCallingHandlers(
        .f(),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        structure(list(message = e$message), class = "cmhc_error")
      }
    )

    last_warns <- warns

    # Hard error from the call
    if (inherits(result, "cmhc_error")) {
      if (is_retryable_msg(result$message) && attempt <= max_retries) {
        delay <- base_delay * (2^(attempt - 1)) + stats::runif(1, 0, 1)
        message(sprintf(
          "%s: error (%s), attempt %d/%d, retry in %.0fs...",
          label,
          result$message,
          attempt,
          max_retries + 1,
          delay
        ))
        Sys.sleep(delay)
        next
      }
      # All retries exhausted — signal transient failure vs genuine no-data
      if (is_transient_msg(result$message)) {
        stop(sprintf("[cmhc_transient] %s: %s", label, result$message))
      }
      warning(sprintf("%s: %s", label, result$message))
      return(NULL)
    }

    # Got data — success
    if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
      # Re-emit any non-retryable warnings (informational)
      for (w in warns) {
        if (!is_retryable_msg(w)) warning(w, call. = FALSE)
      }
      return(result)
    }

    # "Problem reading response" + NULL = genuinely no data (not transient).
    # CMHC returns this for geo units with no survey coverage. No retry needed.
    nodata_warns <- Filter(
      function(w) grepl("problem reading response", tolower(w), fixed = TRUE),
      warns
    )
    if (length(nodata_warns) > 0) {
      return(NULL)
    }

    # NULL or empty result — check if any warning is retryable
    retryable_warns <- Filter(is_retryable_msg, warns)

    if (length(retryable_warns) > 0 && attempt <= max_retries) {
      delay <- base_delay * (2^(attempt - 1)) + stats::runif(1, 0, 1)
      message(sprintf(
        "%s: %s, attempt %d/%d, retry in %.0fs...",
        label,
        retryable_warns[1],
        attempt,
        max_retries + 1,
        delay
      ))
      Sys.sleep(delay)
      next
    }

    # All retries exhausted — check if the last failure was transient
    transient_warns <- Filter(is_transient_msg, warns)
    if (length(transient_warns) > 0) {
      stop(sprintf("[cmhc_transient] %s: %s", label, transient_warns[1]))
    }

    # Non-retryable or exhausted retries — re-emit warnings and return NULL
    for (w in warns) {
      warning(w, call. = FALSE)
    }
    return(NULL)
  }
  NULL
}

cmhc_fetch_cma_data <- function(survey, series, dimension, geo_uid, frequency) {
  if (missing(frequency)) {
    stop(
      "Argument `frequency` is required and must be either 'Monthly' or 'Annual'."
    )
  }

  cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Historical Time Periods",
        geo_uid = geo_uid,
        frequency = frequency
      )
    },
    label = sprintf("CMA %s", geo_uid)
  )
}

#' Clean and standardize variable or column names
#'
#' This function takes a character vector and applies a series of transformations
#' to produce clean, consistent, and R-friendly names. It is useful for harmonizing
#' raw column names from external data sources (e.g., Statistics Canada, CSV files).
#' @param name_vector A character vector of names to be cleaned.
#' @return A character vector of cleaned and standardized names (lowercase, no special characters).
cmhc_clean_names <- function(name_vector) {
  name_vector |>
    stringr::str_trim() |> # Remove leading/trailing whitespace
    stringr::str_to_lower() |> # Convert to lowercase
    stringr::str_replace_all("[^a-z0-9_ ]", "_") |> # Replace non-alphanumeric characters with "_"
    stringr::str_replace_all("\\s+", "_") |> # Replace spaces with "_"
    stringr::str_replace_all("_+", "_") |> # Collapse multiple underscores into one
    stringr::str_replace_all("_$", "") # Remove trailing underscore if present
}


#' Clean CMHC Results (CMA Annual, Monthly, or CT)
#'
#' Cleans CMHC results in annual or monthly format for CMA data (with `DateString`)
#' or CT data (with `Year` and `Month`). Adds the `year_month` column, standardizes
#' the dimension values, and prepares the data frame for reshaping.
#'
#' @param df A raw `data.frame` returned by the CMHC API.
#' @param geo_uid Optional. Geographic identifier used in warning messages.
#'
#' @return A cleaned `data.frame` ready for reshaping, or `NULL` if the input is invalid.
cmhc_clean_results <- function(df, geo_uid = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Format CMA avec DateString (annual ou monthly)
  if ("DateString" %in% names(df)) {
    # Detect annual vs monthly by checking if multiple distinct months exist
    month_str <- stringr::str_extract(df$DateString, "[A-Za-z]+")
    month_str <- tolower(month_str[!is.na(month_str)])
    is_annual <- length(unique(month_str)) <= 1
    if (is_annual) {
      df$year <- stringr::str_extract(df$DateString, "\\d{4}")
      df$year_month <- df$year
      df <- df[, !(names(df) %in% c("year"))]
    } else {
      # Monthly format: "Jan 2021" or "2021 Jan"
      year_month_vec <- character(nrow(df))
      is_valid_monthly <- logical(nrow(df))
      for (i in seq_len(nrow(df))) {
        x <- trimws(df$DateString[i])
        if (grepl("^[0-9]{4} [A-Za-z]+$", x)) {
          parts <- strsplit(x, " ")[[1]]
          year <- parts[1]
          month <- parts[2]
        } else if (grepl("^[A-Za-z]+ [0-9]{4}$", x)) {
          parts <- strsplit(x, " ")[[1]]
          month <- parts[1]
          year <- parts[2]
        } else {
          next
        }
        month_num <- match(tolower(substr(month, 1, 3)), tolower(month.abb))
        if (!is.na(month_num)) {
          year_month_vec[i] <- sprintf("%s%02d", year, month_num)
          is_valid_monthly[i] <- TRUE
        }
      }
      if (!any(is_valid_monthly)) {
        warning(sprintf(
          "CMA %s: no valid monthly format detected. Skipping entry.",
          geo_uid
        ))
        return(NULL)
      }
      ym_valid <- year_month_vec[is_valid_monthly]
      months_only <- substr(ym_valid, 5, 6)
      if (length(unique(months_only)) == 1) {
        warning(sprintf(
          "CMA %s: likely annual format (only month %s detected). Skipping entry.",
          geo_uid,
          unique(months_only)
        ))
        return(NULL)
      }
      df$year_month <- year_month_vec
      df <- df[is_valid_monthly, ]
    }
  } else if (all(c("Year", "GeoUID") %in% names(df))) {
    # Format CT avec Year et Month — keep NA values (surveyed area, no data)
    if ("Month" %in% names(df) && !all(is.na(df$Month))) {
      df$year_month <- sprintf(
        "%d%02d",
        as.integer(df$Year),
        as.integer(df$Month)
      )
    } else {
      df$year_month <- as.character(df$Year)
    }
  } else {
    return(NULL)
  }

  base_cols <- c(
    "GeoUID",
    "Date",
    "DateString",
    "year_month",
    "year",
    "Year",
    "Month",
    "Value",
    "Survey",
    "Series",
    "Quality",
    "Census geography"
  )

  dimension_cols <- setdiff(colnames(df), base_cols)

  if (length(dimension_cols) > 0) {
    for (col in dimension_cols) {
      df[[col]] <- cmhc_clean_names(as.character(df[[col]]))
    }
  }

  keep_cols <- c("GeoUID", "year_month", "Value", "Survey", "Series", "Quality")
  keep_cols <- keep_cols[keep_cols %in% names(df)]

  df <- df |>
    dplyr::select(
      dplyr::all_of(keep_cols),
      tidyselect::all_of(dimension_cols)
    ) |>
    dplyr::filter(!is.na(GeoUID))

  df <- df[!grepl("NA$", df$GeoUID), ]

  return(df)
}


#' CMHC series known to be reported as percentages (e.g. 3.7 for 3.7%).
#' Values are divided by 100 during reshape so the DB stores proportions.
CMHC_PCT_SERIES <- c(
  "Vacancy Rate",
  "Availability Rate",
  "% Change of Average Rent",
  "% of Absorbed Units at Completion (Homeowner + Condo)"
)

#' Reshape CMHC Results to Long Format
#'
#' Converts cleaned CMHC data to long format (GeoUID, time, value), grouping by
#' `Series` and optionally by a breakdown dimension. Series listed in
#' `CMHC_PCT_SERIES` are automatically divided by 100.
#'
#' @param df A cleaned `data.frame` containing at least `GeoUID`, `Series`, `year_month`, and `Value`.
#' @param dimension Character. Optional name of a breakdown column (e.g., "Bedroom Type").
#' @return A named list of long-format data frames with columns (GeoUID, time, value).
cmhc_reshape_results <- function(df, dimension) {
  results_list <- list()

  if (!is.null(df) && nrow(df) > 0) {
    for (s in unique(df$Series)) {
      is_pct <- s %in% CMHC_PCT_SERIES

      if (!is.null(dimension) && dimension %in% names(df)) {
        for (d in sort(unique(df[[dimension]]))) {
          d_clean <- cmhc_clean_names(d)

          df_filtered <- df |>
            dplyr::filter(Series == s, !!rlang::sym(dimension) == d) |>
            dplyr::transmute(
              GeoUID,
              time = year_month,
              value = suppressWarnings(as.numeric(Value))
            ) |>
            dplyr::distinct()

          if (is_pct) {
            df_filtered$value <- df_filtered$value / 100
          }

          list_name <- cmhc_clean_names(paste(s, dimension, d_clean, sep = "_"))
          results_list[[list_name]] <- df_filtered
        }
      } else {
        df_filtered <- df |>
          dplyr::filter(Series == s) |>
          dplyr::transmute(
            GeoUID,
            time = year_month,
            value = suppressWarnings(as.numeric(Value))
          ) |>
          dplyr::distinct()

        if (is_pct) {
          df_filtered$value <- df_filtered$value / 100
        }

        list_name <- cmhc_clean_names(s)
        results_list[[list_name]] <- df_filtered
      }
    }
  }

  return(results_list)
}

#' Retrieve Annual CMHC Data for All CMAs
#'
#' Loops over all CMAs and request configurations to fetch, clean, reshape,
#' and aggregate CMHC data with annual frequency.
#'
#' @param requests A list of request objects. Each must include `survey`, `series`, and `dimension`.
#' @return A named list under `$CMA` containing wide-format CMHC data for all valid entries.
#' @export
cmhc_get_annual_cma <- function(requests, cma_uids = NULL, output_dir = NULL) {
  if (is.null(cma_uids)) {
    cma_uids <- cmhc_get_geo_uids("cma")
  }
  tasks <- cmhc_build_tasks(cma_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]
    frequency <- if (!is.null(req$frequency)) req$frequency else "Annual"

    raw <- cmhc_fetch_cma_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # METHODOLOGICAL NOTE — geo_vintage = "2021" for all years
    #
    # We query the CMHC API using 2021-vintage geo UIDs (from our
    # geography.cma / geography.csd tables).  CMHC returns data keyed by
    # the *requested* ID across all time periods — the API does not
    # re-code IDs to match the census vintage that was current at the
    # time of the survey.  Although the CMHC portal occasionally notes
    # that older CSD data is "based on 1996 Census Geography Definitions",
    # the API response always uses the UID we sent.
    #
    # Assigning a year-based vintage (e.g. 1998 → "1996") caused upload
    # failures because those 2021 UIDs don't exist in older geography
    # vintages in our database.  Since the IDs are always 2021 IDs,
    # geo_vintage must be "2021".
    #
    # CT data is the exception: CMHC returns a "Census geography" column
    # in CT responses, so cmhc_get_annual_ct / cmhc_get_monthly_ct read
    # the vintage directly from the API response.
    lapply(reshaped, function(df) {
      df[["geo_vintage"]] <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_cma_data = cmhc_fetch_cma_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_annual_cma"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CMA = cmhc_finalize_results(collected))
}


#' Extract Year-Month from DateString
#'
#' Parses the `DateString` column in raw CMHC monthly data to create a `year_month` column in `YYYYMM` format.
#' Filters out rows that do not follow valid monthly formatting.
#'
#' @param df A `data.frame` containing a `DateString` column.
#' @param geo_uid Optional. Used in warnings to identify the CMA.
#'
#' @return A filtered `data.frame` with a `year_month` column, or `NULL` if no valid rows are found.
cmhc_add_year_month <- function(df, geo_uid = NULL) {
  if (!"DateString" %in% colnames(df)) {
    stop("The column 'DateString' is required.")
  }

  year_month_vec <- character(nrow(df))
  is_valid_monthly <- logical(nrow(df))

  for (i in seq_len(nrow(df))) {
    x <- trimws(df$DateString[i])
    if (grepl("^[0-9]{4} [A-Za-z]+$", x)) {
      parts <- strsplit(x, " ")[[1]]
      year <- parts[1]
      month <- parts[2]
    } else if (grepl("^[A-Za-z]+ [0-9]{4}$", x)) {
      parts <- strsplit(x, " ")[[1]]
      month <- parts[1]
      year <- parts[2]
    } else {
      next
    }

    month_num <- match(tolower(substr(month, 1, 3)), tolower(month.abb))
    if (!is.na(month_num)) {
      year_month_vec[i] <- sprintf("%s%02d", year, month_num)
      is_valid_monthly[i] <- TRUE
    }
  }

  if (!any(is_valid_monthly)) {
    warning(sprintf(
      "CMA %s: no valid monthly format detected. Skipping entry.",
      geo_uid
    ))
    return(NULL)
  }

  ym_valid <- year_month_vec[is_valid_monthly]
  months_only <- substr(ym_valid, 5, 6)

  if (length(unique(months_only)) == 1) {
    warning(sprintf(
      "CMA %s: likely annual format (only month %s detected). Skipping entry.",
      geo_uid,
      unique(months_only)
    ))
    return(NULL)
  }

  df$year_month <- year_month_vec
  df <- df[is_valid_monthly, ]
  return(df)
}

#' Retrieve Monthly CMHC Data for All CMAs
#'
#' Loops through all CMAs to fetch, clean, reshape, and aggregate monthly CMHC data.
#' Filters out invalid or annual-format results.
#'
#' @param requests A list of request objects. Each must include `survey`, `series`, and optionally `dimension` and `frequency`.
#'
#' @return A named list under `$CMA` containing reshaped CMHC monthly data.
#' @export
cmhc_get_monthly_cma <- function(requests, cma_uids = NULL, output_dir = NULL) {
  if (is.null(cma_uids)) {
    cma_uids <- cmhc_get_geo_uids("cma")
  }
  tasks <- cmhc_build_tasks(cma_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]
    frequency <- if (!is.null(req$frequency)) req$frequency else "Monthly"

    raw <- cmhc_fetch_cma_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    if (!"Series" %in% colnames(raw)) {
      raw$Series <- req$series
    }
    if (!"Survey" %in% colnames(raw)) {
      raw$Survey <- req$survey
    }

    raw <- cmhc_add_year_month(raw, geo_uid = geo_uid)
    if (is.null(raw)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(raw, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      df[["geo_vintage"]] <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_cma_data = cmhc_fetch_cma_data,
      cmhc_add_year_month = cmhc_add_year_month,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_monthly_cma"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CMA = cmhc_finalize_results(collected))
}


#' Fetch CMHC Data for a Specific CT
#'
#' Sends a request to the CMHC API to retrieve data for a specific Census Tract (CT),
#' given the survey, series, dimension, year, and optionally the month.
#' Catches errors and returns `NULL` if the API call fails.
#'
#' @param survey Character. CMHC survey identifier (e.g., "Rms", "Scss").
#' @param series Character. Data series to retrieve (e.g., "Starts", "Vacancy Rate").
#' @param dimension Character. Optional breakdown dimension (e.g., "Bedroom Type"). Can be `NULL`.
#' @param geo_uid Character. Geographic identifier for the Census Tract (CT).
#' @param year Integer. Year of the data request.
#' @param month Integer or NULL. Optional month (1–12). If `NULL`, annual data will be fetched.
#'
#' @return A `data.frame` returned by the CMHC API, or `NULL` if an error occurs.
cmhc_fetch_ct_data <- function(
  survey,
  series,
  dimension,
  geo_uid,
  year,
  month = NULL
) {
  cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Census Tracts",
        geo_uid = geo_uid,
        year = year,
        month = if (!is.null(month)) {
          sprintf("%02d", as.integer(month))
        } else {
          NULL
        }
      )
    },
    label = sprintf("CT %s (%s-%s)", geo_uid, year, month %||% "--")
  )
}

#' Fetch Historical Time Series for an Individual CT
#'
#' Uses `breakdown = "Historical Time Periods"` to retrieve all available
#' years for one Census Tract in a single API call — the same endpoint used
#' for CMA and CSD historical queries. The CMHC API accepts CT UIDs in the
#' short format (e.g. "9330010.02") and returns a `DateString`-format
#' response (identical to CMA historical).
#'
#' @param survey Character. CMHC survey code (e.g., "Rms").
#' @param series Character. Series to extract (e.g., "Vacancy Rate").
#' @param dimension Character. Breakdown dimension (e.g., "Bedroom Type").
#' @param geo_uid Character. CT UID in short format (e.g. "9330010.02").
#' @param frequency Character. "Annual" or "Monthly".
#'
#' @return A raw CMHC data.frame, or NULL if the CT has no data.
cmhc_fetch_ct_historical_data <- function(
  survey,
  series,
  dimension,
  geo_uid,
  frequency = "Annual"
) {
  cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Historical Time Periods",
        geo_uid = geo_uid,
        frequency = frequency
      )
    },
    label = sprintf("CT-hist %s", geo_uid)
  )
}


#' Retrieve Annual CMHC Data for All CTs (Historical Per-CT)
#'
#' Downloads and reshapes annual CMHC Census Tract (CT) data using
#' historical time series per individual CT (one call per CT, all years
#' returned). CT UIDs come from CMHC's ArcGIS layer 0 via
#' \code{cmhc_get_arcgis_ct_uids()}.
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`.
#' @param ct_uids A character vector of CT UIDs in CMHC short format (e.g. "9330010.02").
#' @param output_dir Optional. Directory (or S3 prefix) for caching per-task .qs files.
#'
#' @return A named list `CT` containing reshaped data frames for each variable.
#' @export
cmhc_get_annual_ct <- function(requests, ct_uids, output_dir = NULL) {
  # CT historical: one API call per CT returns ALL years (~6k calls total).
  # Uses breakdown = "Historical Time Periods" — same endpoint as CMA/CSD.
  # Response format has DateString (CMA-style), so we clean with
  # cmhc_clean_results(), NOT cmhc_clean_results_csd().
  # No expand_years — each task is one CT × one request.
  tasks <- cmhc_build_tasks(ct_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]
    frequency <- if (!is.null(req$frequency)) req$frequency else "Annual"

    # Fast pre-check: list_cmhc_periods returns instantly for invalid CTs
    # (HTTP 500 in ~0.1s) vs get_cmhc which hangs ~37s on invalid CTs.
    has_data <- tryCatch(
      {
        cmhc::list_cmhc_periods(
          survey = req$survey,
          series = req$series,
          dimension = req$dimension,
          breakdown = "Census Tracts",
          geo_uid = geo_uid
        )
        TRUE
      },
      error = function(e) {
        # 503/502/429 = transient server error — propagate so we don't cache as empty
        msg <- tolower(conditionMessage(e))
        if (grepl("status: 50[23]|status: 429", msg)) {
          stop(e)
        }
        FALSE
      }
    )
    if (!has_data) {
      return(NULL)
    }

    raw <- cmhc_fetch_ct_historical_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_ct_historical_data = cmhc_fetch_ct_historical_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_annual_ct"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CT = cmhc_finalize_results(collected))
}


#' Retrieve Monthly CMHC Data for All CTs (Historical Per-CT)
#'
#' Downloads and reshapes monthly CMHC Census Tract (CT) data using
#' historical time series per individual CT (one call per CT, all months
#' returned).
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`.
#' @param ct_uids A character vector of CT UIDs in CMHC short format (e.g. "9330010.02").
#' @param output_dir Optional. Directory (or S3 prefix) for caching per-task .qs files.
#'
#' @return A named list `CT` containing reshaped data frames for each variable.
#' @export
cmhc_get_monthly_ct <- function(requests, ct_uids, output_dir = NULL) {
  # Historical per-CT: one call returns all months. No expand_years/months.
  tasks <- cmhc_build_tasks(ct_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]

    # Fast pre-check: list_cmhc_periods returns instantly for invalid CTs
    # (HTTP 500 in ~0.1s) vs get_cmhc which hangs ~37s on invalid CTs.
    has_data <- tryCatch(
      {
        cmhc::list_cmhc_periods(
          survey = req$survey,
          series = req$series,
          dimension = req$dimension,
          breakdown = "Census Tracts",
          geo_uid = geo_uid
        )
        TRUE
      },
      error = function(e) {
        msg <- tolower(conditionMessage(e))
        if (grepl("status: 50[23]|status: 429", msg)) {
          stop(e)
        }
        FALSE
      }
    )
    if (!has_data) {
      return(NULL)
    }

    raw <- cmhc_fetch_ct_historical_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency = "Monthly"
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_ct_historical_data = cmhc_fetch_ct_historical_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_monthly_ct"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CT = cmhc_finalize_results(collected))
}


#' Retrieve Annual CT Data via Per-CMA Per-Year Queries
#'
#' Instead of querying each CT individually with Historical Time Periods
#' (which only returns data for 2021 CT boundaries), this function queries
#' each CMA for each year using `breakdown = "Census Tracts"`. This returns
#' ALL CTs that existed in that CMA for that year's census geography,
#' capturing historical CT boundaries that no longer exist in 2021.
#'
#' @param requests A list of request objects. Each must include `survey`,
#'   `series`, `dimension`, and `years` (character vector of years to query).
#' @param cma_uids A character vector of CMA UIDs.
#' @param output_dir Optional. Directory (or S3 prefix) for caching per-task
#'   .qs files. Each file is named `{cma_uid}_{year}.qs`.
#'
#' @details
#' **geo_vintage mapping:** The census geography used by CMHC depends on
#' the data year:
#' \itemize{
#'   \item Years 2011--2015: geo_vintage = "2011" (2011 census boundaries)
#'   \item Years 2016--2020: geo_vintage = "2016" (2016 census boundaries)
#'   \item Years 2021+: geo_vintage = "2021" (2021 census boundaries)
#' }
#'
#' @return A named list `CT` containing reshaped data frames for each variable.
#' @export
cmhc_get_ct_by_cma <- function(requests, cma_uids, output_dir = NULL) {
  tasks <- cmhc_build_tasks(cma_uids, requests, expand_years = TRUE)

  .worker <- function(geo_uid, req_idx, year) {
    req <- requests[[req_idx]]

    raw <- cmhc_fetch_ct_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid = geo_uid,
      year = year
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # Derive geo_vintage from year: CMHC uses the census geography that was
    # current at the time of the survey.  Minimum vintage is "1996" — for
    # pre-1996 years we also assign "1996".
    yr <- as.integer(year)
    geo_vintage <- if (yr >= 2021) {
      "2021"
    } else if (yr >= 2016) {
      "2016"
    } else if (yr >= 2011) {
      "2011"
    } else if (yr >= 2006) {
      "2006"
    } else if (yr >= 2001) {
      "2001"
    } else {
      "1996"
    }

    lapply(reshaped, \(df) {
      df$geo_vintage <- geo_vintage
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_ct_data = cmhc_fetch_ct_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_ct_by_cma"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CT = cmhc_finalize_results(collected))
}


#' Retrieve Monthly CT Data via Per-CMA Per-Year-Month Queries
#'
#' Like \code{cmhc_get_ct_by_cma()} but expands over both years AND months.
#' Queries each CMA for each year-month combination using
#' \code{breakdown = "Census Tracts"}, returning all CTs within that CMA.
#'
#' @param requests A list of request objects. Each must include \code{survey},
#'   \code{series}, \code{dimension}, \code{years} (character vector), and
#'   \code{months} (character vector, e.g. \code{as.character(1:12)}).
#' @param cma_uids A character vector of CMA UIDs.
#' @param output_dir Optional. Directory (or S3 prefix) for caching per-task
#'   .qs files. Each file is named \code{{cma_uid}_{year}_{month}.qs}.
#'
#' @return A named list \code{CT} containing reshaped data frames for each
#'   variable, with monthly \code{time} values (e.g. "202110").
#' @export
cmhc_get_monthly_ct_by_cma <- function(requests, cma_uids, output_dir = NULL) {
  tasks <- cmhc_build_tasks(
    cma_uids, requests,
    expand_years = TRUE, expand_months = TRUE
  )

  .worker <- function(geo_uid, req_idx, year, month) {
    req <- requests[[req_idx]]

    raw <- cmhc_fetch_ct_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid = geo_uid,
      year = year,
      month = month
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # Derive geo_vintage from year
    yr <- as.integer(year)
    geo_vintage <- if (yr >= 2021) {
      "2021"
    } else if (yr >= 2016) {
      "2016"
    } else if (yr >= 2011) {
      "2011"
    } else if (yr >= 2006) {
      "2006"
    } else if (yr >= 2001) {
      "2001"
    } else {
      "1996"
    }

    lapply(reshaped, \(df) {
      df$geo_vintage <- geo_vintage
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_ct_data = cmhc_fetch_ct_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_monthly_ct_by_cma"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CT = cmhc_finalize_results(collected))
}


#' Fetch CMHC Data for a Specific CSD
#'
#' Sends a request to the CMHC API to retrieve data for a specific Census Subdivision (CSD),
#' given the survey, series, dimension, year, and optionally month.
#' Automatically adds the `GeoUID` column since it is missing in API results for CSDs.
#'
#' @param survey Character. CMHC survey identifier (e.g., "Rms", "Scss").
#' @param series Character. Data series to retrieve (e.g., "Starts", "Vacancy Rate").
#' @param dimension Character. Optional breakdown dimension (e.g., "Dwelling Type"). Can be `NULL`.
#' @param geo_uid Character. Geographic identifier for the Census Subdivision (CSD).
#' @param year Integer. Year of the data request.
#' @param month Integer or NULL. Optional month (1–12). If `NULL`, retrieves annual data.
#'
#' @return A `data.frame` with results and an added `GeoUID` column, or `NULL` on error.
cmhc_fetch_csd_data <- function(
  survey,
  series,
  dimension,
  geo_uid,
  year,
  month = NULL
) {
  df <- cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Census Subdivision",
        geo_uid = geo_uid,
        year = year,
        month = if (!is.null(month)) {
          sprintf("%02d", as.integer(month))
        } else {
          NULL
        }
      )
    },
    label = sprintf("CSD %s (%s-%s)", geo_uid, year, month %||% "--")
  )

  if (!is.null(df) && nrow(df) > 0) {
    df$GeoUID <- geo_uid
  }

  df
}

#' Fetch Historical Time Series for an Individual CSD
#'
#' Uses `breakdown = "Historical Time Periods"` to retrieve all available
#' years for one CSD in a single API call — the same endpoint used for CMA
#' historical queries. The CMHC API recognises CSD-length geo_uids and
#' returns a `DateString`-format response (identical to CMA historical).
#'
#' @param survey Character. CMHC survey code (e.g., "Rms").
#' @param series Character. Series to extract (e.g., "Vacancy Rate").
#' @param dimension Character. Breakdown dimension (e.g., "Bedroom Type").
#' @param geo_uid Character. CSD GeoUID (7-digit, e.g. "2465005").
#' @param frequency Character. "Annual" or "Monthly".
#'
#' @return A raw CMHC data.frame, or NULL if the CSD has no data.
cmhc_fetch_csd_historical_data <- function(
  survey,
  series,
  dimension,
  geo_uid,
  frequency = "Annual"
) {
  cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Historical Time Periods",
        geo_uid = geo_uid,
        frequency = frequency
      )
    },
    label = sprintf("CSD-hist %s", geo_uid)
  )
}

#' Clean CMHC Results for Census Subdivisions (CSD)
#'
#' Cleans and standardizes CMHC data at the Census Subdivision (CSD) level by:
#' - Detecting whether the data is annual or monthly based on the presence or variation of `Month` or `Date`
#' - Generating a `year_month` column from `Year` and `Month`, or parsing it from `Date`
#' - Standardizing values in the `Dimension` column
#' - Keeping only relevant columns required for reshaping
#'
#' @param df A raw `data.frame` returned by `cmhc_fetch_csd_data()`, which must include at minimum the `GeoUID` column.
#'
#' @return A cleaned `data.frame` ready for reshaping, or `NULL` if the input is invalid.
cmhc_clean_results_csd <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  if (!"GeoUID" %in% names(df)) {
    stop("Missing 'GeoUID' column. Ensure it was added manually.")
  }

  # Création de year_month à partir de Date ou Year + Month
  if ("Date" %in% names(df)) {
    months_unique <- unique(format(df$Date, "%m"))
    if (length(months_unique) == 1) {
      df$year_month <- format(df$Date, "%Y")
    } else {
      df$year_month <- format(df$Date, "%Y%m")
    }
  } else if (all(c("Year", "Month") %in% names(df))) {
    unique_months <- unique(df$Month)
    if (length(unique_months) == 1) {
      df$year_month <- as.character(df$Year)
    } else {
      df$year_month <- sprintf(
        "%d%02d",
        as.integer(df$Year),
        as.integer(df$Month)
      )
    }
  } else if ("Year" %in% names(df)) {
    df$year_month <- as.character(df$Year)
  } else {
    stop("Cannot derive 'year_month' from the data.")
  }

  # Identifier les colonnes de dimensions
  base_cols <- c(
    "GeoUID",
    "year_month",
    "Value",
    "Survey",
    "Series",
    "Date",
    "Year",
    "Month",
    "Census Subdivision"
  )
  dimension_cols <- setdiff(names(df), base_cols)

  # Nettoyage des colonnes de dimension
  if (length(dimension_cols) > 0) {
    for (col in dimension_cols) {
      df[[col]] <- cmhc_clean_names(as.character(df[[col]]))
    }
  }

  # Garder les colonnes essentielles
  keep_cols <- c("GeoUID", "year_month", "Value", "Survey", "Series")
  keep_cols <- keep_cols[keep_cols %in% names(df)]

  df_clean <- df |>
    dplyr::select(
      dplyr::all_of(keep_cols),
      tidyselect::all_of(dimension_cols)
    ) |>
    dplyr::filter(!is.na(GeoUID))

  return(df_clean)
}
#' Finalize CMHC reshaped result
#'
#' Cleans up the final reshaped CMHC data frame:
#' - Removes join artifacts (e.g., Year.x, Year.y)
#' - Sorts columns by time
#' - Keeps 'GeoUID' first
#'
#' @param reshaped_list A list of reshaped data.frames.
#' @return A cleaned list of data.frames.
cmhc_finalize_results <- function(reshaped_list) {
  reshaped_list <- Filter(
    function(df) !is.null(df) && is.data.frame(df) && nrow(df) > 0,
    reshaped_list
  )

  purrr::map(reshaped_list, function(df) {
    if ("GeoUID" %in% names(df)) {
      df <- dplyr::rename(df, id = GeoUID)
    } else if ("geouid" %in% names(df)) {
      df <- dplyr::rename(df, id = geouid)
    }
    if ("id" %in% names(df)) {
      df$id <- as.character(df$id)
    }
    df
  })
}

#' Build a self-contained environment for mirai daemon serialization
#'
#' When a closure is serialized for mirai, package-namespace functions resolve
#' to the INSTALLED version on the daemon, not the `load_all()` dev version.
#' This helper creates an environment that contains all needed objects and
#' rebinds every function's enclosing environment so internal references
#' (e.g. `cmhc_clean_names`, `CMHC_PCT_SERIES`) resolve from the environment
#' rather than the installed namespace.
#'
#' @param ... Named objects (functions, data, constants) the worker needs.
#' @return An environment with parent `globalenv()`.
cmhc_make_par_env <- function(...) {
  objs <- list(...)
  env <- list2env(objs, parent = globalenv())
  for (nm in names(objs)) {
    if (is.function(env[[nm]])) {
      environment(env[[nm]]) <- env
    }
  }
  env
}

#' Check whether mirai daemons are active
#'
#' Returns TRUE if mirai is available and daemons are running, FALSE otherwise.
#' Used internally to decide between parallel (mirai_map) and sequential (lapply)
#' execution paths.
#'
#' @return Logical.
cmhc_has_daemons <- function() {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    return(FALSE)
  }
  s <- tryCatch(mirai::status(), error = function(e) NULL)
  if (is.null(s)) {
    return(FALSE)
  }
  isTRUE(s$connections > 0L)
}

#' Build a flat task dataframe for parallel CMHC processing
#'
#' Creates one row per independent unit of work from all combinations of
#' geographic IDs and request parameters. Used with `mirai::mirai_map()`.
#'
#' Get geographic UIDs from the database (no geometry loaded)
#'
#' Queries the `geography.<geo_level>` table for distinct IDs at the 2021
#' vintage. This avoids loading full shapefiles just to get a list of UIDs.
#'
#' For CMA, combines both `geography.cma` and `geography.cmasplit`, then
#' removes the short (3-digit) merged cross-provincial IDs so that only
#' province-qualified CMAPUID values remain.
#'
#' @param geo_level Character. One of `"cma"`, `"csd"`, `"ct"`, etc.
#' @param vintage Character. Census vintage (default `"2021"`).
#' @return Character vector of unique geographic IDs.
#' @export
cmhc_get_geo_uids <- function(geo_level, vintage = "2021") {
  conn <- cc.pipe::db_connect()
  on.exit(DBI::dbDisconnect(conn))

  get_ids <- function(tbl) {
    sql <- sprintf(
      "SELECT DISTINCT id FROM geography.%s WHERE vintage = $1",
      DBI::dbQuoteIdentifier(conn, tbl)
    )
    as.character(DBI::dbGetQuery(conn, sql, params = list(vintage))$id)
  }

  if (geo_level == "cma") {
    # Combine cma + cmasplit, drop short (3-digit) merged cross-provincial IDs
    ids <- unique(c(get_ids("cma"), get_ids("cmasplit")))
    ids <- ids[nchar(ids) > 3]
    return(sort(ids))
  }

  sort(get_ids(geo_level))
}

#' Get CSD UIDs with CMHC Rental Market Survey coverage
#'
#' Queries CMHC's ArcGIS geospatial service (layer 3) to retrieve the set of
#' Census Subdivisions that have Rms survey data, then filters against our
#' geography.csd table to keep only CSDs we store.
#'
#' @param vintage Character. Census vintage to filter against (default "2021").
#' @return Sorted character vector of CSD UIDs.
#' @export
cmhc_get_arcgis_csd_uids <- function(vintage = "2021") {
  url <- paste0(
    "https://geospatial.cmhc-schl.gc.ca/server/rest/services/",
    "CMHC_APPS/HMIP_CURRENT_CAWD/MapServer/3/query"
  )
  # sf = TRUE works reliably; we just drop geometry afterwards
  raw <- arcgis_rest_services_ret(url, sf = TRUE)
  csd_uids <- sort(unique(as.character(raw$CSDUID)))

  # Filter against our DB
  conn <- cc.pipe::db_connect()
  on.exit(DBI::dbDisconnect(conn))
  db_ids <- DBI::dbGetQuery(
    conn,
    "SELECT DISTINCT id FROM geography.csd WHERE vintage = $1",
    params = list(vintage)
  )$id
  sort(csd_uids[csd_uids %in% db_ids])
}

#' Get CMA UIDs with CMHC Rental Market Survey coverage
#'
#' Queries CMHC's ArcGIS geospatial service (layer 4) to retrieve the set of
#' "centres" (CMAs + CAs) that have Rms survey data. UID is constructed as
#' \code{paste0(PRUID, sgc_cma_ca_cde)}. Results are filtered against our
#' geography.cma + geography.cmasplit tables to drop CAs we don't store.
#'
#' @param vintage Character. Census vintage to filter against (default "2021").
#' @return Sorted character vector of CMA UIDs (including cmasplit UIDs like "24505").
#' @export
cmhc_get_arcgis_cma_uids <- function(vintage = "2021") {
  url <- paste0(
    "https://geospatial.cmhc-schl.gc.ca/server/rest/services/",
    "CMHC_APPS/HMIP_CURRENT_CAWD/MapServer/4/query"
  )
  # sf = TRUE works reliably; we just drop geometry afterwards
  raw <- arcgis_rest_services_ret(url, sf = TRUE)
  # UID = province code + 3-digit CMA/CA code (e.g. "24462", "35535")
  centre_uids <- sort(unique(paste0(raw$PRUID, raw$sgc_cma_ca_cde)))

  # Filter against our DB (union of cma + cmasplit, >3 chars)
  db_cma_ids <- cmhc_get_geo_uids("cma", vintage = vintage)
  sort(centre_uids[centre_uids %in% db_cma_ids])
}

#' Get CT UIDs with CMHC Rental Market Survey coverage
#'
#' Queries CMHC's ArcGIS geospatial service (layer 0) to retrieve the set of
#' Census Tracts. Returns a data.frame with two UID formats:
#' \itemize{
#'   \item \code{query_uid}: \code{paste0(sgc_cma_ca_cde, sgc_census_tract_cde)}
#'     — short format for \code{get_cmhc()} calls (e.g. \code{"4620402.00"})
#'   \item \code{validation_uid}: \code{paste0(metropolitan_major_area_cde,
#'     neighbourhood_cde, sgc_census_tract_cde)} — long format for the CMHC
#'     validation endpoint (e.g. \code{"54006759007.00"})
#' }
#'
#' @param vintage Character. Census vintage to filter against (default "2021").
#' @return A data.frame with columns \code{query_uid} and \code{validation_uid},
#'   sorted by \code{query_uid}, deduplicated.
#' @export
cmhc_get_arcgis_ct_uids <- function(vintage = "2021") {
  url <- paste0(
    "https://geospatial.cmhc-schl.gc.ca/server/rest/services/",
    "CMHC_APPS/HMIP_CURRENT_CAWD/MapServer/0/query"
  )
  # sf = TRUE works reliably; we just drop geometry afterwards
  raw <- arcgis_rest_services_ret(url, sf = TRUE)

  # Short format for get_cmhc(): sgc_cma_ca_cde + sgc_census_tract_cde
  # e.g. "462" + "0402.00" -> "4620402.00"
  cma_cde <- as.character(raw$sgc_cma_ca_cde)
  ct_cde <- as.character(raw$sgc_census_tract_cde)
  # Long format for validation POST: metropolitan_major_area_cde +
  # neighbourhood_cde + sgc_census_tract_cde
  # e.g. "540" + "067" + "59007.00" -> "54006759007.00"
  metro_cde <- as.character(raw$metropolitan_major_area_cde)
  nbhd_cde <- as.character(raw$neighbourhood_cde)

  keep <- !is.na(cma_cde) &
    !is.na(ct_cde) &
    nchar(cma_cde) > 0 &
    nchar(ct_cde) > 0 &
    !is.na(metro_cde) &
    !is.na(nbhd_cde) &
    nchar(metro_cde) > 0 &
    nchar(nbhd_cde) > 0

  query_uid <- paste0(cma_cde[keep], ct_cde[keep])
  validation_uid <- paste0(metro_cde[keep], nbhd_cde[keep], ct_cde[keep])

  # Drop malformed UIDs — valid query UIDs have 7 digits before the decimal
  # (3-digit CMA code + 4-digit tract code).
  valid <- grepl("^\\d{7}\\.\\d{2}$", query_uid)
  out <- data.frame(
    query_uid = query_uid[valid],
    validation_uid = validation_uid[valid],
    stringsAsFactors = FALSE
  )
  out <- out[!duplicated(out$query_uid), ]
  out[order(out$query_uid), ]
}

#' Validate CT UIDs against CMHC's CensusTract endpoint
#'
#' POSTs each validation UID to CMHC's CensusTract endpoint. CTs with no data
#' at all (across all surveys) return HTTP 500 instantly, while valid CTs
#' return 200. This is much faster than letting \code{get_cmhc()} time out
#' (~37s per invalid UID).
#'
#' Results are cached to S3 (when \code{options(cmhc.s3_bucket)} is set) at
#' \code{cmhc/validated_ct_uids.qs} so the validation only runs once.
#' Runs sequentially on the main process — the CMHC server rejects concurrent
#' connections, so parallel validation produces false negatives.
#'
#' @param ct_df A data.frame with columns \code{query_uid} and
#'   \code{validation_uid} (as returned by \code{cmhc_get_arcgis_ct_uids()}).
#' @param refresh Logical. If TRUE, ignore cached results and re-validate.
#' @return The input data.frame filtered to only CTs that have data.
#' @export
cmhc_validate_ct_uids <- function(ct_df, refresh = FALSE) {
  cache_key <- "cmhc/validated_ct_uids.qs"
  use_s3 <- !is.null(.cmhc_s3_bucket())

  # Try loading from cache
  if (!refresh && use_s3) {
    cached <- tryCatch(.cmhc_s3_read_qs(cache_key), error = function(e) NULL)
    if (!is.null(cached) && is.data.frame(cached) && nrow(cached) > 0) {
      out <- ct_df[ct_df$query_uid %in% cached$query_uid, ]
      message(sprintf(
        "[cmhc_validate_ct_uids] Loaded %d valid CTs from S3 cache (%d input).",
        nrow(out),
        nrow(ct_df)
      ))
      return(out)
    }
  }

  base_url <- "https://www03.cmhc-schl.gc.ca/hmip-pimh/en/TableMapChart/CensusTract"
  n <- nrow(ct_df)
  message(sprintf(
    "[cmhc_validate_ct_uids] Validating %d CTs sequentially...",
    n
  ))

  # Single-UID check: POST with form body (matches browser behaviour)
  .check_ct <- function(validation_uid) {
    resp <- httr::POST(
      base_url,
      body = list(id = validation_uid, t = "CensusTract"),
      encode = "form"
    )
    httr::status_code(resp)
  }

  # Probe first UID to verify the endpoint works
  probe_uid <- ct_df$validation_uid[1]
  probe_status <- tryCatch(.check_ct(probe_uid), error = function(e) {
    message("  Probe error: ", conditionMessage(e))
    NA_integer_
  })
  message(sprintf(
    "  Probe: validation_uid='%s' → HTTP %s",
    probe_uid,
    ifelse(is.na(probe_status), "ERROR", probe_status)
  ))
  if (!is.na(probe_status) && probe_status == 500) {
    # If probe also 500, try a known-good UID pattern to verify endpoint works
    message(
      "  Probe returned 500. Testing with hardcoded known UID '24101000048.01'..."
    )
    test_status <- tryCatch(.check_ct("24101000048.01"), error = function(e) {
      NA_integer_
    })
    message(sprintf(
      "  Hardcoded test → HTTP %s",
      ifelse(is.na(test_status), "ERROR", test_status)
    ))
    if (!is.na(test_status) && test_status == 500) {
      message(
        "  WARNING: Even known-good UID returns 500. Endpoint may be down or POST format wrong."
      )
    }
  }

  p <- progressr::progressor(steps = n)
  is_valid <- logical(n)
  for (i in seq_len(n)) {
    p(message = sprintf("%d/%d", i, n))
    status <- NA_integer_
    for (attempt in 1:3) {
      status <- tryCatch(
        .check_ct(ct_df$validation_uid[i]),
        error = function(e) NA_integer_
      )
      if (!is.na(status)) {
        break
      }
      Sys.sleep(1)
    }
    if (is.na(status)) {
      warning(
        sprintf(
          "CT %s: all retries failed, assuming valid",
          ct_df$validation_uid[i]
        ),
        call. = FALSE
      )
      is_valid[i] <- TRUE
    } else {
      is_valid[i] <- status != 500
    }
    # Log first 5 results for debugging
    if (i <= 5) {
      message(sprintf(
        "  [%d] uid=%s → %d → valid=%s",
        i,
        ct_df$validation_uid[i],
        status,
        is_valid[i]
      ))
    }
  }

  n_dropped <- sum(!is_valid)
  result <- ct_df[is_valid, ]
  message(sprintf(
    "[cmhc_validate_ct_uids] %d/%d CTs valid, %d dropped.",
    nrow(result),
    n,
    n_dropped
  ))

  # Cache to S3
  if (use_s3 && nrow(result) > 0) {
    creds <- .cmhc_s3_creds()
    raw <- qs::qserialize(result, preset = "fast")
    aws.s3::put_object(
      what = raw,
      object = cache_key,
      bucket = .cmhc_s3_bucket(),
      region = creds$region,
      key = creds$key,
      secret = creds$secret
    )
    message(sprintf(
      "[cmhc_validate_ct_uids] Cached to s3://%s/%s",
      .cmhc_s3_bucket(),
      cache_key
    ))
  }

  result
}

#' Build a descriptive filename from a single task row
#'
#' Encodes geo_uid, req_idx, and optionally year/month into the filename so
#' cached files are human-readable and individually addressable.
#'
#' @param task_row A one-row data.frame with columns from `cmhc_build_tasks()`.
#' @return A character string like `"24462_2021.qs"` or `"24462_vr_bedroom_2021.qs"`.
cmhc_task_filename <- function(task_row) {
  parts <- as.character(task_row$geo_uid)
  # Only include req_tag when there are multiple requests (i.e. tag differs)
  if ("req_tag" %in% names(task_row) && nchar(task_row$req_tag) > 0) {
    parts <- paste0(parts, "_", task_row$req_tag)
  }
  if ("year" %in% names(task_row)) {
    parts <- paste0(parts, "_", task_row$year)
  }
  if ("month" %in% names(task_row)) {
    parts <- paste0(parts, "_m", sprintf("%02d", as.integer(task_row$month)))
  }
  paste0(parts, ".qs")
}

#' Build a short tag from a request's series and dimension
#'
#' Used in file naming so cached files are human-readable.
#' E.g. series="Vacancy Rate", dimension="Bedroom Type" -> "vr_bedroom"
#' @param req A request list with `series` and optionally `dimension`.
#' @return A short snake_case tag string.
cmhc_req_tag <- function(req) {
  # Abbreviate common series names
  abbrevs <- c(
    "Vacancy Rate" = "vr",
    "Average Rent" = "avgrent",
    "Median Rent" = "medrent",
    "Rental Universe" = "universe",
    "Starts" = "starts",
    "Completions" = "completions",
    "Under Construction" = "undercx",
    "Absorbed Units" = "absorbed",
    "Unabsorbed Units" = "unabsorbed",
    "Length of Construction" = "lencx",
    "Starts SAAR" = "starts_saar"
  )
  s <- if (req$series %in% names(abbrevs)) {
    abbrevs[[req$series]]
  } else {
    tolower(gsub("[^a-zA-Z0-9]+", "_", req$series))
  }
  # Abbreviate dimension
  if (is.null(req$dimension) || is.na(req$dimension)) {
    return(s)
  }
  dim_abbrevs <- c(
    "Bedroom Type" = "bedroom",
    "Year of Construction" = "yoc",
    "Structure Size" = "struct",
    "Intended Market" = "market",
    "Dwelling Type" = "dwelling",
    "Price Range" = "price"
  )
  d <- if (req$dimension %in% names(dim_abbrevs)) {
    dim_abbrevs[[req$dimension]]
  } else {
    tolower(gsub("[^a-zA-Z0-9]+", "_", req$dimension))
  }
  paste0(s, "_", d)
}

#' Build a task grid for CMHC data fetching
#'
#' Constructs a data.frame of all (geo_uid x request) combinations to process,
#' optionally expanded across years and months.
#'
#' @param geo_uids Character vector of geographic identifiers.
#' @param requests List of request objects.
#' @param expand_years Logical. Cross with `req$years` if TRUE.
#' @param expand_months Logical. Cross with `req$months` if TRUE.
#' @return A data.frame with columns `geo_uid`, `req_idx`, `req_tag`,
#'   and optionally `year`, `month`.
cmhc_build_tasks <- function(
  geo_uids,
  requests,
  expand_years = FALSE,
  expand_months = FALSE
) {
  tags <- vapply(requests, cmhc_req_tag, character(1))
  # If only one request, no need for tag in filename (dir already encodes it)
  if (length(unique(tags)) == 1) {
    tags <- rep("", length(tags))
  }

  tasks_list <- lapply(seq_along(requests), function(i) {
    grids <- list(geo_uid = geo_uids, req_idx = i)
    if (expand_years) {
      grids$year <- requests[[i]]$years
    }
    if (expand_months) {
      grids$month <- requests[[i]]$months
    }
    df <- do.call(
      expand.grid,
      c(grids, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
    )
    df$req_tag <- tags[i]
    df
  })
  do.call(rbind, tasks_list)
}

#' Collect mirai_map results, check for errors, and combine by variable key
#'
#' Each mirai result is expected to be either NULL or a named list of data.frames
#' (one per variable). Results are combined via `bind_rows` per variable key.
#'
#' @param mp A mirai_map promise object.
#' @return A named list of combined data.frames (one per variable key).
cmhc_collect_results <- function(mp) {
  res <- mp[.progress]

  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    stop(
      sprintf(
        "mirai_map had %d failure(s). First failure:\nindex=%d\n%s",
        length(idx),
        idx[1],
        as.character(res[[idx[1]]])
      ),
      call. = FALSE
    )
  }

  results_list <- list()
  for (x in res) {
    if (is.null(x)) {
      next
    }
    for (key in setdiff(names(x), ".debug")) {
      val <- x[[key]]
      if (is.null(val) || !is.data.frame(val) || nrow(val) == 0) {
        next
      }
      results_list[[key]] <- dplyr::bind_rows(results_list[[key]], val)
    }
  }

  results_list
}

#' Collect sequential (non-mirai) results and combine by variable key
#'
#' Equivalent to `cmhc_collect_results` but for plain lists produced by `lapply`.
#'
#' @param res A list of worker results (each NULL or a named list of data.frames).
#' @return A named list of combined data.frames (one per variable key).
cmhc_collect_results_seq <- function(res) {
  results_list <- list()

  for (x in res) {
    if (is.null(x)) {
      next
    }
    for (key in setdiff(names(x), ".debug")) {
      val <- x[[key]]
      if (is.null(val) || !is.data.frame(val) || nrow(val) == 0) {
        next
      }
      results_list[[key]] <- dplyr::bind_rows(results_list[[key]], val)
    }
  }
  results_list
}

#' Read a single variable from a directory (or S3 prefix) of per-task .qs files
#'
#' Each task file is a named list of data.frames (one per variable key).
#' This function reads files one at a time, extracts only the requested
#' variable, and combines the rows.  Peak RAM is one task file plus the
#' accumulated rows for that single variable.
#'
#' When `options(cmhc.s3_bucket)` is set, `dir` is treated as an S3 key
#' prefix and files are streamed from the bucket.
#'
#' @param dir Directory containing `.qs` task files, or S3 key prefix.
#' @param variable Character. The variable key to extract (e.g.
#'   `"vacancy_rate_bedroom_type_total"`).
#' @param geo_level Character or NULL. When `"ct"`, IDs are validated against
#'   `geography.ct` per `geo_vintage` and rows with unknown IDs are dropped.
#'   Pass `NULL` (default) to skip validation.
#' @return A single data.frame (finalized: `id` column, character), or
#'   `NULL` if the variable is not found in any file.
#' @export
cmhc_read_variable <- function(
  dir,
  variable,
  min_file_size = 69L,
  geo_level = NULL
) {
  use_s3 <- !is.null(.cmhc_s3_bucket())

  if (use_s3) {
    info <- .cmhc_s3_list_qs_info(dir)
    if (nrow(info) == 0) {
      warning("No .qs files found in s3://", .cmhc_s3_bucket(), "/", dir)
      return(NULL)
    }
    # Skip files that are too small to contain real data (empty results)
    keys <- sort(info$key[info$size >= min_file_size])
    n_skipped <- nrow(info) - length(keys)
    if (n_skipped > 0) {
      cat(sprintf(
        "  [%s] Skipping %d/%d empty files (< %d bytes)\n",
        basename(dir),
        n_skipped,
        nrow(info),
        min_file_size
      ))
    }
    if (length(keys) == 0) {
      return(NULL)
    }
    acc <- NULL
    for (k in keys) {
      x <- .cmhc_s3_read_qs(k)
      if (is.null(x)) {
        next
      }
      val <- x[[variable]]
      if (is.null(val) || !is.data.frame(val) || nrow(val) == 0) {
        next
      }
      acc <- dplyr::bind_rows(acc, val)
    }
  } else {
    files <- sort(list.files(dir, pattern = "\\.qs$", full.names = TRUE))
    if (length(files) == 0) {
      warning("No .qs files found in ", dir)
      return(NULL)
    }
    acc <- NULL
    for (f in files) {
      x <- qs::qread(f)
      if (is.null(x)) {
        next
      }
      val <- x[[variable]]
      if (is.null(val) || !is.data.frame(val) || nrow(val) == 0) {
        next
      }
      acc <- dplyr::bind_rows(acc, val)
    }
  }

  if (is.null(acc)) {
    return(NULL)
  }
  # Finalize (rename GeoUID -> id, coerce to character)
  if ("GeoUID" %in% names(acc)) {
    acc <- dplyr::rename(acc, id = GeoUID)
  } else if ("geouid" %in% names(acc)) {
    acc <- dplyr::rename(acc, id = geouid)
  }
  if ("id" %in% names(acc)) {
    acc$id <- as.character(acc$id)
  }

  if (!is.null(geo_level) && "geo_vintage" %in% names(acc)) {
    conn <- cc.pipe::db_connect()
    on.exit(DBI::dbDisconnect(conn))
    vintages <- unique(acc$geo_vintage)

    # Query valid IDs per vintage (used by both expansion and validation)
    geo_tbl <- DBI::dbQuoteIdentifier(conn, geo_level)
    valid_by_vintage <- stats::setNames(
      lapply(vintages, \(v) {
        DBI::dbGetQuery(
          conn,
          sprintf(
            "SELECT DISTINCT id FROM geography.%s WHERE vintage = $1",
            geo_tbl
          ),
          params = list(v)
        )$id
      }),
      vintages
    )

    # --- Expand to full CT coverage per CMA per vintage ---
    # CMHC omits CTs with zero data entirely. Supplement them with NA rows
    # so "no data" is distinguishable from "CT doesn't exist in vintage".
    if (geo_level == "ct") {
      cma_codes_present <- unique(substr(acc$id, 1, 3))
      times_by_vintage <- lapply(
        split(acc$time, acc$geo_vintage),
        unique
      )

      fill_rows <- list()
      for (v in vintages) {
        db_cts_v <- valid_by_vintage[[v]]
        # Only supplement CTs in CMAs that already appear in the data
        db_cts_v <- db_cts_v[substr(db_cts_v, 1, 3) %in% cma_codes_present]
        existing_cts <- unique(acc$id[acc$geo_vintage == v])
        missing_cts <- setdiff(db_cts_v, existing_cts)

        if (length(missing_cts) > 0) {
          times_v <- times_by_vintage[[v]]
          fill_rows[[v]] <- data.frame(
            id = rep(missing_cts, each = length(times_v)),
            time = rep(times_v, times = length(missing_cts)),
            value = NA_real_,
            geo_vintage = v,
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(fill_rows) > 0) {
        fill_df <- do.call(rbind, fill_rows)
        message(sprintf(
          "  [cmhc_read_variable] Supplemented %d NA rows for CTs absent from CMHC response",
          nrow(fill_df)
        ))
        acc <- dplyr::bind_rows(acc, fill_df)
      }
    }

    # Drop rows whose ID doesn't exist in geography.{geo_level} for that
    # row's specific geo_vintage.  CMHC sometimes returns codes that StatCan
    # never assigned (e.g. internal provisional codes), which would cause
    # upload failures.
    n_before <- nrow(acc)
    valid_keys <- unlist(lapply(names(valid_by_vintage), \(v) {
      paste0(valid_by_vintage[[v]], "|", v)
    }))
    row_keys <- paste0(acc$id, "|", acc$geo_vintage)
    acc <- acc[row_keys %in% valid_keys, , drop = FALSE]
    n_dropped <- n_before - nrow(acc)
    if (n_dropped > 0) {
      message(sprintf(
        "  [cmhc_read_variable] Dropped %d rows with IDs absent from geography.%s for their geo_vintage",
        n_dropped,
        geo_level
      ))
    }
  }

  if (nrow(acc) == 0) {
    return(NULL)
  }
  acc
}

#' List variable keys available in a directory (or S3 prefix) of per-task .qs files
#'
#' Scans a sample of task files to discover which variable keys are present.
#' Transparently uses S3 when `options(cmhc.s3_bucket)` is set.
#'
#' @param dir Directory containing `.qs` task files, or S3 key prefix.
#' @param n_sample Number of files to sample (default 20). Set to `Inf` to
#'   scan all files.
#' @return Character vector of unique variable keys.
#' @export
cmhc_list_variables <- function(dir, n_sample = 20) {
  use_s3 <- !is.null(.cmhc_s3_bucket())

  if (use_s3) {
    all_keys <- sort(.cmhc_s3_list_qs(dir))
    if (length(all_keys) == 0) {
      return(character(0))
    }
    if (n_sample < length(all_keys)) {
      all_keys <- all_keys[seq_len(n_sample)]
    }
    keys <- character(0)
    for (k in all_keys) {
      x <- .cmhc_s3_read_qs(k)
      if (!is.null(x)) keys <- union(keys, setdiff(names(x), ".debug"))
    }
  } else {
    files <- sort(list.files(dir, pattern = "\\.qs$", full.names = TRUE))
    if (length(files) == 0) {
      return(character(0))
    }
    if (n_sample < length(files)) {
      files <- files[seq_len(n_sample)]
    }
    keys <- character(0)
    for (f in files) {
      x <- qs::qread(f)
      if (!is.null(x)) keys <- union(keys, setdiff(names(x), ".debug"))
    }
  }
  keys
}

# =========================================================================
# S3 helpers — used when options(cmhc.s3_bucket = "bucket_name") is set.
# When set, output_dir / dir arguments are treated as S3 key prefixes
# instead of local filesystem paths.
# =========================================================================

#' @keywords internal
.cmhc_s3_bucket <- function() getOption("cmhc.s3_bucket")

#' @keywords internal
.cmhc_s3_creds <- function() {
  list(
    region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
    key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
    secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY")
  )
}

#' @keywords internal
.cmhc_s3_list_qs <- function(prefix, bucket = .cmhc_s3_bucket()) {
  info <- .cmhc_s3_list_qs_info(prefix, bucket)
  info$key
}

#' @keywords internal
.cmhc_s3_list_qs_info <- function(prefix, bucket = .cmhc_s3_bucket()) {
  creds <- .cmhc_s3_creds()
  pfx <- sub("/*$", "/", prefix) # ensure trailing /
  objects <- aws.s3::get_bucket(
    bucket = bucket,
    prefix = pfx,
    max = Inf,
    region = creds$region,
    key = creds$key,
    secret = creds$secret
  )
  keys <- vapply(objects, `[[`, "", "Key")
  sizes <- as.integer(sapply(objects, \(x) x$Size))
  is_qs <- grepl("\\.qs$", keys)
  data.frame(key = keys[is_qs], size = sizes[is_qs], stringsAsFactors = FALSE)
}

#' @keywords internal
.cmhc_s3_read_qs <- function(key, bucket = .cmhc_s3_bucket()) {
  creds <- .cmhc_s3_creds()
  tmp <- tempfile(fileext = ".qs")
  on.exit(unlink(tmp), add = TRUE)
  aws.s3::save_object(
    object = key,
    bucket = bucket,
    file = tmp,
    region = creds$region,
    key = creds$key,
    secret = creds$secret,
    overwrite = TRUE
  ) |>
    suppressMessages()
  qs::qread(tmp)
}

#' List .qs cache files (local or S3)
#'
#' Returns basenames of `.qs` files in a cache directory.
#' Transparently uses S3 when `options(cmhc.s3_bucket)` is set.
#'
#' @param dir Local directory path or S3 key prefix.
#' @return Character vector of `.qs` basenames (e.g. `"10001_2021.qs"`).
#' @export
cmhc_cache_files <- function(dir) {
  if (!is.null(.cmhc_s3_bucket())) {
    basename(.cmhc_s3_list_qs(dir))
  } else {
    list.files(dir, pattern = "\\.qs$")
  }
}

# =========================================================================

#' Run CMHC tasks and collect results (parallel or sequential, RAM or disk)
#'
#' Common execution logic shared by all `cmhc_get_*` functions. Handles the
#' parallel vs sequential decision, the optional disk-save mode (`output_dir`),
#' and the final combine step.
#'
#' When `output_dir` is set, each task result is saved as a small `.qs` file
#' (to local disk or S3 when `options(cmhc.s3_bucket)` is set) and the function
#' returns the path/prefix — nothing is accumulated in RAM.
#' Use `cmhc_read_variable(dir, key)` afterwards to read one variable at a time.
#'
#' @param tasks A data.frame of tasks (one row per unit of work) as produced by
#'   `cmhc_build_tasks()`. Column names must match `.worker`'s formals.
#' @param .worker The worker function (closure).
#' @param par_env_objs A named list of objects for `cmhc_make_par_env()`.
#' @param output_dir Optional directory (or S3 prefix) to save per-task `.qs`
#'   files. When `NULL`, results are accumulated in RAM.
#' @param label A short string for progress messages (e.g. `"cmhc_get_annual_ct"`).
#' @return When `output_dir` is `NULL`: a named list of combined data.frames.
#'   When `output_dir` is set: the directory path (character).
cmhc_run_and_collect <- function(
  tasks,
  .worker,
  par_env_objs,
  output_dir = NULL,
  label = "cmhc"
) {
  # ------------------------------------------------------------------
  # Disk / S3 path: workers save results, return NULL — nothing in RAM
  # Supports both parallel (mirai) and sequential.
  # ------------------------------------------------------------------
  if (!is.null(output_dir)) {
    use_s3 <- !is.null(.cmhc_s3_bucket())

    if (use_s3) {
      s3_bucket <- .cmhc_s3_bucket()
      s3_creds <- .cmhc_s3_creds()
      existing_basenames <- basename(.cmhc_s3_list_qs(output_dir, s3_bucket))
    } else {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Pre-compute filenames and filter out already-cached tasks
    all_fns <- vapply(
      seq_len(nrow(tasks)),
      function(i) {
        cmhc_task_filename(tasks[i, , drop = FALSE])
      },
      character(1)
    )
    if (use_s3) {
      already <- all_fns %in% existing_basenames
    } else {
      already <- file.exists(file.path(output_dir, all_fns))
    }
    n_done <- sum(already)
    todo_idx <- which(!already)
    cat(sprintf(
      "[%s] %d tasks, %d cached, %d to fetch\n",
      label,
      nrow(tasks),
      n_done,
      length(todo_idx)
    ))

    if (length(todo_idx) == 0) {
      return(output_dir)
    }

    todo_tasks <- tasks[todo_idx, , drop = FALSE]
    todo_fns <- all_fns[todo_idx]

    # Drop metadata columns that are not worker arguments
    todo_tasks$req_tag <- NULL

    use_parallel <- cmhc_has_daemons()

    if (use_parallel) {
      # Add disk/S3-saving deps to the parallel environment
      par_env_objs$.disk_worker <- .worker
      if (use_s3) {
        # S3 writer: references .s3_bucket etc. from par_env after
        # cmhc_make_par_env() re-binds all function environments.
        par_env_objs$qs_qsave <- function(x, key) {
          raw <- qs::qserialize(x, preset = "fast")
          aws.s3::put_object(
            what = raw,
            object = key,
            bucket = .s3_bucket,
            region = .s3_region,
            key = .s3_key,
            secret = .s3_secret
          )
        }
        par_env_objs$.s3_bucket <- s3_bucket
        par_env_objs$.s3_region <- s3_creds$region
        par_env_objs$.s3_key <- s3_creds$key
        par_env_objs$.s3_secret <- s3_creds$secret
      } else {
        par_env_objs$qs_qsave <- function(x, file) qs::qsave(x, file)
      }
      par_env <- do.call(cmhc_make_par_env, par_env_objs)

      # Wrapped worker: runs on daemon, saves to disk.
      # Returns a compact status list:
      #   s = "saved"  — data written to disk
      #   s = "nodata" — CT genuinely has no data, empty sentinel saved
      #   s = "transient" — server overloaded (503/429/timeout), NOT cached
      #   s = "error"  — unexpected error
      .par_disk_worker <- function(..., .out_dir, .out_fn) {
        warns <- character(0)
        result <- tryCatch(
          withCallingHandlers(
            .disk_worker(...),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            structure(e$message, class = "worker_error")
          }
        )
        wmsg <- if (length(warns) > 0) paste(warns, collapse = "; ") else NULL
        if (inherits(result, "worker_error")) {
          msg <- as.character(result)
          # Transient server error — do NOT cache, will retry next run
          if (grepl("\\[cmhc_transient\\]", msg)) {
            return(list(
              s = "transient",
              w = paste(c(msg, wmsg), collapse = "; ")
            ))
          }
          return(list(
            s = "error",
            w = paste(c(msg, wmsg), collapse = "; ")
          ))
        }
        if (!is.null(result)) {
          qs_qsave(result, file.path(.out_dir, .out_fn))
          return(list(s = "saved", w = wmsg))
        }
        # Genuinely no data — save empty sentinel so we skip on re-run
        qs_qsave(data.frame(), file.path(.out_dir, .out_fn))
        list(s = "nodata", w = wmsg)
      }
      environment(.par_disk_worker) <- par_env

      # Append .out_dir and .out_fn columns so mirai passes them as args
      todo_tasks$.out_dir <- output_dir
      todo_tasks$.out_fn <- todo_fns

      # --- Chunked dispatch to avoid OOM on large task sets ---
      chunk_size <- 500L
      n_total <- nrow(todo_tasks)
      n_chunks <- ceiling(n_total / chunk_size)
      total_mirai_err <- 0L
      total_worker_err <- 0L
      total_transient <- 0L
      total_saved <- 0L
      total_nodata <- 0L
      all_reasons <- character(0) # unique warning/error messages

      for (ch in seq_len(n_chunks)) {
        start_i <- (ch - 1L) * chunk_size + 1L
        end_i <- min(ch * chunk_size, n_total)
        chunk_rows <- start_i:end_i

        mp <- mirai::mirai_map(
          todo_tasks[chunk_rows, , drop = FALSE],
          .par_disk_worker
        )
        res <- mp[.progress]

        ch_saved <- 0L
        ch_nodata <- 0L
        ch_err <- 0L
        ch_mirai <- 0L
        ch_transient <- 0L

        for (r in res) {
          if (mirai::is_error_value(r)) {
            ch_mirai <- ch_mirai + 1L
            all_reasons <- unique(c(
              all_reasons,
              paste0("[mirai] ", as.character(r))
            ))
          } else if (is.list(r) && !is.null(r$s)) {
            if (r$s == "saved") {
              ch_saved <- ch_saved + 1L
            } else if (r$s == "transient") {
              ch_transient <- ch_transient + 1L
              if (!is.null(r$w)) all_reasons <- unique(c(all_reasons, r$w))
            } else if (r$s == "error") {
              ch_err <- ch_err + 1L
              if (!is.null(r$w)) all_reasons <- unique(c(all_reasons, r$w))
            } else {
              ch_nodata <- ch_nodata + 1L
              if (!is.null(r$w)) all_reasons <- unique(c(all_reasons, r$w))
            }
          } else {
            ch_nodata <- ch_nodata + 1L
          }
        }

        total_mirai_err <- total_mirai_err + ch_mirai
        total_worker_err <- total_worker_err + ch_err
        total_transient <- total_transient + ch_transient
        total_saved <- total_saved + ch_saved
        total_nodata <- total_nodata + ch_nodata

        cat(sprintf(
          "  [%s] chunk %d/%d — %d saved, %d empty, %d transient, %d err (total: %d/%d)\n",
          label,
          ch,
          n_chunks,
          ch_saved,
          ch_nodata,
          ch_transient,
          ch_err + ch_mirai,
          total_saved + n_done,
          nrow(tasks)
        ))

        # If transient errors are piling up, server is overloaded — abort early
        if (ch_transient > chunk_size * 0.5) {
          warning(
            sprintf(
              "[%s] >50%% transient errors in chunk %d — server overloaded, stopping early. Re-run to retry.",
              label,
              ch
            ),
            call. = FALSE
          )
          break
        }

        # After first chunk, show the reasons we're seeing (if any)
        if (ch == 1 && length(all_reasons) > 0) {
          cat(sprintf(
            "    sample reasons: %s\n",
            paste(head(all_reasons, 3), collapse = "\n                    ")
          ))
        }

        rm(res, mp)
        gc()
      }

      # Final summary
      cat(sprintf(
        "  [%s] DONE — %d saved, %d empty, %d transient (will retry), %d errors, %d cached\n",
        label,
        total_saved,
        total_nodata,
        total_transient,
        total_mirai_err + total_worker_err,
        n_done
      ))
      if (length(all_reasons) > 0) {
        show <- head(all_reasons, 10)
        cat(sprintf(
          "  [%s] unique reasons (%d):\n    %s\n",
          label,
          length(all_reasons),
          paste(show, collapse = "\n    ")
        ))
      }
    } else {
      # Sequential disk path — use todo_tasks (req_tag already stripped)
      n_transient <- 0L
      for (j in seq_len(nrow(todo_tasks))) {
        if (j %% 50 == 1 || j == nrow(todo_tasks)) {
          cat(sprintf(
            "  [%s] %d/%d\n",
            label,
            j,
            nrow(todo_tasks)
          ))
        }
        is_transient <- FALSE
        result <- tryCatch(
          do.call(.worker, as.list(todo_tasks[j, , drop = FALSE])),
          error = function(e) {
            if (grepl("\\[cmhc_transient\\]", e$message)) {
              is_transient <<- TRUE
              warning(
                sprintf(
                  "[%s] %s: %s (transient, will retry next run)",
                  label,
                  todo_fns[j],
                  e$message
                ),
                call. = FALSE
              )
            } else {
              warning(
                sprintf("[%s] %s: %s", label, todo_fns[j], e$message),
                call. = FALSE
              )
            }
            NULL
          }
        )
        # Transient errors (503/429/timeout) — do NOT cache, retry next run
        if (is_transient) {
          n_transient <- n_transient + 1L
          if (n_transient > 10) {
            warning(
              sprintf(
                "[%s] >10 consecutive transient errors — server overloaded, stopping.",
                label
              ),
              call. = FALSE
            )
            break
          }
          next
        }
        n_transient <- 0L # reset on non-transient
        s3_key <- file.path(output_dir, todo_fns[j])
        # Save result (or empty sentinel for nodata so we skip on re-run)
        out <- if (!is.null(result)) result else data.frame()
        if (use_s3) {
          raw <- qs::qserialize(out, preset = "fast")
          aws.s3::put_object(
            what = raw,
            object = s3_key,
            bucket = s3_bucket,
            region = s3_creds$region,
            key = s3_creds$key,
            secret = s3_creds$secret
          )
        } else {
          qs::qsave(out, s3_key)
        }
      }
    }
    return(output_dir)
  }

  # ------------------------------------------------------------------
  # In-memory path: parallel if daemons available, else sequential
  # ------------------------------------------------------------------
  # Drop metadata columns that are not worker arguments
  tasks$req_tag <- NULL

  use_parallel <- cmhc_has_daemons()
  cat(sprintf("[%s] %d tasks, parallel=%s\n", label, nrow(tasks), use_parallel))

  if (use_parallel) {
    par_env <- do.call(cmhc_make_par_env, par_env_objs)
    environment(.worker) <- par_env
    mp <- mirai::mirai_map(tasks, .worker)
    cmhc_collect_results(mp)
  } else {
    n <- nrow(tasks)
    res <- lapply(seq_len(n), function(i) {
      if (i %% 50 == 1 || i == n) {
        cat(sprintf("  [%s] task %d/%d\n", label, i, n))
      }
      tryCatch(
        do.call(.worker, as.list(tasks[i, , drop = FALSE])),
        error = function(e) {
          warning(sprintf("[%s] task %d error: %s", label, i, e$message))
          NULL
        }
      )
    })
    cmhc_collect_results_seq(res)
  }
}

#' Downloads and reshapes annual CMHC Census Subdivision (CSD) data
#' for each year and CSD specified in the request list.
#' Data is harmonized to the 2021 geography using correspondence tables.
#'
#' @param requests A list of request objects. Each must include `survey`, `series`, `dimension`, and `years`.
#' @param csd_uids A character vector of CSD GeoUIDs to include.
#'
#' @return A named list `CSD` containing reshaped data frames for each variable.
#' @export
cmhc_get_annual_csd <- function(
  requests,
  csd_uids,
  output_dir = NULL
) {
  # CSD historical: one API call per CSD returns ALL years (~800 calls total).
  # Uses breakdown = "Historical Time Periods" — same endpoint as CMA.
  # Response format has DateString (CMA-style), so we clean with
  # cmhc_clean_results(), NOT cmhc_clean_results_csd().
  # No expand_years — each task is one CSD × one request.
  tasks <- cmhc_build_tasks(csd_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]
    frequency <- if (!is.null(req$frequency)) req$frequency else "Annual"

    raw <- cmhc_fetch_csd_historical_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_csd_historical_data = cmhc_fetch_csd_historical_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_annual_csd"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CSD = cmhc_finalize_results(collected))
}

#' Retrieve Monthly CMHC Data for All CSDs (Historical Per-CSD)
#'
#' Downloads and reshapes monthly CMHC Census Subdivision (CSD) data
#' using historical time series per individual CSD (one call per CSD,
#' all months returned).
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`.
#' @param csd_uids A vector of CSD GeoUIDs to include.
#'
#' @return A named list `CSD` containing reshaped data frames for each variable.
#' @export
cmhc_get_monthly_csd <- function(
  requests,
  csd_uids,
  output_dir = NULL
) {
  # Historical per-CSD: one call returns all months. No expand_years/months.
  tasks <- cmhc_build_tasks(csd_uids, requests)

  .worker <- function(geo_uid, req_idx) {
    req <- requests[[req_idx]]
    raw <- cmhc_fetch_csd_historical_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      frequency = "Monthly"
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_csd_historical_data = cmhc_fetch_csd_historical_data,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_monthly_csd"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(CSD = cmhc_finalize_results(collected))
}

#' Fetch CMHC Data for a Specific Survey Zone (Monthly or Annual)
#'
#' Downloads raw CMHC data for a given Survey Zone (CMA GeoUID) and time period.
#'
#' @param survey Character. The CMHC survey code (e.g., `"Scss"`).
#' @param series Character. The CMHC data series to retrieve (e.g., `"Starts"`).
#' @param dimension Character. The breakdown dimension (e.g., `"Dwelling Type"`).
#' @param geo_uid Character. The CMA GeoUID (e.g., `"24462"` for Montréal).
#' @param year Character or numeric. The year of the data (format `"YYYY"`).
#' @param month Character or numeric, optional. The month of the data (format `"MM"`). Use `NULL` for annual data.
#'
#' @return A `data.frame` (tibble) of raw CMHC results, or `NULL` in case of error.
cmhc_fetch_survey_zone_data <- function(
  survey,
  series,
  dimension,
  geo_uid,
  year,
  month = NULL
) {
  cmhc_with_retry(
    function() {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = "Survey Zones",
        geo_uid = geo_uid,
        year = year,
        month = month
      )
    },
    label = sprintf("SZ %s (%s-%s)", geo_uid, year, month %||% "--")
  )
}

#' Attach GeoUID to raw CMHC Survey Zone data
#'
#' Matches `Survey Zones` from raw CMHC data with official survey zone references using fuzzy string matching,
#' and attaches the corresponding `GeoUID` (`METZONE_UID`) for further processing.
#'
#' @param df A raw CMHC data.frame that must contain a column named `"Survey Zones"`.
#' @param survey_zones_ref A reference table (data.frame) with columns `"ZONE_NAME_EN"` and `"METZONE_UID"`.
#' @param max_dist Integer. Maximum Levenshtein distance allowed for fuzzy matching (default is 3).
#'
#' @return A `data.frame` with an added column `GeoUID`. Returns `NULL` if input is empty or unmatched.
cmhc_attach_zone_geouid <- function(df, survey_zones_ref, max_dist = 3) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  stopifnot(
    "Survey Zones" %in% names(df),
    all(c("ZONE_NAME_EN", "METZONE_UID") %in% colnames(survey_zones_ref))
  )

  df <- dplyr::filter(df, !is.na(`Survey Zones`))
  if (nrow(df) == 0) {
    return(NULL)
  }

  zone_names <- unique(df$`Survey Zones`)

  clean_zone_name <- function(name) {
    name |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[éèêë]", "e") |>
      stringr::str_replace_all("[àâä]", "a") |>
      stringr::str_replace_all("[ç]", "c") |>
      stringr::str_replace_all("[^a-z0-9]", "") |>
      stringr::str_trim()
  }

  input_df <- tibble::tibble(
    zone_raw = zone_names,
    zone_clean = clean_zone_name(zone_names)
  )
  ref_df <- dplyr::mutate(
    survey_zones_ref,
    zone_clean = clean_zone_name(ZONE_NAME_EN)
  )

  matched <- fuzzyjoin::stringdist_inner_join(
    input_df,
    ref_df,
    by = "zone_clean",
    method = "lv",
    max_dist = max_dist,
    distance_col = "dist"
  ) |>
    dplyr::group_by(zone_raw) |>
    dplyr::slice_min(order_by = dist, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(zone_raw, METZONE_UID)

  dplyr::left_join(df, matched, by = c("Survey Zones" = "zone_raw")) |>
    dplyr::rename(GeoUID = METZONE_UID)
}

#' Retrieve Annual CMHC Data for Survey Zones
#'
#' Downloads, cleans, reshapes, and merges annual CMHC data for survey zones associated with selected parent CMAs.
#' This function runs in parallel over all requested parent CMA GeoUIDs and returns data harmonized by zone and year.
#'
#' @param requests A list of request objects. Each must include the following elements:
#'   - `survey`: Character. The CMHC survey code (e.g., `"Scss"`).
#'   - `series`: Character. The data series (e.g., `"Starts"`).
#'   - `dimension`: Character. The breakdown dimension (e.g., `"Dwelling Type"`).
#'   - `years`: Integer vector. List of years to request (e.g., `2010:2024`).
#' @param survey_zones_ref A reference `data.frame` with columns `ZONE_NAME_EN` and `METZONE_UID`, used to attach unique `GeoUID` to each zone.
#'
#' @return A named list with element `survey_zone`, containing one cleaned `data.frame` per variable requested.
#'         Each data.frame has one row per survey zone (`id`), and one column per year (e.g., `starts_dwelling_type_all_2010`, ...).
#' @export
cmhc_get_annual_survey_zone <- function(
  requests,
  survey_zones_ref,
  output_dir = NULL
) {
  cma_uids <- cmhc_get_geo_uids("cma")
  tasks <- cmhc_build_tasks(cma_uids, requests, expand_years = TRUE)

  .worker <- function(geo_uid, req_idx, year) {
    req <- requests[[req_idx]]
    raw <- cmhc_fetch_survey_zone_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      year
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    raw <- cmhc_attach_zone_geouid(raw, survey_zones_ref)
    if (is.null(raw) || !"GeoUID" %in% names(raw)) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      survey_zones_ref = survey_zones_ref,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_survey_zone_data = cmhc_fetch_survey_zone_data,
      cmhc_attach_zone_geouid = cmhc_attach_zone_geouid,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_annual_survey_zone"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(survey_zone = cmhc_finalize_results(collected))
}

#' Retrieve Monthly CMHC Data for Survey Zones
#'
#' Downloads, cleans, reshapes, and merges monthly CMHC data for survey zones associated with selected parent CMAs.
#' Runs in parallel over all parent CMA GeoUIDs and time combinations.
#'
#' @param requests A list of request objects. Each must include the following elements:
#'   - `survey`: Character. The CMHC survey code (e.g., `"Scss"`).
#'   - `series`: Character. The data series (e.g., `"Starts"`).
#'   - `dimension`: Character. The breakdown dimension (e.g., `"Dwelling Type"`).
#'   - `years`: Integer vector. Years to request (e.g., `2015:2024`).
#'   - `months`: Character or integer vector of two-digit months (e.g., `sprintf("%02d", 1:12)`).
#' @param survey_zones_ref A reference `data.frame` with columns `ZONE_NAME_EN` and `METZONE_UID`, used to match and attach GeoUIDs.
#'
#' @return A named list with element `survey_zone`, containing one cleaned `data.frame` per variable requested.
#'         Each data.frame has one row per survey zone (`id`), and one column per month (e.g., `starts_dwelling_type_all_201501`).
#' @export
cmhc_get_monthly_survey_zone <- function(
  requests,
  survey_zones_ref,
  output_dir = NULL
) {
  cma_uids <- cmhc_get_geo_uids("cma")
  tasks <- cmhc_build_tasks(
    cma_uids,
    requests,
    expand_years = TRUE,
    expand_months = TRUE
  )

  .worker <- function(geo_uid, req_idx, year, month) {
    req <- requests[[req_idx]]
    raw <- cmhc_fetch_survey_zone_data(
      req$survey,
      req$series,
      req$dimension,
      geo_uid,
      year,
      month
    )
    if (is.null(raw) || nrow(raw) == 0) {
      return(NULL)
    }

    raw <- cmhc_attach_zone_geouid(raw, survey_zones_ref)
    if (is.null(raw) || !"GeoUID" %in% names(raw)) {
      return(NULL)
    }

    cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
    if (is.null(cleaned)) {
      return(NULL)
    }

    reshaped <- cmhc_reshape_results(cleaned, req$dimension)
    reshaped <- Filter(function(df) !is.null(df) && nrow(df) > 0, reshaped)
    if (length(reshaped) == 0) {
      return(NULL)
    }

    # geo_vintage = "2021" — see methodological note in cmhc_get_annual_cma.
    # We query with 2021 IDs; CMHC returns data on those same IDs for all years.
    lapply(reshaped, \(df) {
      df$geo_vintage <- "2021"
      df
    })
  }

  collected <- cmhc_run_and_collect(
    tasks,
    .worker,
    par_env_objs = list(
      requests = requests,
      survey_zones_ref = survey_zones_ref,
      cmhc_with_retry = cmhc_with_retry,
      cmhc_fetch_survey_zone_data = cmhc_fetch_survey_zone_data,
      cmhc_attach_zone_geouid = cmhc_attach_zone_geouid,
      cmhc_clean_results = cmhc_clean_results,
      cmhc_reshape_results = cmhc_reshape_results,
      cmhc_clean_names = cmhc_clean_names,
      CMHC_PCT_SERIES = CMHC_PCT_SERIES
    ),
    output_dir = output_dir,
    label = "cmhc_get_monthly_survey_zone"
  )
  if (is.character(collected)) {
    return(collected)
  }
  list(survey_zone = cmhc_finalize_results(collected))
}
