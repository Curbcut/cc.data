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
cmhc_fetch_cma_data <- function(survey, series, dimension, geo_uid, frequency) {
  if (missing(frequency)) {
    stop("Argument `frequency` is required and must be either 'Monthly' or 'Annual'.")
  }
  
  tryCatch(
    cmhc::get_cmhc(
      survey = survey,
      series = series,
      dimension = dimension,
      breakdown = "Historical Time Periods",
      geo_uid = geo_uid,
      frequency = frequency
    ),
    error = function(e) {
      warning(sprintf("CMA %s: error during get_cmhc() -> %s", geo_uid, e$message))
      return(NULL)
    }
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
    stringr::str_trim() |>                           # Remove leading/trailing whitespace
    stringr::str_to_lower() |>                       # Convert to lowercase
    stringr::str_replace_all("[^a-z0-9_ ]", "_") |>  # Replace non-alphanumeric characters with "_"
    stringr::str_replace_all("\\s+", "_") |>         # Replace spaces with "_"
    stringr::str_replace_all("_+", "_") |>           # Collapse multiple underscores into one
    stringr::str_replace_all("_$", "")               # Remove trailing underscore if present
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
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Format CMA avec DateString (annual ou monthly)
  if ("DateString" %in% names(df)) {
    if (all(grepl("^[0-9]{4}$", stringr::str_extract(df$DateString, "\\d{4}")))) {
      df$year <- stringr::str_extract(df$DateString, "\\d{4}")
      month_str <- stringr::str_extract(df$DateString, "[A-Za-z]+")
      month_str <- tolower(month_str[!is.na(month_str)])
      if (length(unique(month_str)) > 1) return(NULL)
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
          year <- parts[1]; month <- parts[2]
        } else if (grepl("^[A-Za-z]+ [0-9]{4}$", x)) {
          parts <- strsplit(x, " ")[[1]]
          month <- parts[1]; year <- parts[2]
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
        warning(sprintf("CMA %s: no valid monthly format detected. Skipping entry.", geo_uid))
        return(NULL)
      }
      ym_valid <- year_month_vec[is_valid_monthly]
      months_only <- substr(ym_valid, 5, 6)
      if (length(unique(months_only)) == 1) {
        warning(sprintf("CMA %s: likely annual format (only month %s detected). Skipping entry.", geo_uid, unique(months_only)))
        return(NULL)
      }
      df$year_month <- year_month_vec
      df <- df[is_valid_monthly, ]
    }
  }
  
  # Format CT avec Year et Month
  else if (all(c("Year", "GeoUID") %in% names(df))) {
    df <- df |> dplyr::filter(!is.na(Value))
    if ("Month" %in% names(df) && !all(is.na(df$Month))) {
      df$year_month <- sprintf("%d%02d", as.integer(df$Year), as.integer(df$Month))
    } else {
      df$year_month <- as.character(df$Year)
    }
  } else {
    return(NULL)
  }
  
  base_cols <- c(
    "GeoUID", "Date", "DateString", "year_month", "year", "Year", "Month",
    "Value", "Survey", "Series", "Quality", "Census geography"
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
    dplyr::select(dplyr::all_of(keep_cols), tidyselect::all_of(dimension_cols)) |>
    dplyr::filter(!is.na(GeoUID))
  
  df <- df[!grepl("NA$", df$GeoUID), ]

  return(df)
}


#' Reshape CMHC Results to Wide Format
#'
#' Converts long-format CMHC data to wide format, grouping by `Series`, `year_month`, and optionally by a breakdown dimension.
#'
#' @param df A cleaned `data.frame` containing at least `GeoUID`, `Series`, `year_month`, and `Value`.
#' @param dimension Character. Optional name of a breakdown column (e.g., "Bedroom Type").
#' @param series_prefix Logical. If `TRUE`, prefixes column names with the series name.
#' @return A named list of reshaped data frames.
cmhc_reshape_results <- function(df, dimension, series_prefix = TRUE) {
  results_list <- list()
  
  if (!is.null(df) && nrow(df) > 0) {
    for (s in unique(df$Series)) {
      if (!is.null(dimension) && dimension %in% names(df)) {
        for (d in sort(unique(df[[dimension]]))) {
          d_clean <- cmhc_clean_names(d)
          
          df_filtered <- df |>
            dplyr::filter(Series == s, !!rlang::sym(dimension) == d) |>
            dplyr::select(GeoUID, year_month, Value) |>
            dplyr::distinct() |>
            tidyr::pivot_wider(
              names_from = year_month,
              values_from = Value,
              names_prefix = if (series_prefix) {
                paste0(s, "_", dimension, "_", d_clean, "_")
              } else {
                paste0(dimension, "_", d_clean, "_")
              },
              values_fn = list(Value = ~ if (length(.) == 1) . else mean(., na.rm = TRUE)),
              values_fill = NA
            ) |>
            dplyr::filter(!is.na(GeoUID))
          
          colnames(df_filtered) <- cmhc_clean_names(colnames(df_filtered))
          list_name <- cmhc_clean_names(paste(s, dimension, d_clean, sep = "_"))
          results_list[[list_name]] <- df_filtered
        }
      } else {
        df_filtered <- df |>
          dplyr::filter(Series == s) |>
          dplyr::select(GeoUID, year_month, Value) |>
          dplyr::distinct() |>
          tidyr::pivot_wider(
            names_from = year_month,
            values_from = Value,
            names_prefix = paste0(s, "_"),
            values_fn = list(Value = ~ if (length(.) == 1) . else mean(., na.rm = TRUE)),
            values_fill = NA
          ) |>
          dplyr::filter(!is.na(GeoUID))
        
        colnames(df_filtered) <- cmhc_clean_names(colnames(df_filtered))
        list_name <- cmhc_clean_names(s)
        results_list[[list_name]] <- df_filtered
      }
    }
  }
  
  return(results_list)
}


#' Finalize GeoUID Column in CMHC Results
#'
#' Standardizes the geographic identifier column in all result tables by renaming
#' `GeoUID` or `geouid` to `id`. This ensures consistency across all outputs.
#' @param results_list A named list of `data.frame`s resulting from CMHC data processing.
#' @return A list of `data.frame`s with a standardized `id` column as the geographic identifier.
cmhc_finalize_results <- function(results_list) {
  lapply(results_list, function(df) {
    if (is.data.frame(df)) {
      if ("GeoUID" %in% colnames(df)) {
        df <- dplyr::rename(df, id = GeoUID)
      } else if ("geouid" %in% colnames(df)) {
        df <- dplyr::rename(df, id = geouid)
      }
    }
    return(df)
  })
}


#' Retrieve Annual CMHC Data for All CMAs
#'
#' Loops over all CMAs and request configurations to fetch, clean, reshape,
#' and aggregate CMHC data with annual frequency.
#'
#' @param requests A list of request objects. Each must include `survey`, `series`, and `dimension`.
#' @return A named list under `$CMA` containing wide-format CMHC data for all valid entries.
#' @export
cmhc_get_annual_cma <- function(requests) {
  cmhc_vectors <- list(CMA = list())
  cma_all <- cc.pipe::get_census_digital_scales(scales = "cma")$cmasplit

  # Set up parallel processing (seed is handled in future_lapply)
  future::plan(future::multisession)

  # Loop through each CMA
  cma_results <- future.apply::future_lapply(
    unique(cma_all$id),
    function(geo_uid) {
      local_results <- list()
      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        frequency <- if (!is.null(req$frequency)) req$frequency else "Annual"

        message(sprintf("Processing CMA %s — %s — %s / %s", geo_uid, frequency, survey, series))

        results <- cmhc_fetch_cma_data(survey, series, dimension, geo_uid, frequency)
        cleaned <- cmhc_clean_results(results, geo_uid = geo_uid)

        if (is.null(cleaned)) {
          warning(sprintf("CMA %s: No valid annual data found. Entry skipped.", geo_uid))
          next
        }

        reshaped_data <- cmhc_reshape_results(cleaned, dimension)

        for (key in names(reshaped_data)) {
          local_results[[key]] <- dplyr::bind_rows(local_results[[key]], reshaped_data[[key]])
        }
      }
      return(local_results)
    },
    future.seed = TRUE  # Ensures safe and reproducible parallel processing
  )

  # Combine results from all CMAs
  results_list <- list()
  any_valid_annual <- FALSE

  for (res in cma_results) {
    if (is.null(res)) next
    any_valid_annual <- TRUE
    for (key in names(res)) {
      results_list[[key]] <- dplyr::bind_rows(results_list[[key]], res[[key]])
    }
  }

  if (!any_valid_annual) {
    stop("ERROR: No valid annual data found. Try using cmhc_get_monthly_cma().")
  }

  cmhc_vectors$CMA <- cmhc_finalize_results(results_list)
  return(cmhc_vectors)
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
    warning(sprintf("CMA %s: no valid monthly format detected. Skipping entry.", geo_uid))
    return(NULL)
  }
  
  ym_valid <- year_month_vec[is_valid_monthly]
  months_only <- substr(ym_valid, 5, 6)
  
  if (length(unique(months_only)) == 1) {
    warning(sprintf("CMA %s: likely annual format (only month %s detected). Skipping entry.", geo_uid, unique(months_only)))
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
cmhc_get_monthly_cma <- function(requests) {
  cmhc_vectors <- list(CMA = list())
  cma_all <- cc.pipe::get_census_digital_scales(scales = "cma")$cmasplit

  # Set up parallel processing (seed handled inside future_lapply)
  future::plan(future::multisession)

  # Loop through each CMA
  cma_results <- future.apply::future_lapply(
    unique(cma_all$id),
    function(geo_uid) {
      local_results <- list()
      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        frequency <- if (!is.null(req$frequency)) req$frequency else "Monthly"

        message(sprintf("Processing CMA %s — %s — %s / %s", geo_uid, frequency, survey, series))

        raw_results <- cmhc_fetch_cma_data(survey, series, dimension, geo_uid, frequency)
        if (is.null(raw_results) || nrow(raw_results) == 0) next

        if (!"Series" %in% colnames(raw_results)) raw_results$Series <- series
        if (!"Survey" %in% colnames(raw_results)) raw_results$Survey <- survey

        raw_results <- cmhc_add_year_month(raw_results, geo_uid = geo_uid)
        if (is.null(raw_results)) {
          warning(sprintf("CMA %s: No valid monthly-format data found. Entry skipped.", geo_uid))
          next
        }

        reshaped_data <- cmhc_reshape_results(raw_results, dimension)

        for (key in names(reshaped_data)) {
          local_results[[key]] <- dplyr::bind_rows(local_results[[key]], reshaped_data[[key]])
        }
      }
      return(local_results)
    },
    future.seed = TRUE  # Ensures reproducible parallel computation
  )

  # Combine results from all CMAs
  results_list <- list()
  any_valid_monthly <- FALSE

  for (res in cma_results) {
    if (is.null(res)) next
    any_valid_monthly <- TRUE
    for (key in names(res)) {
      results_list[[key]] <- dplyr::bind_rows(results_list[[key]], res[[key]])
    }
  }

  if (!any_valid_monthly) {
    stop("ERROR: No valid monthly-format data found. Try using cmhc_get_annual_cma() instead.")
  }

  cmhc_vectors$CMA <- cmhc_finalize_results(results_list)
  return(cmhc_vectors)
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
cmhc_fetch_ct_data <- function(survey, series, dimension, geo_uid, year, month = NULL) {
  tryCatch({
    cmhc::get_cmhc(
      survey = survey,
      series = series,
      dimension = dimension,
      breakdown = "Census Tracts",
      geo_uid = geo_uid,
      year = year,
      month = if (!is.null(month)) sprintf("%02d", as.integer(month)) else NULL
    )
  }, error = function(e) {
    warning(sprintf("CT %s (%s-%s): get_cmhc() error -> %s", geo_uid, year, month %||% "--", e$message))
    return(NULL)
  })
}

#' Apply Census Tract Correspondence Table
#'
#' Harmonizes CT-level CMHC data to 2021 census geography by applying the appropriate
#' correspondence table. The function dynamically detects the census vintage and
#' matches the GeoUID using the correct mapping table.
#'
#' @param data A `data.frame` with a `Census geography` column and historical `GeoUID`s.
#' @param ct_correspondence_list A named list of correspondence tables between historical and 2021 GeoUIDs.
#'
#' @return A `data.frame` with GeoUIDs harmonized to 2021 geography.
cmhc_ct_correspondence <- function(data, ct_correspondence_list) {
  if (!is.null(data) && "Census geography" %in% colnames(data)) {
    
    # Remove geometry from input data if it exists
    if ("geometry" %in% names(data)) {
      data <- sf::st_drop_geometry(data)
    }
    
    # Extract the census year identifier
    census_geo <- unique(data$`Census geography`)
    if (length(census_geo) != 1) {
      stop("Invalid input: multiple or missing census geography identifiers.")
    }
    
    # If already 2021, standardize column names and return
    if (census_geo == "2021") {
      col_uid <- intersect(c("GeoUID", "geouid", "id"), names(data))
      if (length(col_uid) == 1 && col_uid != "GeoUID") {
        names(data)[names(data) == col_uid] <- "GeoUID"
      }
      return(data |> dplyr::select(-"Census geography"))
    }
    
    # Build correspondence table name
    corr_table_name <- paste0("correspondence_2021_", census_geo)
    if (!corr_table_name %in% names(ct_correspondence_list)) {
      stop("Missing correspondence table: ", corr_table_name)
    }
    
    correspondence_table <- ct_correspondence_list[[corr_table_name]]
    
    # Fix: drop geometry to prevent sfc issues in parallel environments
    if ("geometry" %in% names(correspondence_table)) {
      correspondence_table <- sf::st_drop_geometry(correspondence_table)
    }
    
    if (nrow(correspondence_table) == 0) {
      stop("Correspondence table is empty.")
    }
    
    # Identify old GeoUID column in the correspondence table
    old_geouid_col <- setdiff(
      names(correspondence_table)[grepl("^geouid_\\d+$", names(correspondence_table))],
      "geouid_21"
    )
    if (length(old_geouid_col) != 1) {
      stop("Ambiguous original GeoUID column in correspondence table.")
    }
    
    # Match GeoUID column in data
    geouid_col <- intersect(c("GeoUID", "geouid", "id"), colnames(data))
    colnames(data)[which(colnames(data) == geouid_col)] <- old_geouid_col
    
    # Join with correspondence table to get geouid_21
    data <- dplyr::inner_join(data, correspondence_table, by = old_geouid_col)
    
    # Clean and reorder columns
    data <- data |>
      dplyr::select(geouid_21, dplyr::everything()) |>
      dplyr::rename(GeoUID = geouid_21) |>
      dplyr::select(-dplyr::all_of(old_geouid_col),
                    -dplyr::any_of(c("geometry", "status", "cma_code", "Census geography")))
    
    return(data)
  } else {
    stop("Invalid input: 'Census geography' column is missing.")
  }
}


#' Retrieve Annual CMHC Data for All CTs (by CMA and Year)
#'
#' Downloads and reshapes annual CMHC Census Tract (CT) data for each CMA and year
#' specified in the request list. Data is harmonized to the 2021 geography using
#' correspondence tables.
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`, and `years`.
#' @param ct_correspondence_list A named list of correspondence tables to convert older CT GeoUIDs to 2021 equivalents.
#' @param cma_uids Optional. A character vector of CMAUIDs to include. If NULL, all available CMAUIDs from CT 2021 are used.
#'
#' @return A named list `CT` containing reshaped and harmonized data frames for each variable.
#'         Each data frame uses standardized 2021 GeoUIDs and columns by year.
#' @export
cmhc_get_annual_ct <- function(requests, ct_correspondence_list, cma_uids = NULL) {
  cmhc_vectors <- list(CT = list())

  # Load CMAUIDs from 2021 CTs
  ct_21 <- cancensus::get_census(dataset = "CA21", regions = list(C = "01"), level = "CT", geo_format = "sf")
  cma_all <- ct_21 |>
    sf::st_drop_geometry() |>
    dplyr::select(CMA_UID) |>
    dplyr::distinct() |>
    dplyr::rename(id = CMA_UID)

  if (is.null(cma_uids)) {
    cma_uids <- unique(cma_all$id)
  }

  local_ct_correspondence_list <- ct_correspondence_list
  future::plan(future::multisession)

  # Fetch and reshape data for each CMA and year
  cma_parallel_results <- future.apply::future_lapply(
    cma_uids,
    function(geo_uid) {
      local_results <- list()
      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        years <- req$years

        for (year in years) {
          message(sprintf("Processing CMA %s — year %s — %s / %s", geo_uid, year, survey, series))
          raw <- cmhc_fetch_ct_data(survey, series, dimension, geo_uid, year)
          if (is.null(raw) || nrow(raw) == 0) next

          cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
          if (is.null(cleaned)) next

          reshaped <- cmhc_reshape_results(cleaned, dimension)

          census_geo <- unique(raw$`Census geography`)
          if (length(census_geo) != 1) {
            warning(sprintf("CMA %s year %s: non-unique census geography detected", geo_uid, year))
            next
          }

          reshaped <- lapply(reshaped, function(df) {
            df$`Census geography` <- census_geo
            cmhc_ct_correspondence(df, local_ct_correspondence_list)
          })

          for (key in names(reshaped)) {
            cma_name <- paste0("cma_", geo_uid)
            if (!key %in% names(local_results)) local_results[[key]] <- list()
            if (!cma_name %in% names(local_results[[key]])) {
              local_results[[key]][[cma_name]] <- reshaped[[key]]
            } else {
              local_results[[key]][[cma_name]] <- dplyr::full_join(
                local_results[[key]][[cma_name]],
                reshaped[[key]],
                by = "GeoUID"
              )
            }
          }
        }
      }
      return(local_results)
    },
    future.seed = TRUE  # Ensures deterministic parallel behavior
  )

  # Merge all results across CMAs
  all_years_results <- list()
  for (cma_result in cma_parallel_results) {
    for (key in names(cma_result)) {
      for (cma_name in names(cma_result[[key]])) {
        if (!key %in% names(all_years_results)) all_years_results[[key]] <- list()
        if (!cma_name %in% names(all_years_results[[key]])) {
          all_years_results[[key]][[cma_name]] <- cma_result[[key]][[cma_name]]
        } else {
          all_years_results[[key]][[cma_name]] <- dplyr::full_join(
            all_years_results[[key]][[cma_name]],
            cma_result[[key]][[cma_name]],
            by = "GeoUID"
          )
        }
      }
    }
  }

  cmhc_vectors$CT <- lapply(all_years_results, function(ct_results_by_cma) {
    final_df <- dplyr::bind_rows(ct_results_by_cma)
    cmhc_finalize_results(list(final_df))[[1]]
  })

  return(cmhc_vectors)
}


#' Retrieve Monthly CMHC Data for All CTs (by CMA and Year/Month)
#'
#' Downloads and reshapes monthly CMHC Census Tract (CT) data for each CMA, year, and month
#' specified in the request list. Data is harmonized to the 2021 geography using correspondence tables.
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`, `years`, and `months`.
#' @param ct_correspondence_list A named list of correspondence tables used to map older GeoUIDs to 2021 equivalents.
#' @param cma_uids Optional. A vector of CMAUIDs to include. If NULL, all CMAUIDs available in the 2021 CT geography will be used.
#'
#' @return A named list `CT` containing reshaped and harmonized data frames for each variable,
#'         with columns formatted as "YYYYMM" for each month.
#' @export
cmhc_get_monthly_ct <- function(requests, ct_correspondence_list, cma_uids = NULL) {
  cmhc_vectors <- list(CT = list())

  # Load CMAUIDs from 2021 CTs
  ct_21 <- cancensus::get_census(dataset = "CA21", regions = list(C = "01"), level = "CT", geo_format = "sf")
  cma_all <- ct_21 |>
    sf::st_drop_geometry() |>
    dplyr::select(CMA_UID) |>
    dplyr::distinct() |>
    dplyr::rename(id = CMA_UID)

  if (is.null(cma_uids)) {
    cma_uids <- unique(cma_all$id)
  }

  local_ct_correspondence_list <- ct_correspondence_list
  future::plan(future::multisession)

  cma_parallel_results <- future.apply::future_lapply(
    cma_uids,
    function(geo_uid) {
      local_results <- list()

      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        years <- req$years
        months <- req$months

        for (year in years) {
          for (month in months) {
            message(sprintf("Processing CMA %s — %s-%s — %s / %s", geo_uid, year, month, survey, series))

            raw <- cmhc_fetch_ct_data(survey, series, dimension, geo_uid, year, month)
            if (is.null(raw) || nrow(raw) == 0) next

            cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
            if (is.null(cleaned)) next

            reshaped <- cmhc_reshape_results(cleaned, dimension)

            census_geo <- unique(raw$`Census geography`)
            if (length(census_geo) != 1) {
              warning(sprintf("CMA %s %s-%s: non-unique census geography detected", geo_uid, year, month))
              next
            }

            reshaped <- lapply(reshaped, function(df) {
              df$`Census geography` <- census_geo
              cmhc_ct_correspondence(df, local_ct_correspondence_list)
            })

            for (key in names(reshaped)) {
              cma_name <- paste0("cma_", geo_uid)
              if (!key %in% names(local_results)) local_results[[key]] <- list()
              if (!cma_name %in% names(local_results[[key]])) {
                local_results[[key]][[cma_name]] <- reshaped[[key]]
              } else {
                local_results[[key]][[cma_name]] <- dplyr::full_join(
                  local_results[[key]][[cma_name]],
                  reshaped[[key]],
                  by = "GeoUID"
                )
              }
            }
          }
        }
      }

      return(local_results)
    },
    future.seed = TRUE
  )

  # Final merge of all CMA-month combinations
  all_months_results <- list()
  for (cma_result in cma_parallel_results) {
    for (key in names(cma_result)) {
      for (cma_name in names(cma_result[[key]])) {
        if (!key %in% names(all_months_results)) all_months_results[[key]] <- list()
        if (!cma_name %in% names(all_months_results[[key]])) {
          all_months_results[[key]][[cma_name]] <- cma_result[[key]][[cma_name]]
        } else {
          all_months_results[[key]][[cma_name]] <- dplyr::full_join(
            all_months_results[[key]][[cma_name]],
            cma_result[[key]][[cma_name]],
            by = "GeoUID"
          )
        }
      }
    }
  }

  cmhc_vectors$CT <- lapply(all_months_results, function(ct_results_by_cma) {
    final_df <- dplyr::bind_rows(ct_results_by_cma)
    cmhc_finalize_results(list(final_df))[[1]]
  })

  return(cmhc_vectors)
}

#' Fetch CMHC Data for a Specific CSD
#'
#' Sends a request to the CMHC API to retrieve data for a specific Census Subdivision (CSD),
#' given the survey, series, dimension, year, and optionally the month.
#' Adds the GeoUID column manually to ensure downstream compatibility.
#' Catches errors and returns `NULL` if the API call fails.
#'
#' @param survey Character. CMHC survey identifier (e.g., "Rms", "Scss").
#' @param series Character. Data series to retrieve (e.g., "Starts", "Vacancy Rate").
#' @param dimension Character. Optional breakdown dimension (e.g., "Bedroom Type"). Can be `NULL`.
#' @param geo_uid Character. Geographic identifier for the Census Subdivision (CSD).
#' @param year Integer. Year of the data request.
#' @param month Integer or NULL. Optional month (1–12). If `NULL`, annual data will be fetched.
#'
#' @return A `data.frame` with the added `GeoUID` column, or `NULL` if an error occurs.
cmhc_fetch_csd_data <- function(survey, series, dimension, geo_uid, year, month = NULL) {
  tryCatch({
    df <- cmhc::get_cmhc(
      survey = survey,
      series = series,
      dimension = dimension,
      breakdown = "Census Subdivision",
      geo_uid = geo_uid,
      year = year,
      month = if (!is.null(month)) sprintf("%02d", as.integer(month)) else NULL
    )
    df$GeoUID <- geo_uid
    return(df)
  }, error = function(e) {
    warning(sprintf("CSD %s (%s-%s): get_cmhc() error -> %s", geo_uid, year, month %||% "--", e$message))
    return(NULL)
  })
}

#' Apply Census Subdivision Correspondence Table
#'
#' Harmonizes CSD-level CMHC data to 2021 census geography by applying the appropriate
#' correspondence table. The function dynamically detects the census vintage and
#' matches the GeoUID using the correct mapping table.
#'
#' @param data A `data.frame` with a `Census geography` column and historical `GeoUID`s.
#' @param csd_correspondence_list A named list of correspondence tables between historical and 2021 GeoUIDs.
#'
#' @return A `data.frame` with GeoUIDs harmonized to 2021 geography.
cmhc_csd_correspondence <- function(data, csd_correspondence_list) {
  if (!is.null(data) && "Census geography" %in% colnames(data)) {
    
    if ("geometry" %in% names(data)) {
      data <- sf::st_drop_geometry(data)
    }
    
    census_geo <- unique(data$`Census geography`)
    if (length(census_geo) != 1) {
      stop("Invalid input: multiple or missing census geography identifiers.")
    }
    
    if (census_geo == "2021") {
      col_uid <- intersect(c("GeoUID", "geouid", "id"), names(data))
      if (length(col_uid) == 1 && col_uid != "GeoUID") {
        names(data)[names(data) == col_uid] <- "GeoUID"
      }
      return(data |> dplyr::select(-"Census geography"))
    }
    
    corr_table_name <- paste0("correspondence_2021_", census_geo)
    if (!corr_table_name %in% names(csd_correspondence_list)) {
      stop("Missing correspondence table: ", corr_table_name)
    }
    
    correspondence_table <- csd_correspondence_list[[corr_table_name]]
    
    if ("geometry" %in% names(correspondence_table)) {
      correspondence_table <- sf::st_drop_geometry(correspondence_table)
    }
    
    if (nrow(correspondence_table) == 0) {
      stop("Correspondence table is empty.")
    }
    
    old_geouid_col <- setdiff(
      names(correspondence_table)[grepl("^geouid_\\d+$", names(correspondence_table))],
      "geouid_21"
    )
    if (length(old_geouid_col) != 1) {
      stop("Ambiguous original GeoUID column in correspondence table.")
    }
    
    geouid_col <- intersect(c("GeoUID", "geouid", "id"), colnames(data))
    colnames(data)[which(colnames(data) == geouid_col)] <- old_geouid_col
    
    data <- dplyr::inner_join(data, correspondence_table, by = old_geouid_col)
    
    data <- data |>
      dplyr::select(geouid_21, dplyr::everything()) |>
      dplyr::rename(GeoUID = geouid_21) |>
      dplyr::select(-dplyr::all_of(old_geouid_col),
                    -dplyr::any_of(c("geometry", "status", "cma_code", "Census geography")))
    
    return(data)
  } else {
    stop("Invalid input: 'Census geography' column is missing.")
  }
}


#' Retrieve Annual CMHC Data for All CSDs (by CMA and Year)
#'
#' Downloads and reshapes annual CMHC Census Subdivision (CSD) data for each CMA and year
#' specified in the request list. Data is harmonized to the 2021 geography using
#' correspondence tables.
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`, and `years`.
#' @param csd_correspondence_list A named list of correspondence tables to convert older CSD GeoUIDs to 2021 equivalents.
#' @param csd_uids Optional. A character vector of CMAUIDs to include. If NULL, all available CMAUIDs from CSD 2021 are used.
#'
#' @return A named list `CSD` containing reshaped and harmonized data frames for each variable.
#'         Each data frame uses standardized 2021 GeoUIDs and columns by year.
#' @export
cmhc_get_annual_csd <- function(requests, csd_correspondence_list, csd_uids = NULL) {
  cmhc_vectors <- list(CSD = list())
  
  csd_all <- cc.pipe::get_census_digital_scales(scales = "csd")$csd
  if (is.null(csd_uids)) {
    csd_uids <- unique(csd_all$id)
  }
  
  local_csd_correspondence_list <- csd_correspondence_list
  
  csd_parallel_results <- future.apply::future_lapply(
    csd_uids,
    function(geo_uid) {
      local_results <- list()
      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        years <- req$years
        
        for (year in years) {
          message(sprintf("Processing CSD %s — year %s — %s / %s", geo_uid, year, survey, series))
          
          raw <- cmhc_fetch_csd_data(survey, series, dimension, geo_uid, year)
          if (is.null(raw) || nrow(raw) == 0) next
          
          cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
          if (is.null(cleaned)) next
          
          reshaped <- cmhc_reshape_results(cleaned, dimension)
          
          reshaped <- lapply(reshaped, function(df) {
            df$Year <- year
            cmhc_csd_correspondence(df, local_csd_correspondence_list)
          })
          
          for (key in names(reshaped)) {
            csd_name <- paste0("csd_", geo_uid)
            if (!key %in% names(local_results)) local_results[[key]] <- list()
            if (!csd_name %in% names(local_results[[key]])) {
              local_results[[key]][[csd_name]] <- reshaped[[key]]
            } else {
              tmp_join <- dplyr::full_join(
                local_results[[key]][[csd_name]],
                reshaped[[key]],
                by = "GeoUID"
              )
              tmp_join <- tmp_join[, !grepl("^Year(\\.x|\\.y)?$", names(tmp_join))]
              local_results[[key]][[csd_name]] <- tmp_join
            }
          }
        }
      }
      return(local_results)
    },
    future.seed = TRUE
  )
  
  all_years_results <- list()
  for (csd_result in csd_parallel_results) {
    for (key in names(csd_result)) {
      for (csd_name in names(csd_result[[key]])) {
        if (!key %in% names(all_years_results)) all_years_results[[key]] <- list()
        if (!csd_name %in% names(all_years_results[[key]])) {
          all_years_results[[key]][[csd_name]] <- csd_result[[key]][[csd_name]]
        } else {
          all_years_results[[key]][[csd_name]] <- dplyr::full_join(
            all_years_results[[key]][[csd_name]],
            csd_result[[key]][[csd_name]],
            by = "GeoUID"
          )
        }
      }
    }
  }
  
  cmhc_vectors$CSD <- lapply(all_years_results, function(csd_results_by_csd) {
    final_df <- dplyr::bind_rows(csd_results_by_csd)
    cmhc_finalize_results(list(final_df))[[1]]
  })
  
  return(cmhc_vectors)
}

#' Retrieve Monthly CMHC Data for All CSDs (by CMA and Year/Month)
#'
#' Downloads and reshapes monthly CMHC Census Subdivision (CSD) data for each CMA, year, and month
#' specified in the request list. Data is harmonized to the 2021 geography using correspondence tables.
#'
#' @param requests A list of request objects. Each must include the keys `survey`, `series`, `dimension`, `years`, and `months`.
#' @param csd_correspondence_list A named list of correspondence tables used to map older GeoUIDs to 2021 equivalents.
#' @param csd_uids Optional. A vector of CMAUIDs to include. If NULL, all CMAUIDs available in the 2021 CSD geography will be used.
#'
#' @return A named list `CSD` containing reshaped and harmonized data frames for each variable,
#'         with columns formatted as "YYYYMM" for each month.
#' @export
cmhc_get_monthly_csd <- function(requests, csd_correspondence_list, csd_uids = NULL) {
  cmhc_vectors <- list(CSD = list())
  
  csd_all <- cc.pipe::get_census_digital_scales(scales = "csd")$csd
  if (is.null(csd_uids)) {
    csd_uids <- unique(csd_all$id)
  }
  
  local_csd_correspondence_list <- csd_correspondence_list
  
  csd_parallel_results <- future.apply::future_lapply(
    csd_uids,
    function(geo_uid) {
      local_results <- list()
      for (req in requests) {
        survey <- req$survey
        series <- req$series
        dimension <- req$dimension
        years <- req$years
        months <- req$months
        
        for (year in years) {
          for (month in months) {
            message(sprintf("Processing CSD %s — %s-%02d — %s / %s", geo_uid, year, month, survey, series))
            
            raw <- cmhc_fetch_csd_data(survey, series, dimension, geo_uid, year, month)
            if (is.null(raw) || nrow(raw) == 0) next
            
            cleaned <- cmhc_clean_results(raw, geo_uid = geo_uid)
            if (is.null(cleaned)) next
            
            reshaped <- cmhc_reshape_results(cleaned, dimension)
            
            reshaped <- lapply(reshaped, function(df) {
              df$Year <- year
              df$Month <- sprintf("%04d%02d", year, month)
              cmhc_csd_correspondence(df, local_csd_correspondence_list)
            })
            
            for (key in names(reshaped)) {
              csd_name <- paste0("csd_", geo_uid)
              if (!key %in% names(local_results)) local_results[[key]] <- list()
              if (!csd_name %in% names(local_results[[key]])) {
                local_results[[key]][[csd_name]] <- reshaped[[key]]
              } else {
                tmp_join <- dplyr::full_join(
                  local_results[[key]][[csd_name]],
                  reshaped[[key]],
                  by = "GeoUID"
                )
                tmp_join <- tmp_join[, !grepl("^Month(\\.x|\\.y)?$", names(tmp_join))]
                local_results[[key]][[csd_name]] <- tmp_join
              }
            }
          }
        }
      }
      return(local_results)
    },
    future.seed = TRUE
  )
  
  all_months_results <- list()
  for (csd_result in csd_parallel_results) {
    for (key in names(csd_result)) {
      for (csd_name in names(csd_result[[key]])) {
        if (!key %in% names(all_months_results)) all_months_results[[key]] <- list()
        if (!csd_name %in% names(all_months_results[[key]])) {
          all_months_results[[key]][[csd_name]] <- csd_result[[key]][[csd_name]]
        } else {
          all_months_results[[key]][[csd_name]] <- dplyr::full_join(
            all_months_results[[key]][[csd_name]],
            csd_result[[key]][[csd_name]],
            by = "GeoUID"
          )
        }
      }
    }
  }
  
  cmhc_vectors$CSD <- lapply(all_months_results, function(csd_results_by_csd) {
    final_df <- dplyr::bind_rows(csd_results_by_csd)
    cmhc_finalize_results(list(final_df))[[1]]
  })
  
  cmhc_vectors$CSD <- lapply(all_months_results, function(csd_results_by_csd) {
    final_df <- dplyr::bind_rows(csd_results_by_csd)
    final_df <- cmhc_finalize_results(list(final_df))[[1]]
    
    # Supprimer toutes les colonnes "Year", "Year.x", "Year.y", etc.
    final_df <- final_df[, !grepl("^Year(\\.|$)", names(final_df))]
    
    return(final_df)
  })
  
  return(cmhc_vectors)
}