#' Build land-use summary at arbitrary polygon boundaries (and per CMA-year)
#'
#' Scans a root directory of CMA subfolders (e.g., "MTL", "CGY"), loads each
#' land-use raster, and computes category counts and percentages for values
#' 1..5 (lowrise, midrise, highrise, nonres, nonbuilt).
#'
#' If `boundaries` is provided, computes the stats for each polygon in that
#' `sf` object (after reprojection to the raster CRS) for every raster file
#' found. Otherwise, returns one row per CMA-year file (total over all cells).
#'
#' @param base_dir Character scalar. Root folder containing CMA subfolders.
#' @param mapping Data.frame with required columns: "folder" (subfolder name)
#'   and "CMA_UID" (character or numeric id). Extra columns are ignored.
#' @param pattern_year Character scalar. Regex to extract a 4-digit year
#'   from filenames. Default "\\\\d{4}".
#' @param exact Logical scalar. If TRUE, use exact area fractions in
#'   intersections (slower, more accurate). If FALSE, count cell centers.
#' @param use_data_table Logical. Whether to use data.table for output
#'   (recommended for performance).
#' @param progress Logical. Whether to show progress messages.
#'
#' @return Data.frame. When `boundaries` is NULL, one row per CMA-year.
#'   When `boundaries` is provided, one row per (polygon id, CMA-year).
#'   Columns:
#'   \describe{
#'     \item{cma_id}{CMA UID as character}
#'     \item{year}{4-digit year (NA if not found)}
#'     \item{id}{polygon id (only when `boundaries` provided)}
#'     \item{*_pct}{percentages (0..1) for categories 1..5}
#'     \item{landuse_*}{raw counts for categories 1..5}
#'   }
#'
#' @details
#' The function processes raster files in parallel using future.apply.
#' Set up your parallel backend before calling this function:
#' \code{future::plan(future::multisession, workers = n_cores)}
#'
#' @export
build_landuse_summary <- function(
  base_dir = "calculated_ignore/landuse_v2/landuse_v2",
  mapping = data.table::data.table(
    folder = c(
      "VIC",
      "VAN",
      "CGY",
      "EDM",
      "WPG",
      "HMT",
      "TOR",
      "OTTGTU",
      "MTL",
      "QBC",
      "HRM"
    ),
    region = c(
      "bc_victoria",
      "bc_vancouver",
      "ab_calgary",
      "ab_edmonton",
      "mb_winnipeg",
      "on_hamilton",
      "on_toronto",
      "on_ottawa",
      "qc_montreal",
      "qc_quebec_city",
      "ns_halifax"
    ),
    CMA_UID = c(
      59935,
      59933,
      48825,
      48835,
      46602,
      35537,
      35535,
      505,
      24462,
      24421,
      12205
    )
  ),
  pattern_year = "\\d{4}",
  exact = FALSE,
  use_data_table = TRUE,
  progress = TRUE
) {
  # Load required packages with error handling
  required_pkgs <- c("terra", "sf", "data.table", "future.apply", "cancensus")
  if (use_data_table) {
    required_pkgs <- c(required_pkgs, "data.table")
  }

  missing_pkgs <- required_pkgs[
    !sapply(required_pkgs, requireNamespace, quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0) {
    stop(
      "Required packages not available: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }

  # --- Enhanced Validation with Performance Focus ---
  stopifnot(
    is.character(base_dir),
    length(base_dir) == 1L,
    dir.exists(base_dir),
    is.data.frame(mapping) || data.table::is.data.table(mapping),
    all(c("folder", "CMA_UID") %in% names(mapping))
  )

  # Convert to data.table for performance
  if (!data.table::is.data.table(mapping)) {
    mapping <- data.table::as.data.table(mapping)
  }
  mapping[, `:=`(
    folder = as.character(folder),
    CMA_UID = as.character(CMA_UID)
  )]

  # --- Optimized Helper Functions ---
  extract_year <- function(fnames, pattern) {
    # Vectorized year extraction
    years <- stringi::stri_extract_first_regex(basename(fnames), pattern)
    suppressWarnings(as.integer(years))
  }

  # Fast frequency counting with error handling
  safe_freq <- function(raster) {
    tryCatch(
      {
        ft <- terra::freq(raster, useNA = "no")
        if (is.null(ft) || nrow(ft) == 0) {
          return(NULL)
        }

        # Handle column name variations across terra versions
        val_col <- if ("value" %in% colnames(ft)) "value" else colnames(ft)[1]
        cnt_col <- if ("count" %in% colnames(ft)) "count" else colnames(ft)[2]

        # Return as named vector for fast lookup
        counts <- ft[[cnt_col]]
        names(counts) <- as.character(ft[[val_col]])
        counts
      },
      error = function(e) {
        warning("Failed to compute frequency for raster: ", e$message)
        NULL
      }
    )
  }

  # Optimized count extraction
  get_counts_fast <- function(freq_vec) {
    # Pre-allocate result vector
    counts <- numeric(5L)
    if (is.null(freq_vec)) {
      return(counts)
    }

    # Vectorized lookup - much faster than repeated .getn calls
    idx <- match(c("1", "2", "3", "4", "5"), names(freq_vec))
    counts[!is.na(idx)] <- freq_vec[idx[!is.na(idx)]]
    counts
  }

  # Compute percentages with division guard
  compute_percentages <- function(counts) {
    total <- sum(counts)
    if (total == 0) {
      return(rep(0, 5L))
    }
    counts / total
  }

  # --- File Discovery Phase (Optimized) ---
  if (progress) {
    cat("Discovering files...\n")
  }

  file_inventory <- mapping[,
    {
      cma_dir <- file.path(base_dir, folder)
      if (!dir.exists(cma_dir)) {
        list(tif_files = character(0), years = integer(0))
      } else {
        tifs <- list.files(cma_dir, pattern = "\\.tif$", full.names = TRUE)
        list(tif_files = tifs, years = extract_year(tifs, pattern_year))
      }
    },
    by = .(folder, CMA_UID)
  ]

  # Flatten and filter valid files
  file_dt <- file_inventory[lengths(tif_files) > 0][,
    {
      list(
        cma_id = CMA_UID,
        tif_path = unlist(tif_files),
        year = unlist(years)
      )
    },
    by = folder
  ][!is.na(year)]

  if (nrow(file_dt) == 0) {
    warning("No valid TIFF files found")
    return(data.table::data.table())
  }

  if (progress) {
    cat(
      "Found",
      nrow(file_dt),
      "files across",
      file_dt[, data.table::uniqueN(cma_id)],
      "regions\n"
    )
  }

  # --- Processing Functions ---

  # Fast whole-file processing
  process_file_simple <- function(file_info) {
    r <- tryCatch(terra::rast(file_info$tif_path), error = function(e) NULL)
    if (is.null(r)) {
      return(NULL)
    }

    freq_vec <- safe_freq(r)
    counts <- get_counts_fast(freq_vec)
    pcts <- compute_percentages(counts)

    list(
      cma_id = file_info$cma_id,
      year = file_info$year,
      landuse_res_lowrise_pct = pcts[1],
      landuse_res_midrise_pct = pcts[2],
      landuse_res_highrise_pct = pcts[3],
      landuse_nonres_pct = pcts[4],
      landuse_nonbuilt_pct = pcts[5],
      landuse_res_lowrise = counts[1],
      landuse_res_midrise = counts[2],
      landuse_res_highrise = counts[3],
      landuse_nonres = counts[4],
      landuse_nonbuilt = counts[5]
    )
  }

  # Boundary-aware processing with chunking
  process_file_boundaries <- function(file_info, boundaries_chunk) {
    r <- tryCatch(terra::rast(file_info$tif_path), error = function(e) NULL)
    if (is.null(r)) {
      return(NULL)
    }

    # Reproject boundaries to raster CRS (cached per unique CRS)
    b_proj <- sf::st_transform(boundaries_chunk, terra::crs(r))
    b_vect <- terra::vect(b_proj)

    # Extract with error handling
    ex <- tryCatch(
      {
        terra::extract(r, b_vect, exact = exact)
      },
      error = function(e) {
        warning("Extraction failed for ", file_info$tif_path, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(ex) || nrow(ex) == 0) {
      return(NULL)
    }

    # Convert to data.table for faster operations
    ex_dt <- data.table::as.data.table(ex)
    ex_dt <- ex_dt[!is.na(land_use)]

    if (nrow(ex_dt) == 0) {
      return(NULL)
    }

    # Fast aggregation using data.table
    if ("weight" %in% names(ex_dt)) {
      # Weighted aggregation
      agg <- ex_dt[,
        .(count = sum(weight, na.rm = TRUE)),
        by = .(ID, land_use) # Use actual column name
      ]
    } else {
      # Count aggregation
      agg <- ex_dt[, .(count = .N), by = .(ID, land_use)]
    }

    # Pivot to get counts per polygon
    result_list <- agg[,
      {
        counts <- numeric(5L)
        idx <- match(land_use, 1:5)
        valid_idx <- !is.na(idx)
        counts[idx[valid_idx]] <- count[valid_idx]
        pcts <- compute_percentages(counts)

        list(
          id = as.character(b_proj[["GeoUID"]][ID[1]]),
          cma_id = file_info$cma_id,
          year = file_info$year,
          landuse_res_lowrise_pct = pcts[1],
          landuse_res_midrise_pct = pcts[2],
          landuse_res_highrise_pct = pcts[3],
          landuse_nonres_pct = pcts[4],
          landuse_nonbuilt_pct = pcts[5],
          landuse_res_lowrise = counts[1],
          landuse_res_midrise = counts[2],
          landuse_res_highrise = counts[3],
          landuse_nonres = counts[4],
          landuse_nonbuilt = counts[5]
        )
      },
      by = ID
    ]

    result_list[, ID := NULL] # Remove helper column
    result_list
  }

  # --- Main Processing Logic with future.apply ---

  # Check if future backend is set up
  if (!inherits(future::plan(), "sequential") && progress) {
    cat("Using parallel processing with", future::nbrOfWorkers(), "workers\n")
  } else if (progress) {
    cat("Using sequential processing (consider setting up future::plan())\n")
  }

  # Process each file using future.apply for parallelism
  if (progress) {
    cat("Processing", nrow(file_dt), "files...\n")
  }

  all_results <- future.apply::future_lapply(
    seq_len(nrow(file_dt)),
    function(i) {
      file_info <- file_dt[i]

      # TODO: This hardcoded boundary retrieval should be parameterized
      chunk_result <- process_file_boundaries(
        file_info,
        boundaries_chunk = cancensus::get_census(
          "CA21",
          regions = list(CMA = file_info$cma_id),
          level = "CT",
          geo_format = "sf"
        )["GeoUID"]
      )

      return(chunk_result)
    },
    future.seed = TRUE
  )

  # Filter out NULL results and combine
  valid_results <- all_results[!sapply(all_results, is.null)]

  if (length(valid_results) == 0) {
    return(data.table::data.table())
  }

  if (progress) {
    cat("Combining results from", length(valid_results), "successful files\n")
  }

  if (use_data_table) {
    return(data.table::rbindlist(valid_results))
  } else {
    return(dplyr::bind_rows(valid_results))
  }
}
