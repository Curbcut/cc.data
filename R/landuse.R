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
#' @param census_scale <`sf data.frame`> Dataframe at which to calculate landuse.
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
build_landuse_from_file <- function(
  boundaries,
  base_dir = "calculated_ignore/landuse_v3",
  mapping = data.table::data.table(
    folder = c(
      "QBC",
      "VIC",
      "VAN",
      "CGY",
      "EDM",
      "WPG",
      "HMT",
      "TOR",
      "OTTGTU",
      "MTL",
      "HRM"
    ),
    region = c(
      "qc_quebec",
      "bc_victoria",
      "bc_vancouver",
      "ab_calgary",
      "ab_edmonton",
      "mb_winnipeg",
      "on_hamilton",
      "on_toronto",
      "on_ottawa",
      "qc_montreal",
      "ns_halifax"
    ),
    CMA_UID = c(
      24421,
      59935,
      59933,
      48825,
      48835,
      46602,
      35537,
      35535,
      505,
      24462,
      12205
    )
  ),
  pattern_year = "\\d{4}",
  exact = FALSE,
  use_data_table = TRUE,
  progress = TRUE
) {
  if (!"id" %in% names(boundaries)) {
    stop("`boundaries` df must have an `id` column")
  }

  # Load required packages with error handling
  required_pkgs <- c("terra", "sf", "data.table", "future.apply", "cancensus")
  if (use_data_table) {
    required_pkgs <- c(required_pkgs, "data.table")
  }
  require("data.table")

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

  # Boundary-aware processing with chunking
  process_file_boundaries <- function(file_info, boundaries_chunk) {
    print(file_info)
    r <- tryCatch(terra::rast(file_info$tif_path), error = function(e) NULL)
    if (is.null(r)) {
      stop("Failed to load raster: ", file_info$tif_path, ": ", e$message)
    }

    # TODO: Cache CRS transformations per unique CRS to avoid repeated operations
    b_proj <- sf::st_transform(boundaries_chunk, terra::crs(r))

    # Store original boundary IDs before creating terra vector (critical for mapping)
    original_ids <- b_proj[["id"]]
    b_vect <- terra::vect(b_proj)

    # TODO: Extract actual pixel resolution instead of hardcoding
    pixel_area_m2 <- prod(terra::res(r)) # Real pixel area from raster metadata

    # Extract with error handling and memory cleanup
    ex <- tryCatch(
      {
        gc(verbose = FALSE) # Clean memory before large operation
        terra::extract(r, b_vect, exact = exact)
      },
      error = function(e) {
        stop("Extraction failed for ", file_info$tif_path, ": ", e$message)
      }
    )

    if (is.null(ex) || nrow(ex) == 0) {
      return(NULL)
    }

    # Convert to data.table and filter valid land use values
    ex_dt <- data.table::as.data.table(ex)
    ex_dt <- ex_dt[!is.na(land_use)]

    if (nrow(ex_dt) == 0) {
      return(NULL)
    }

    # Validate terra IDs don't exceed boundary count (safety check)
    if (max(ex_dt$ID, na.rm = TRUE) > length(original_ids)) {
      stop("Terra ID exceeds boundary count - data structure mismatch")
    }

    # Fast aggregation using data.table
    if ("weight" %in% names(ex_dt)) {
      # Exact=TRUE case: weighted aggregation by area fractions
      agg <- ex_dt[,
        .(count = sum(weight, na.rm = TRUE)),
        by = .(ID, land_use)
      ]
    } else {
      # Exact=FALSE case: simple pixel counting
      agg <- ex_dt[, .(count = .N), by = .(ID, land_use)]
    }

    # Transform aggregated data into final format
    result_list <- agg[,
      {
        # Initialize count vector for 5 land use categories
        counts <- numeric(5L)

        # Map land use values (1-5) to array positions
        idx <- match(land_use, 1:5)
        valid_idx <- !is.na(idx)
        counts[idx[valid_idx]] <- count[valid_idx]

        # Convert pixel counts to area (m²)
        areas <- counts * pixel_area_m2

        list(
          id = as.character(original_ids[ID[1]]), # Safe mapping using stored IDs
          cma_id = file_info$cma_id,
          year = file_info$year,
          landuse_res_lowrise = areas[1],
          landuse_res_midrise = areas[2],
          landuse_res_highrise = areas[3],
          landuse_nonres = areas[4],
          landuse_nonbuilt = areas[5]
        )
      },
      by = ID
    ]

    result_list[, ID := NULL] # Remove terra's internal ID column
    return(result_list)
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
        boundaries_chunk = boundaries
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

  # Efficient combination of results
  combined <- data.table::rbindlist(
    valid_results,
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(combined) == 0) {
    return(data.table::data.table())
  }

  # Define area columns for aggregation
  # Define area columns for aggregation
  area_cols <- c(
    "landuse_res_lowrise",
    "landuse_res_midrise",
    "landuse_res_highrise",
    "landuse_nonres",
    "landuse_nonbuilt"
  )

  # Validate expected columns exist
  missing_cols <- setdiff(area_cols, names(combined))
  if (length(missing_cols) > 0) {
    stop("Missing expected columns: ", paste(missing_cols, collapse = ", "))
  }

  # STEP 1: Calculate total land use activity per (id, cma_id, year) combination
  combined[, total_activity := rowSums(.SD), .SDcols = area_cols]

  # STEP 2: For each (id, year), keep only the CMA version with highest total activity
  # This handles edge effects by assigning boundary to its "primary" CMA
  deduplicated <- combined[
    combined[, .I[which.max(total_activity)], by = .(id, year)]$V1
  ]

  if (progress) {
    duplicates_removed <- nrow(combined) - nrow(deduplicated)
    if (duplicates_removed > 0) {
      cat(
        "Removed",
        duplicates_removed,
        "duplicate id-year combinations (kept highest activity)\n"
      )
    }
  }

  # STEP 3: Now aggregate by (id, year) - should be 1:1 mapping after deduplication
  aggregated <- deduplicated[,
    lapply(.SD, sum, na.rm = TRUE),
    by = .(id, year),
    .SDcols = area_cols
  ]

  # Remove the temporary total_activity column if it exists
  if ("total_activity" %in% names(aggregated)) {
    aggregated[, total_activity := NULL]
  }

  # Calculate total area per boundary for percentage computation
  aggregated[, total_area := rowSums(.SD), .SDcols = area_cols]

  # Compute percentages (0-1 scale) with proper zero handling
  pct_cols <- paste0(area_cols, "_pct")
  aggregated[,
    (pct_cols) := lapply(.SD, function(x) {
      ifelse(total_area == 0, 0, x / total_area)
    }),
    .SDcols = area_cols
  ]

  # Remove intermediate total_area column
  aggregated[, total_area := NULL]

  # Validate output structure
  if (progress) {
    cat("Final result:", nrow(aggregated), "unique id-year combinations\n")
    cat("Columns:", paste(names(aggregated), collapse = ", "), "\n")
  }

  # Validate no duplicates remain
  duplicates_check <- aggregated[, .N, by = .(id, year)][N > 1]
  if (nrow(duplicates_check) > 0) {
    stop("Duplicates still exist after deduplication - logic error")
  }

  # Validate percentages sum to approximately 1.0
  pct_sums <- rowSums(aggregated[, ..pct_cols])
  bad_pcts <- abs(pct_sums - 1.0) > 0.001
  if (any(bad_pcts, na.rm = TRUE)) {
    warning(
      "Some percentage rows don't sum to 1.0 (found ",
      sum(bad_pcts, na.rm = TRUE),
      " cases)"
    )
  }

  return(aggregated)
}

# # Increase GDAL cache to 4GB (adjust based on your total system RAM)
# Sys.setenv("GDAL_CACHEMAX" = "32768")
# # Increase terra memory fraction to 80%
# terra::terraOptions(memfrac = 0.8)

# census_dfs <- sapply(
#   c("DB", "DA", "CT", "CSD", "CMA"),
#   \(census_scale) {
#     out <- cancensus::get_census(
#       "CA21",
#       regions = list(
#         CMA = c(
#           59935,
#           59933,
#           48825,
#           48835,
#           46602,
#           35537,
#           35535,
#           505,
#           24462,
#           24421,
#           12205
#         )
#       ),
#       level = census_scale,
#       geo_format = "sf"
#     )["GeoUID"]
#     names(out)[1] <- "id"
#     out
#   },
#   simplify = FALSE,
#   USE.NAMES = TRUE
# )
# landuse_dfs <- lapply(census_dfs, build_landuse_from_file, exact = FALSE)
