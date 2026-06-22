#' Build land-use summary at arbitrary polygon boundaries (and per CMA-year)
#'
#' Scans a root directory of CMA subfolders (e.g., "MTL", "CGY"), loads each
#' land-use raster (Curbcut LULC v4, 10 m, 7-class), and computes per-class
#' area (m^2) and percentages for each polygon in `boundaries`.
#'
#' v4 pixel encoding: 1 = vege, 2 = water, 3 = lowrise, 4 = non-res built,
#' 5 = midrise, 6 = highrise, 7 = road.
#'
#' @param boundaries <`sf data.frame`> Polygons at which to compute land-use.
#'   Must have an `id` column.
#' @param base_dir Character scalar. Root folder containing CMA subfolders.
#'   Defaults to the v4 LULC drop at
#'   `CURBCUT_DATA_SHARING_PATH/../landuse/LandCover_Classification_v4` (the
#'   v4 folder sits one level above the `IMPORTANT_centralized_data` directory
#'   that `CURBCUT_DATA_SHARING_PATH` points to).
#' @param mapping Data.frame with required columns: "folder" (subfolder name)
#'   and "CMA_UID" (character or numeric id). Extra columns are ignored.
#' @param pattern_year Character scalar. Regex to extract a 4-digit year
#'   from filenames. Default "\\\\d{4}".
#' @param exact Logical scalar. If TRUE, use exact area fractions in
#'   intersections (slower, more accurate). If FALSE, count cell centers.
#' @param progress Logical. Whether to show progress messages.
#' @param checkpoint_dir Character scalar or NULL. If a directory path is
#'   supplied, each successful per-file extraction is written to
#'   `<checkpoint_dir>/<cma_id>_<year>.qs` immediately, and subsequent runs
#'   skip files whose checkpoint already exists. Makes the build crash-
#'   resumable: an interrupted run loses at most one file instead of an
#'   entire scale. Defaults to NULL (no checkpointing); the canonical
#'   shared location is
#'   `CURBCUT_DATA_SHARING_PATH/cc.data/landuse_ckpt/<scale>` — see the
#'   example at the bottom of `R/landuse.R`.
#'
#' @return Data.table with one row per (id, year). Columns:
#'   \describe{
#'     \item{id}{polygon id}
#'     \item{year}{4-digit year}
#'     \item{landuse_res_lowrise, landuse_res_midrise, landuse_res_highrise,
#'       landuse_nonres, landuse_vege, landuse_water, landuse_road}{area (m^2)
#'       per class}
#'     \item{*_pct}{percentages (0..1) of each class}
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
  base_dir = paste0(
    Sys.getenv("CURBCUT_DATA_SHARING_PATH"),
    "../landuse/LandCover_Classification_v4"
  ),
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
  progress = TRUE,
  checkpoint_dir = NULL
) {
  if (!"id" %in% names(boundaries)) {
    stop("`boundaries` df must have an `id` column")
  }

  # Reclaim memory at the start of each build. Long-running sessions that
  # process multiple census scales accumulate GDAL/terra cache fragments that
  # R's lazy gc cannot recover; an explicit full sweep here keeps each scale's
  # extractions from inheriting the previous scale's heap state.
  gc(full = TRUE, verbose = FALSE)
  if (requireNamespace("terra", quietly = TRUE)) {
    try(terra::tmpFiles(remove = TRUE), silent = TRUE)
  }

  # Load required packages with error handling
  required_pkgs <- c("terra", "sf", "data.table", "future.apply")
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
    r <- tryCatch(
      terra::rast(file_info$tif_path),
      error = function(e) {
        stop(
          "Failed to load raster: ",
          file_info$tif_path,
          ": ",
          e$message,
          call. = FALSE
        )
      }
    )

    # TODO: Cache CRS transformations per unique CRS to avoid repeated operations
    b_proj <- sf::st_transform(boundaries_chunk, terra::crs(r))

    # Drop polygons that can't possibly intersect this raster. Each LULC v4
    # file covers one CMA, so the vast majority of the country's polygons
    # (DBs, DAs, CTs, CSDs, even CMAs from other regions) sit outside the
    # raster's extent and only cost memory if passed to terra::extract.
    r_bbox <- sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = terra::xmin(r),
          xmax = terra::xmax(r),
          ymin = terra::ymin(r),
          ymax = terra::ymax(r)
        ),
        crs = sf::st_crs(b_proj)
      )
    )
    hits <- lengths(sf::st_intersects(b_proj, r_bbox)) > 0L
    b_proj <- b_proj[hits, ]
    if (nrow(b_proj) == 0L) {
      rm(r, r_bbox)
      try(terra::tmpFiles(remove = TRUE), silent = TRUE)
      gc(verbose = FALSE)
      return(NULL)
    }

    # Store original boundary IDs before creating terra vector (critical for mapping)
    original_ids <- b_proj[["id"]]
    b_vect <- terra::vect(b_proj)

    # TODO: Extract actual pixel resolution instead of hardcoding
    pixel_area_m2 <- prod(terra::res(r)) # Real pixel area from raster metadata

    # v4 pixel encoding:
    # 1 = vege, 2 = water, 3 = lowrise, 4 = non-res built,
    # 5 = midrise, 6 = highrise, 7 = road
    classes <- c(
      "landuse_vege", # 1
      "landuse_water", # 2
      "landuse_res_lowrise", # 3
      "landuse_nonres", # 4
      "landuse_res_midrise", # 5
      "landuse_res_highrise", # 6
      "landuse_road" # 7
    )

    gc(verbose = FALSE) # Clean memory before large operation

    if (exact) {
      # Weighted path (area-fraction at polygon edges) still uses extract()
      # because freq() can't apply partial-pixel weights.
      ex <- tryCatch(
        terra::extract(r, b_vect, exact = TRUE),
        error = function(e) {
          stop(
            "Extraction failed for ",
            file_info$tif_path,
            ": ",
            e$message,
            call. = FALSE
          )
        }
      )
      if (is.null(ex) || nrow(ex) == 0) {
        return(NULL)
      }

      ex_dt <- data.table::as.data.table(ex)
      value_col <- setdiff(names(ex_dt), c("ID", "weight"))[1]
      if (is.na(value_col)) {
        return(NULL)
      }
      data.table::setnames(ex_dt, value_col, "land_use")
      ex_dt <- ex_dt[!is.na(land_use)]
      if (nrow(ex_dt) == 0) {
        return(NULL)
      }
      if (max(ex_dt$ID, na.rm = TRUE) > length(original_ids)) {
        stop("Terra ID exceeds boundary count - data structure mismatch")
      }

      agg <- ex_dt[,
        .(count = sum(weight, na.rm = TRUE)),
        by = .(ID, land_use)
      ]

      result_list <- agg[,
        {
          counts <- numeric(7L)
          idx <- match(land_use, 1:7)
          valid_idx <- !is.na(idx)
          counts[idx[valid_idx]] <- count[valid_idx]
          areas <- counts * pixel_area_m2

          out <- list(
            id = as.character(original_ids[ID[1]]),
            cma_id = file_info$cma_id,
            year = file_info$year
          )
          for (k in seq_along(classes)) {
            out[[classes[k]]] <- areas[k]
          }
          out
        },
        by = ID
      ]
      result_list[, ID := NULL]
    } else {
      # Memory-safe path: crop + mask + freq per polygon. terra::freq counts
      # pixels by class at the C level WITHOUT materializing the per-pixel
      # table in R memory. This is what lets the function survive on huge
      # polygons (e.g. the Edmonton CMA at 10 m = ~950M pixels), where
      # terra::extract would try to allocate a ~15 GB data.frame and OOM.
      n_poly <- length(b_vect)
      rows <- vector("list", n_poly)
      for (k in seq_len(n_poly)) {
        one_poly <- b_vect[k]
        r_part <- tryCatch(
          terra::mask(terra::crop(r, one_poly, snap = "out"), one_poly),
          error = function(e) NULL
        )
        counts <- numeric(7L)
        if (!is.null(r_part)) {
          # Call terra::freq() with no extra args — it reliably returns a
          # data.frame with columns ('layer'?, value, count) across terra
          # versions. Earlier attempts (`useNA = "no"` / `value = 1:7`)
          # hit version-specific signature quirks that made tryCatch return
          # NULL silently and left counts at zero.
          ft <- tryCatch(
            as.data.frame(terra::freq(r_part)),
            error = function(e) NULL
          )
          if (!is.null(ft) && nrow(ft) > 0) {
            val_col <- if ("value" %in% colnames(ft)) {
              "value"
            } else {
              colnames(ft)[1]
            }
            cnt_col <- if ("count" %in% colnames(ft)) {
              "count"
            } else {
              colnames(ft)[2]
            }
            vals <- ft[[val_col]]
            cnts <- ft[[cnt_col]]
            # Filter to non-NA cells in the v4 class range (1..7);
            # a stray `0` padding value sometimes appears at raster edges.
            keep <- !is.na(vals) & vals %in% 1:7
            if (any(keep)) {
              idx <- match(
                as.character(1:7),
                as.character(vals[keep])
              )
              counts[!is.na(idx)] <- cnts[keep][idx[!is.na(idx)]]
            }
          }
          rm(r_part)
        }
        areas <- counts * pixel_area_m2
        row <- list(
          id = as.character(original_ids[k]),
          cma_id = file_info$cma_id,
          year = file_info$year
        )
        for (j in seq_along(classes)) {
          row[[classes[j]]] <- areas[j]
        }
        rows[[k]] <- row
      }
      result_list <- data.table::rbindlist(rows)
      ex <- NULL
      ex_dt <- NULL
      agg <- NULL # placeholders for the cleanup rm() below
    }

    # Release the SpatRaster and the intermediate vectors so GDAL's file
    # handle is closed before we move to the next raster. Without this, on
    # large extractions (e.g. DB scale on 10 m rasters) GDAL's cache holds
    # onto the previous file and fragmentation eventually triggers
    # std::bad_alloc somewhere mid-batch.
    rm(r, r_bbox, b_proj, b_vect, ex, ex_dt, agg)
    try(terra::tmpFiles(remove = TRUE), silent = TRUE)
    gc(verbose = FALSE)

    return(result_list)
  }

  # --- Main Processing Logic with future.apply ---

  # Check if future backend is set up
  if (!inherits(future::plan(), "sequential") && progress) {
    cat("Using parallel processing with", future::nbrOfWorkers(), "workers\n")
  } else if (progress) {
    cat("Using sequential processing (consider setting up future::plan())\n")
  }

  # Process each file using future.apply for parallelism. progressr emits a
  # progress signal after each file so the user sees forward motion (works in
  # both sequential and parallel future plans).
  if (progress) {
    cat("Processing", nrow(file_dt), "files...\n")
    # Fall back to a text progress bar when no handler is configured; users
    # can override with `progressr::handlers(...)` in their session.
    if (length(progressr::handlers(global = NA)) == 0) {
      progressr::handlers("txtprogressbar")
    }
  }

  # Per-file checkpointing: when checkpoint_dir is set, each successful
  # extraction is written to disk before the next file starts. On re-run,
  # already-completed files are read from cache instead of re-processed,
  # so a mid-scale crash costs at most one file's worth of work.
  ckpt_path <- function(file_info) {
    if (is.null(checkpoint_dir)) {
      return(NULL)
    }
    file.path(
      checkpoint_dir,
      sprintf("%s_%s.qs", file_info$cma_id, file_info$year)
    )
  }

  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
    done <- vapply(
      seq_len(nrow(file_dt)),
      function(i) file.exists(ckpt_path(file_dt[i, ])),
      logical(1)
    )
    if (progress && any(done)) {
      cat(
        "Resume mode:",
        sum(done),
        "of",
        nrow(file_dt),
        "files already cached in",
        checkpoint_dir,
        "\n"
      )
    }
  }

  run_lapply <- function(p) {
    future.apply::future_lapply(
      seq_len(nrow(file_dt)),
      function(i) {
        file_info <- file_dt[i, ]
        cp <- ckpt_path(file_info)

        # Skip files already processed in a previous run.
        if (!is.null(cp) && file.exists(cp)) {
          chunk_result <- qs::qread(cp)
        } else {
          chunk_result <- process_file_boundaries(
            file_info,
            boundaries_chunk = boundaries
          )
          if (!is.null(cp)) {
            # Atomic write so a crash mid-save doesn't leave a half-file.
            tmp <- paste0(cp, ".part")
            qs::qsave(chunk_result, tmp)
            file.rename(tmp, cp)
          }
        }

        if (!is.null(p)) {
          p(sprintf("%s %s", file_info$cma_id, file_info$year))
        }
        chunk_result
      },
      future.seed = TRUE,
      # data.table and terra are loaded once per worker at startup, not per
      # task — avoids stdio chatter that can corrupt the future IPC handshake.
      future.packages = c("data.table", "terra")
    )
  }

  all_results <- if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(file_dt))
      run_lapply(p)
    })
  } else {
    run_lapply(NULL)
  }

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

  # Define area columns for aggregation (v4: 7 classes)
  area_cols <- c(
    "landuse_res_lowrise",
    "landuse_res_midrise",
    "landuse_res_highrise",
    "landuse_nonres",
    "landuse_vege",
    "landuse_water",
    "landuse_road"
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


#' Assemble per-file landuse checkpoints into a single plug-and-play .qs
#'
#' Reads every per-file checkpoint produced by [build_landuse_from_file()]
#' (via its `checkpoint_dir` argument), rbinds them per scale, applies the
#' same dedup + aggregation + percentage pipeline used inside the build
#' function, and returns a named list of one aggregated data.table per
#' scale subdirectory. Optionally writes the result to a single `.qs` file
#' so downstream code can load one artifact instead of walking a tree of
#' per-file caches.
#'
#' @param ckpt_dir Character. Root directory containing one subdirectory
#'   per scale (e.g. `landuse_ckpt/CMA`, `landuse_ckpt/DB`). Each
#'   subdirectory should contain `<cma_id>_<year>.qs` checkpoint files
#'   written by [build_landuse_from_file()]. Defaults to the canonical
#'   shared location `CURBCUT_DATA_SHARING_PATH/cc.data/landuse_ckpt`.
#' @param out_path Character or NULL. If a path is supplied, the assembled
#'   list is written there with `qs::qsave`. Defaults to the canonical
#'   shared location `CURBCUT_DATA_SHARING_PATH/cc.data/landuse.qs`.
#' @param aggregate Logical. If TRUE (default), run the same dedup +
#'   aggregation + pct-computation pipeline as [build_landuse_from_file()],
#'   so each scale's data.table is one row per `(id, year)` with seven
#'   `landuse_*` area columns (m^2) and seven `landuse_*_pct` columns
#'   (summing to 1.0). If FALSE, return the raw `rbindlist`-ed checkpoint
#'   rows (preserves the `cma_id` column, may contain multiple rows per
#'   `(id, year)` when a polygon's bbox intersects more than one CMA's
#'   raster).
#' @param progress Logical. Whether to print per-scale progress messages.
#'
#' @return A named list with one entry per scale subdirectory of
#'   `ckpt_dir`, each a `data.table`. Invisibly returned when `out_path`
#'   is supplied.
#'
#' @export
landuse_assemble_checkpoints <- function(
  ckpt_dir = paste0(
    Sys.getenv("CURBCUT_DATA_SHARING_PATH"),
    "cc.data/landuse_ckpt"
  ),
  out_path = paste0(
    Sys.getenv("CURBCUT_DATA_SHARING_PATH"),
    "cc.data/landuse.qs"
  ),
  aggregate = TRUE,
  progress = TRUE
) {
  stopifnot(
    is.character(ckpt_dir),
    length(ckpt_dir) == 1L,
    dir.exists(ckpt_dir)
  )

  area_cols <- c(
    "landuse_res_lowrise",
    "landuse_res_midrise",
    "landuse_res_highrise",
    "landuse_nonres",
    "landuse_vege",
    "landuse_water",
    "landuse_road"
  )
  pct_cols <- paste0(area_cols, "_pct")

  scale_dirs <- list.dirs(ckpt_dir, recursive = FALSE)
  if (length(scale_dirs) == 0L) {
    warning("No scale subdirectories found in ", ckpt_dir)
    return(invisible(list()))
  }
  scale_names <- basename(scale_dirs)

  result <- stats::setNames(
    vector("list", length(scale_dirs)),
    scale_names
  )

  for (i in seq_along(scale_dirs)) {
    s <- scale_names[i]
    files <- list.files(scale_dirs[i], pattern = "\\.qs$", full.names = TRUE)
    if (length(files) == 0L) {
      if (progress) {
        cat(sprintf("[%s] no checkpoint files, skipping\n", s))
      }
      result[[s]] <- data.table::data.table()
      next
    }
    if (progress) {
      cat(sprintf("[%s] reading %d files...\n", s, length(files)))
    }

    pieces <- lapply(files, function(f) {
      tryCatch(qs::qread(f), error = function(e) {
        warning("Failed to read ", f, ": ", e$message)
        NULL
      })
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]

    if (length(pieces) == 0L) {
      result[[s]] <- data.table::data.table()
      next
    }

    combined <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    if (progress) {
      cat(sprintf("[%s] combined: %d rows\n", s, nrow(combined)))
    }

    if (!aggregate || nrow(combined) == 0L) {
      result[[s]] <- combined
      next
    }

    missing_cols <- setdiff(area_cols, names(combined))
    if (length(missing_cols) > 0L) {
      stop(sprintf(
        "[%s] missing expected columns: %s",
        s,
        paste(missing_cols, collapse = ", ")
      ))
    }

    # Same post-processing as build_landuse_from_file(): dedup by max
    # total_activity per (id, year), sum, then compute percentages.
    combined[, total_activity := rowSums(.SD), .SDcols = area_cols]
    deduplicated <- combined[
      combined[, .I[which.max(total_activity)], by = .(id, year)]$V1
    ]
    aggregated <- deduplicated[,
      lapply(.SD, sum, na.rm = TRUE),
      by = .(id, year),
      .SDcols = area_cols
    ]
    if ("total_activity" %in% names(aggregated)) {
      aggregated[, total_activity := NULL]
    }

    aggregated[, total_area := rowSums(.SD), .SDcols = area_cols]
    aggregated[,
      (pct_cols) := lapply(.SD, function(x) {
        ifelse(total_area == 0, 0, x / total_area)
      }),
      .SDcols = area_cols
    ]
    aggregated[, total_area := NULL]

    result[[s]] <- aggregated
    if (progress) {
      cat(sprintf("[%s] aggregated: %d rows\n", s, nrow(aggregated)))
    }
  }

  if (!is.null(out_path)) {
    if (progress) {
      cat(sprintf("Saving to %s ...\n", out_path))
    }
    qs::qsave(result, out_path)
  }

  invisible(result)
}

# Do NOT raise GDAL_CACHEMAX or terra::terraOptions(memfrac=...) here.
# When the symptom is std::bad_alloc partway through a long run, both knobs
# make it worse, not better: they tell GDAL/terra to hoard MORE memory in
# C++ allocators, which makes the next contiguous allocation more likely to
# fail. If you want to nudge terra at all, go the other way:
#   terra::terraOptions(memfrac = 0.3)   # smaller in-memory blocks
# The real durability fix is per-file checkpointing via `checkpoint_dir`,
# already wired through build_landuse_from_file().

# # Pull boundaries straight from the Curbcut PostgreSQL DB (schema `geography`),
# # restricted to the 11 CMAs covered by LULC v4 via the dictionary
# # `geographic_hierarchy_tree` materialized view. Requires cc.pipe::db_connect().
# cma_uids <- c(
#   "24421", "59935", "59933", "48825", "48835", "46602",
#   "35537", "35535", "505",   "24462", "12205"
# )
# vintage <- 2021
# cma_in <- paste(sprintf("'%s'", cma_uids), collapse = ",")

# conn <- cc.pipe::db_connect()
# on.exit(cc.pipe::db_disconnect(conn), add = TRUE)

# census_dfs <- sapply(
#   c("db", "da", "ct", "csd", "cma"),
#   \(census_scale) {
#     query <- if (census_scale == "cma") {
#       sprintf(
#         "SELECT id, geometry
#            FROM geography.cma
#           WHERE vintage = '%s'
#             AND boundary_type = 'cartographic'
#             AND id IN (%s)",
#         vintage, cma_in
#       )
#     } else {
#       # Sub-CMA IDs come from the hierarchy tree (transitive spatial containment),
#       # then geometries are joined back from the geography schema.
#       sprintf(
#         "SELECT lower.id, lower.geometry
#            FROM geography.%s AS lower
#           WHERE lower.vintage = '%s'
#             AND lower.boundary_type = 'cartographic'
#             AND lower.id IN (
#                   SELECT DISTINCT source_geo_id
#                     FROM dictionary.geographic_hierarchy_tree
#                    WHERE target_geo_level_id = 'cma'
#                      AND source_geo_level_id = '%s'
#                      AND target_geo_id IN (%s)
#                 )",
#         census_scale, vintage, census_scale, cma_in
#       )
#     }
#     sf::st_read(conn, query = query, geometry_column = "geometry")
#   },
#   simplify = FALSE,
#   USE.NAMES = TRUE
# )
# names(census_dfs) <- toupper(names(census_dfs))

# # Run the extraction once per scale. Each successful per-file result is
# # written to CURBCUT_DATA_SHARING_PATH/cc.data/landuse_ckpt/<scale>/<cma_id>_<year>.qs
# # so a crash only costs the file currently in flight. Smallest scales first.
# ckpt_root <- paste0(Sys.getenv("CURBCUT_DATA_SHARING_PATH"), "cc.data/landuse_ckpt")
# future::plan(future::sequential)  # multisession multiplies GDAL memory, not throughput
# for (scale in c("CMA", "CSD", "CT", "DA", "DB")) {
#   cat("==", scale, "==\n")
#   build_landuse_from_file(
#     census_dfs[[scale]],
#     exact = FALSE,
#     checkpoint_dir = file.path(ckpt_root, scale)
#   )
#   gc(full = TRUE)
# }
#
# # Assemble the per-file checkpoints into a single plug-and-play artifact.
# # Defaults already point at CURBCUT_DATA_SHARING_PATH/cc.data, so no args needed.
# landuse_dfs <- landuse_assemble_checkpoints()
