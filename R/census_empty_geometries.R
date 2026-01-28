#' Get empty census geometries
#'
#' Downloads census geometries for requested scales/years with a
#' Canada-level request first, falling back to per-province when needed.
#' Returns simple `sf` layers with `ID` and `geometry` only, projected to EPSG:3347.
#'
#' @param census_scales <character> Census levels (e.g., "C","PR","CMA","CSD","CT","DA").
#' @param census_years  <numeric>   Census years (e.g., 1996, 2001, …, 2021).
#'
#' @return Named list `out[[scale]][[year]]` with `sf(ID, geometry)` in EPSG:3347.
#' @export
census_empty_geometries <- function(
  census_scales = cc.data::census_scales,
  census_years = cc.data::census_years
) {
  stopifnot(length(census_scales) > 0L, length(census_years) > 0L)

  # Build dataset codes like "CA1996", …, "CA2021"
  census_dataset <- paste0("CA", sub("^20", "", as.character(census_years)))
  latest_census <- census_dataset[length(census_dataset)]

  # Cache of PR codes per dataset (avoids repeated lookups)
  pr_cache <- new.env(parent = emptyenv())
  get_pr_codes <- function(ds) {
    key <- paste0("pr_", ds)
    if (!exists(key, envir = pr_cache)) {
      regs <- cancensus::list_census_regions(ds, quiet = TRUE)
      pr <- regs$region[regs$level == "PR"]
      pr <- gsub("^PR", "", pr) # keep numeric codes only
      assign(key, pr, envir = pr_cache)
    }
    get(key, envir = pr_cache)
  }

  # Try fetching for Canada; if it fails, fetch per-province and bind
  fetch_canada_then_pr <- function(ds, scale) {
    tryCatch(
      {
        tmp <- cancensus::get_census(
          dataset = ds,
          regions = list(C = "01"),
          level = scale,
          geo_format = "sf",
          quiet = TRUE,
          use_cache = TRUE,
          vectors = NULL
        )
        tmp[, c("GeoUID", "geometry"), drop = FALSE]
      },
      error = function(e) {
        pr_codes <- get_pr_codes(ds)
        pieces <- lapply(pr_codes, function(code) {
          tmp <- cancensus::get_census(
            dataset = ds,
            regions = list(PR = code),
            level = scale,
            geo_format = "sf",
            quiet = TRUE,
            use_cache = TRUE,
            vectors = NULL
          )
          tmp[, c("GeoUID", "geometry"), drop = FALSE]
        })
        do.call(rbind, pieces)
      }
    )
  }

  # Standardize names, apply Nunavut 1996 fix, and reproject if needed
  post_process <- function(x, scale, ds_label_for_nnvt) {
    names(x) <- c("ID", "geometry")

    # Nunavut fix for 1996 at PR level: merge 61+62 into 61
    if (identical(scale, "PR") && identical(ds_label_for_nnvt, "CA1996")) {
      geoms_to_merge <- x[x$ID %in% c(61, 62), ]
      if (nrow(geoms_to_merge) == 2) {
        combined_geometry <- sf::st_union(geoms_to_merge)
        new_feature <- sf::st_sf(ID = 61, geometry = combined_geometry)
        x <- x[!x$ID %in% c(61, 62), ]
        x <- rbind(x, new_feature)
      }
    }

    # Reproject only when CRS is not already EPSG:3347
    crs_now <- sf::st_crs(x)
    if (is.na(crs_now$epsg) || crs_now$epsg != 3347) {
      x <- sf::st_transform(x, 3347)
    }
    x
  }

  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("Package 'mirai' is required for mirai_map().")
  }
  if (!requireNamespace("promises", quietly = TRUE)) {
    stop(
      "Package 'promises' is required to keep progressr progress updates with mirai_map(.promise=...)."
    )
  }

  # Progress: 1 step for C/PR, else one per year
  total_steps <- sum(ifelse(
    census_scales %in% c("C", "PR"),
    1L,
    length(census_years)
  ))

  jobs <-
    expand.grid(
      job_type = "year",
      scale = census_scales,
      i = seq_along(census_dataset),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

  mp <- mirai::mirai_map(
    jobs,
    function(job_type, scale, i) {
      ds <- census_dataset[i]
      if (ds == "CA21" && scale == "DA") {
        from_bucket <- bucket_read_object_zip_shp(
          "DA2021_carto.zip",
          "curbcut.rawdata"
        )
        from_bucket <- from_bucket[c("DAUID")]
        names(from_bucket)[1] <- "ID"
        from_bucket[, c("ID", "geometry"), drop = FALSE]
        return(list(
          kind = "year",
          scale = scale,
          year = as.character(census_years[i]),
          value = from_bucket[,
            c("ID", "geometry"),
            drop = FALSE
          ]
        ))
      }
      out <- fetch_canada_then_pr(ds, scale)
      out <- post_process(out, scale, ds)
      list(
        kind = "year",
        scale = scale,
        year = as.character(census_years[i]),
        value = out
      )
    },
    # keep progressr updates in the main process as each task resolves
    latest_census = latest_census,
    census_dataset = census_dataset,
    census_years = census_years,
    fetch_canada_then_pr = fetch_canada_then_pr,
    post_process = post_process,
    bucket_read_object_zip_shp = bucket_read_object_zip_shp
  )

  res <- mp[.progress] # collect all results (waits)

  # Hard-fail if any mirai returned an errorValue (miraiError, timeout, interrupt, etc.)
  bad <- vapply(res, mirai::is_error_value, logical(1))
  if (any(bad)) {
    idx <- which(bad)
    msg <- paste0(
      "mirai_map had ",
      length(idx),
      " failure(s). First failure:\n",
      "index=",
      idx[1],
      "\n",
      as.character(res[[idx[1]]])
    )
    stop(msg, call. = FALSE)
  }

  # Assemble final out_by_scale
  out_by_scale <- setNames(
    vector("list", length(census_scales)),
    census_scales
  )

  # seed with static outputs
  for (x in res) {
    if (identical(x$kind, "static")) {
      out_by_scale[[x$scale]] <- x$by_year
    }
  }

  # ensure non-static scales have list containers
  for (s in census_scales) {
    if (is.null(out_by_scale[[s]])) {
      out_by_scale[[s]] <- setNames(
        vector("list", length(census_years)),
        as.character(census_years)
      )
    }
  }

  # fill year outputs
  for (x in res) {
    if (identical(x$kind, "year")) {
      out_by_scale[[x$scale]][[x$year]] <- x$value
    }
  }

  out_by_scale
}
