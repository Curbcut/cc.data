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

  # Progress: 1 step for C/PR, else one per year
  total_steps <- sum(ifelse(
    census_scales %in% c("C", "PR"),
    1L,
    length(census_years)
  ))

  progressr::with_progress({
    pb <- progressr::progressor(steps = total_steps)

    out_by_scale <- future.apply::future_lapply(
      census_scales,
      FUN = function(scale) {
        # C and PR: fetch once (latest), then replicate across years
        if (scale %in% c("C", "PR")) {
          ds <- latest_census
          out <- fetch_canada_then_pr(ds, scale)
          out <- post_process(out, scale, "CA9999") # no Nunavut patch here
          pb()

          by_year <- setNames(
            rep(list(out), length(census_years)),
            as.character(census_years)
          )

          # Apply Nunavut patch only for 1996
          if (scale == "PR" && any(census_dataset == "CA1996")) {
            ykey <- as.character(census_years[which(
              census_dataset == "CA1996"
            )])
            by_year[[ykey]] <- post_process(by_year[[ykey]], "PR", "CA1996")
          }

          return(by_year)
        }

        # Other scales: parallelize across years
        by_year <- future.apply::future_lapply(
          seq_along(census_dataset),
          FUN = function(i) {
            ds <- census_dataset[i]
            out <- fetch_canada_then_pr(ds, scale)
            out <- post_process(out, scale, ds)
            pb()
            out
          },
          future.seed = NULL
        )
        names(by_year) <- as.character(census_years)
        by_year
      },
      future.seed = NULL
    )

    names(out_by_scale) <- census_scales
    out_by_scale
  })
}
