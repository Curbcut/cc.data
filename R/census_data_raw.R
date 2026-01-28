#' Download census vectors and join to geometries for specific variables.
#'
#' @param empty_geometries list [[scale]][[year]] sf(ID, geometry) in target CRS
#' @param census_scales    character, e.g., c("C","PR","CMA","CSD","CT","DA")
#' @param census_years     numeric, e.g., c(1996, 2001, ..., 2021)
#' @param var_codes        character|NULL subset of var_code; NULL = all (+parents)
#' @return list[[scale]][[year]] sf(ID, geometry, <vars>)
#' @export
census_data_raw <- function(
  empty_geometries,
  census_scales = cc.data::census_scales,
  census_years = cc.data::census_years,
  var_codes = NULL
) {
  stopifnot(length(census_scales) > 0L, length(census_years) > 0L)

  # --- datasets and vectors reference ---
  census_dataset <- paste0("CA", sub("^20", "", as.character(census_years)))
  ref <- cc.data::census_vectors_table

  # build target var_code set (include parents for consistency)
  if (is.null(var_codes)) {
    selected_varcodes <- unique(ref$var_code)
  } else {
    var_codes <- unique(as.character(var_codes))
    parents <- ref$parent_vec[match(var_codes, ref$var_code)]
    parents <- parents[!is.na(parents)]
    parents <- intersect(parents, ref$var_code)
    selected_varcodes <- unique(c(var_codes, parents))
  }

  # cache table (vectors rows only)
  cache_tbl <- cancensus::list_cancensus_cache()
  cache_tbl <- cache_tbl[!is.na(cache_tbl$vectors), , drop = FALSE]

  # memoized PR list per dataset
  pr_cache <- new.env(parent = emptyenv())
  get_pr_codes <- function(ds) {
    key <- paste0("pr_", ds)
    if (!exists(key, envir = pr_cache, inherits = FALSE)) {
      regs <- cancensus::list_census_regions(ds, quiet = TRUE)
      prs <- regs$region[regs$level == "PR"]
      assign(key, prs, envir = pr_cache)
    }
    get(key, envir = pr_cache, inherits = FALSE)
  }

  # retry helper (handles transient errors)
  with_retries <- function(
    expr,
    times = 3L,
    sleep = c(1, 2, 4),
    what = "request"
  ) {
    last_err <- NULL
    for (i in seq_len(times)) {
      res <- try(force(expr), silent = TRUE)
      if (!inherits(res, "try-error")) {
        return(res)
      }
      last_err <- attr(res, "condition")
      if (i < times) Sys.sleep(sleep[i])
    }
    stop(
      sprintf(
        "Failed %s after %d retries: %s",
        what,
        times,
        conditionMessage(last_err)
      ),
      call. = FALSE
    )
  }

  # cache-aware fetch: full block if cached; else per-vector, full-join on GeoUID
  get_census_data <- function(
    census_dataset,
    var_codes,
    region,
    scale,
    cache,
    warn_empty = FALSE
  ) {
    stopifnot(length(var_codes) > 0)

    cache_has_request <- function(
      cache_df,
      dataset,
      level,
      regions,
      var_codes
    ) {
      if (is.null(cache_df) || !nrow(cache_df)) {
        return(FALSE)
      }
      vc <- unname(var_codes)
      same_vecset <- function(js) {
        if (is.na(js)) FALSE else setequal(jsonlite::fromJSON(js), vc)
      }
      cand <- cache_df[
        vapply(cache_df$vectors, same_vecset, logical(1L)),
        ,
        drop = FALSE
      ]
      if (!nrow(cand)) {
        return(FALSE)
      }
      cand <- cand[
        cand$dataset == dataset & cand$level == level,
        ,
        drop = FALSE
      ]
      if (!nrow(cand)) {
        return(FALSE)
      }
      same_reg <- function(js) identical(jsonlite::fromJSON(js), regions)
      any(vapply(cand$regions, same_reg, logical(1L)))
    }

    normalize_piece <- function(df, out_name) {
      df <- df[, c("GeoUID", out_name), drop = FALSE]
      df$GeoUID <- as.character(df$GeoUID)
      df
    }

    if (
      cache_has_request(cache, census_dataset, scale, region, unname(var_codes))
    ) {
      out <- with_retries(
        cancensus::get_census(
          dataset = census_dataset,
          regions = region,
          level = scale,
          vectors = var_codes,
          geo_format = NA,
          quiet = TRUE,
          use_cache = TRUE
        ),
        what = "get_census cached (block)"
      )
      out <- out[, c("GeoUID", names(var_codes)), drop = FALSE]
      out$GeoUID <- as.character(out$GeoUID)
      return(out)
    }

    pieces <- lapply(seq_along(var_codes), function(j) {
      nm <- names(var_codes)[j]
      df <- with_retries(
        cancensus::get_census(
          dataset = census_dataset,
          regions = region,
          level = scale,
          vectors = var_codes[j],
          geo_format = NA,
          quiet = TRUE,
          use_cache = TRUE
        ),
        what = sprintf("get_census vec (%s)", nm)
      )
      if (nrow(df) == 0L) {
        if (warn_empty) {
          warning(sprintf(
            "Vector %s returned 0 rows (dataset=%s, scale=%s).",
            nm,
            census_dataset,
            scale
          ))
        }
        df <- data.frame(
          GeoUID = character(0),
          setNames(list(numeric(0)), nm),
          check.names = FALSE
        )
      } else {
        df <- normalize_piece(df, nm)
      }
      df
    })

    Reduce(function(x, y) merge(x, y, all = TRUE, by = "GeoUID"), pieces)
  }
  # coalesce helper in case of duplicated GeoUID after PR stacking (DA)
  coalesce_by_id <- function(df, id_col = "GeoUID") {
    sp <- split(df, df[[id_col]])
    rows <- lapply(names(sp), function(k) {
      d <- sp[[k]]
      out <- d[1, , drop = FALSE]
      for (nm in setdiff(names(d), id_col)) {
        v <- d[[nm]]
        idx <- which(!is.na(v))[1]
        out[[nm]] <- if (length(idx)) v[idx] else NA
      }
      out[[id_col]] <- k
      out
    })
    do.call(rbind, rows)
  }

  #  Download attributes
  progressr::with_progress({
    pb <- progressr::progressor(
      steps = length(census_scales) * length(census_years)
    )

    jobs <- expand.grid(
      scale = census_scales,
      i_year = seq_along(census_years),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    mp <- mirai::mirai_map(
      jobs,
      function(scale, i_year) {
        year_chr <- as.character(census_years[i_year])
        ds <- census_dataset[i_year]

        colname <- paste0("vec_", year_chr)
        vecs_tbl <- ref[
          !is.na(ref[[colname]]),
          c("var_code", colname),
          drop = FALSE
        ]
        if (!is.null(var_codes)) {
          vecs_tbl <- vecs_tbl[
            vecs_tbl$var_code %in% selected_varcodes,
            ,
            drop = FALSE
          ]
        }

        raw_codes <- vecs_tbl[[colname]]
        names(raw_codes) <- vecs_tbl$var_code
        raw_codes <- raw_codes[!is.na(raw_codes)]

        # name as var___i for multi-code variables
        expanded <- mapply(
          function(vec, nm) {
            v <- unname(vec)
            names(v) <- paste0(nm, "___", seq_along(v))
            v
          },
          raw_codes,
          names(raw_codes),
          USE.NAMES = FALSE
        )
        expanded <- unlist(expanded, use.names = TRUE)

        # split into lots when same code maps to different names
        dup_codes <- duplicated(expanded) |
          duplicated(expanded, fromLast = TRUE)
        dup_names <- duplicated(names(expanded)) |
          duplicated(names(expanded), fromLast = TRUE)
        split_idx <- cumsum(dup_codes & !dup_names)
        named_vecs <- Map(
          stats::setNames,
          split(expanded, split_idx),
          split(names(expanded), split_idx)
        )

        # download per lot (Canada or per-PR at DA)
        lot_list <- lapply(named_vecs, function(vcodes_named) {
          if (scale == "DA") {
            prs <- get_pr_codes(ds)
            parts <- lapply(prs, function(pr) {
              get_census_data(
                census_dataset = ds,
                var_codes = vcodes_named,
                region = list(PR = pr),
                scale = scale,
                cache = cache_tbl
              )
            })
            dat_pr <- do.call(rbind, parts)
            if (any(duplicated(dat_pr$GeoUID))) {
              dat_pr <- coalesce_by_id(dat_pr, "GeoUID")
            }
            dat_pr
          } else {
            get_census_data(
              census_dataset = ds,
              var_codes = vcodes_named,
              region = list(C = "01"),
              scale = scale,
              cache = cache_tbl
            )
          }
        })

        # merge lots (full join on GeoUID), then collapse ___i sums
        if (length(lot_list) == 0L) {
          dat <- data.frame(ID = character(), check.names = FALSE)
        } else {
          dat <- Reduce(
            function(x, y) merge(x, y, all = TRUE, by = "GeoUID"),
            lot_list
          )
          names(dat)[1] <- "ID"
          names(dat) <- sub("\\.(x|y)(\\.\\d+)?$", "", names(dat)) # safeguard

          tb <- table(gsub("___[0-9]+$", "", names(dat)))
          roots_multi <- setdiff(names(tb[tb > 1]), "ID")

          if (length(roots_multi)) {
            for (root in roots_multi) {
              idx <- grepl(paste0("^", root, "___[0-9]+$"), names(dat))
              dat[[root]] <- if (!any(idx)) {
                NA_real_
              } else {
                rowSums(dat[, idx, drop = FALSE], na.rm = TRUE)
              }
              dat <- dat[, !idx, drop = FALSE]
            }
          }
          names(dat) <- gsub("___[0-9]+$", "", names(dat))
        }

        list(
          kind = "year",
          scale = scale,
          year = year_chr,
          value = dat
        )
      },
      census_years = census_years,
      census_dataset = census_dataset,
      ref = ref,
      var_codes = var_codes,
      selected_varcodes = selected_varcodes,
      get_pr_codes = get_pr_codes,
      get_census_data = get_census_data,
      cache_tbl = cache_tbl,
      coalesce_by_id = coalesce_by_id,
      with_retries = with_retries,
      pr_cache = pr_cache
    )

    res <- mp[] # collect all results (waits)

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

    data_raw <- setNames(
      vector("list", length(census_scales)),
      census_scales
    )
    for (s in census_scales) {
      data_raw[[s]] <- setNames(
        vector("list", length(census_years)),
        as.character(census_years)
      )
    }

    for (x in res) {
      if (identical(x$kind, "year")) {
        data_raw[[x$scale]][[x$year]] <- x$value
        pb()
      }
    }

    #  Join to geometries
    # keep sf/CRS and align by ID
    out_sf <- mapply(
      function(d_by_year, e_by_year) {
        mapply(
          function(d, e) {
            if (!identical(typeof(d$ID), typeof(e$ID))) {
              d$ID <- as.character(d$ID)
              e$ID <- as.character(e$ID)
            }
            idx <- match(e$ID, d$ID)
            attrs <- setdiff(names(d), "ID")
            res <- e
            if (length(attrs)) {
              res <- cbind(res, d[idx, attrs, drop = FALSE])
            }
            res
          },
          d_by_year,
          e_by_year,
          SIMPLIFY = FALSE,
          USE.NAMES = TRUE
        )
      },
      data_raw,
      empty_geometries,
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )

    out_sf
  })
}


#' Get Census Data (robust cache check + retries; result-compatible)
#'
#' Retrieves census data for the given region/scale. If the full vector set
#' is cached, fetch once; otherwise fetch per-vector and full-join on GeoUID.
#' Uses set-equality for cache detection (order-insensitive) and simple retries.
#'
#' @param census_dataset <character> e.g., "CA2021"
#' @param var_codes      <named character> names are output column names
#' @param region         <named list> e.g., list(C = "01")
#' @param scale          <character> level, e.g., "CMA"
#' @param cache          <data.frame> cancensus::list_cancensus_cache()
#' @param warn_empty     <logical> warn if a vector returns 0 rows
#' @return data.frame with columns GeoUID + names(var_codes)
get_census_data <- function(
  census_dataset,
  var_codes,
  region,
  scale,
  cache = cancensus::list_cancensus_cache(),
  warn_empty = TRUE
) {
  stopifnot(length(var_codes) > 0)

  # retry helper (light backoff)
  with_retries <- function(
    expr,
    times = 3L,
    sleep = c(1, 2, 4),
    what = "request"
  ) {
    last_err <- NULL
    for (i in seq_len(times)) {
      res <- try(force(expr), silent = TRUE)
      if (!inherits(res, "try-error")) {
        return(res)
      }
      last_err <- attr(res, "condition")
      if (i < times) Sys.sleep(sleep[i])
    }
    stop(
      sprintf(
        "Failed %s after %d retries: %s",
        what,
        times,
        conditionMessage(last_err)
      ),
      call. = FALSE
    )
  }

  # cache check using set-equality of vector sets (order-insensitive)
  cache_has_request <- function(cache_df, dataset, level, regions, var_codes) {
    if (is.null(cache_df) || !nrow(cache_df)) {
      return(FALSE)
    }
    vc <- unname(var_codes)
    same_vecset <- function(js) {
      if (is.na(js)) FALSE else setequal(jsonlite::fromJSON(js), vc)
    }
    cand <- cache_df[
      vapply(cache_df$vectors, same_vecset, logical(1L)),
      ,
      drop = FALSE
    ]
    if (!nrow(cand)) {
      return(FALSE)
    }
    cand <- cand[cand$dataset == dataset & cand$level == level, , drop = FALSE]
    if (!nrow(cand)) {
      return(FALSE)
    }
    same_reg <- function(js) identical(jsonlite::fromJSON(js), regions)
    any(vapply(cand$regions, same_reg, logical(1L)))
  }

  # normalize a piece: keep GeoUID + expected column, fix names
  normalize_piece <- function(df, out_name) {
    # keep only GeoUID + target, drop if absent (let it error early)
    df <- df[, c("GeoUID", out_name), drop = FALSE]
    # enforce types compatible with merge; keep GeoUID as character like cancensus default
    df$GeoUID <- as.character(df$GeoUID)
    df
  }

  # path 1: full set is cached -> single call
  if (
    cache_has_request(cache, census_dataset, scale, region, unname(var_codes))
  ) {
    out <- with_retries(
      cancensus::get_census(
        dataset = census_dataset,
        regions = region,
        level = scale,
        vectors = var_codes,
        geo_format = NA,
        quiet = TRUE,
        use_cache = TRUE
      ),
      what = "get_census cached (block)"
    )
    # keep column order stable
    out <- out[, c("GeoUID", names(var_codes)), drop = FALSE]
    out$GeoUID <- as.character(out$GeoUID)
    return(out)
  }

  # path 2: fetch per vector, then full outer join by GeoUID (merge all=TRUE)
  pieces <- lapply(seq_along(var_codes), function(j) {
    nm <- names(var_codes)[j]
    df <- with_retries(
      cancensus::get_census(
        dataset = census_dataset,
        regions = region,
        level = scale,
        vectors = var_codes[j],
        geo_format = NA,
        quiet = TRUE,
        use_cache = TRUE
      ),
      what = sprintf("get_census vec (%s)", nm)
    )

    if (nrow(df) == 0L) {
      if (warn_empty) {
        warning(sprintf(
          "Vector %s returned 0 rows (dataset=%s, scale=%s).",
          nm,
          census_dataset,
          scale
        ))
      }
      # keep structure (avoid changing result shape): return empty with cols present
      df <- data.frame(
        GeoUID = character(0),
        setNames(list(numeric(0)), nm),
        check.names = FALSE
      )
    } else {
      df <- normalize_piece(df, nm)
    }
    df
  })

  # exact same join semantics as before (full outer by GeoUID)
  out <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "GeoUID"), pieces)

  # keep stable column order
  out <- out[, c("GeoUID", names(var_codes)), drop = FALSE]
  out
}
