#' Download CMM canopy tiles for a given year
#'
#' Downloads all ZIP archives of the canopy index for a given year from the
#' Observatoire de la CMM web page, unzips them under \code{out_dir}, and
#' deletes the ZIP files.
#'
#' Internally, the function scrapes the HTML page and keeps only URLs whose
#' filename matches \code{IndiceCanopee_<year>_TIF.zip}.
#'
#' @param year Integer. Canopy index year (e.g., 2023).
#' @param out_dir Character. Local directory where the unzipped folders will
#'   be stored. Created if it does not exist.
#'
#' @return Invisibly, a character vector of unzipped directory paths (one per
#'   ZIP archive).
#' @export
canopy_download_cmm <- function(
    year    = 2023L,
    out_dir = "data/cmm_canopy"
) {
  overwrite  <- FALSE
  unzip_arch <- TRUE
  remove_zip <- TRUE
  
  page_url <- "https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#indice_canopee"
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1) Fetch HTML --------------------------------------------------------
  resp <- httr::GET(
    page_url,
    httr::user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    ),
    httr::accept("text/html")
  )
  
  if (httr::status_code(resp) != 200L) {
    stop(
      "HTTP error ", httr::status_code(resp),
      " when fetching ", page_url, ".",
      call. = FALSE
    )
  }
  
  html_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  doc      <- xml2::read_html(html_txt)
  
  # 2) Extract links and keep canopy ZIPs for the requested year --------
  hrefs <- rvest::html_elements(doc, "a") |>
    rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  
  zip_pattern <- sprintf("IndiceCanopee_%d_TIF\\.zip$", year)
  zip_urls    <- unique(hrefs[grepl(zip_pattern, hrefs)])
  
  if (!length(zip_urls)) {
    stop("No canopy ZIP links found for year ", year, ".", call. = FALSE)
  }
  
  # make URLs absolute
  zip_urls <- xml2::url_absolute(zip_urls, page_url)
  message("Found ", length(zip_urls), " canopy ZIP files for year ", year, ".")
  
  unzipped_dirs <- character(0)
  
  # 3) Download / unzip / delete ZIP ------------------------------------
  for (u in zip_urls) {
    zip_name <- basename(u)
    dest_zip <- file.path(out_dir, zip_name)
    
    message(">>> Downloading: ", zip_name)
    
    if (!file.exists(dest_zip) || isTRUE(overwrite)) {
      curl::curl_download(u, destfile = dest_zip, quiet = FALSE)
    } else {
      message("    (already exists, skipping download)")
    }
    
    if (isTRUE(unzip_arch)) {
      unzip_dir <- file.path(
        out_dir,
        tools::file_path_sans_ext(zip_name)
      )
      dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(dest_zip, exdir = unzip_dir)
      unzipped_dirs <- c(unzipped_dirs, unzip_dir)
    }
    
    if (isTRUE(remove_zip) && file.exists(dest_zip)) {
      unlink(dest_zip)
    }
  }
  
  invisible(unzipped_dirs)
}


#' Load and mosaic CMM canopy tiles
#'
#' Given a directory containing all unzipped canopy TIFF folders/files
#' from the CMM, this function:
#'   * finds all .tif files recursively
#'   * reads them as SpatRaster tiles
#'   * drops invalid tiles
#'   * checks that all tiles share the same CRS
#'   * mosaics them into a single SpatRaster
#'   * optionally writes the mosaic to disk
#'
#' @param tif_dir Character. Root directory where all unzipped canopy TIFF
#'   tiles are stored.
#' @param out_raster Optional character path to a raster file (e.g. ".tif")
#'   where the mosaic will be written using \code{terra::writeRaster()}.
#'   If \code{NULL}, nothing is written.
#' @param overwrite Logical. If \code{TRUE}, overwrite \code{out_raster}
#'   if it already exists.
#'
#' @return A \code{SpatRaster} mosaic of all valid canopy tiles.
#' @export
canopy_load_tiles <- function(
    tif_dir,
    out_raster = NULL,
    overwrite  = FALSE
) {
  stopifnot(dir.exists(tif_dir))
  
  # 1) List all TIFF files ------------------------------------------------
  tif_files <- list.files(
    tif_dir,
    pattern    = "\\.tif$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  if (length(tif_files) == 0L) {
    stop("No .tif files found in 'tif_dir'.", call. = FALSE)
  }
  
  # 2) Read all tiles, ignore problematic ones ---------------------------
  tiles <- lapply(tif_files, function(f) {
    r <- try(terra::rast(f), silent = TRUE)
    if (inherits(r, "try-error")) {
      message("Ignoring problematic TIFF: ", f)
      return(NULL)
    }
    r
  })
  
  tiles <- Filter(Negate(is.null), tiles)
  
  if (length(tiles) == 0L) {
    stop("No valid raster tiles could be read.", call. = FALSE)
  }
  
  # 3) Basic CRS check ----------------------------------------------------
  crs0 <- terra::crs(tiles[[1]])
  same_crs <- vapply(
    tiles,
    function(r) identical(terra::crs(r), crs0),
    logical(1)
  )
  
  if (!all(same_crs)) {
    stop("Not all tiles share the same CRS.", call. = FALSE)
  }
  
  # 4) Mosaic all tiles into a single SpatRaster --------------------------
  canopy <- do.call(terra::mosaic, tiles)
  
  # 5) Optionally write mosaic to disk ------------------------------------
  if (!is.null(out_raster)) {
    terra::writeRaster(
      canopy,
      filename  = out_raster,
      overwrite = overwrite
    )
  }
  
  canopy
}



#' Compute canopy shares by census geography for Montreal CMA
#'
#' Uses a pre-built canopy SpatRaster (already mosaicked) and returns,
#' for each requested census level, the share of area in canopy classes 1–4.
#'
#' Canopy classes are:
#'   1 = mineral low  (NDVI < 0.3,  MNH < 3 m)
#'   2 = mineral high (NDVI < 0.3,  MNH >= 3 m)
#'   3 = veg low      (NDVI >= 0.3, MNH < 3 m)
#'   4 = veg high     (NDVI >= 0.3, MNH >= 3 m) = tree canopy
#'
#' @param canopy SpatRaster with canopy classes coded 1–5
#'   (where 5 corresponds to "no canopy / other / excluded").
#' @param levels Character vector of census levels. Any of:
#'   "DB", "DA", "CT", "CSD", "CMA".
#' @param chunk_size Integer. Number of polygons per batch in
#'   \code{terra::extract()} for levels other than CMA.
#'   Smaller values use less memory but are slower.
#'
#' @return Named list of sf objects, one per requested level, each including:
#'   \code{id}, the four canopy share variables, and \code{geometry}.
#' @export
canopy_get_shares <- function(
    canopy,
    levels     = c("DB", "DA", "CT", "CSD", "CMA"),
    chunk_size = 200L
) {
  stopifnot(inherits(canopy, "SpatRaster"))
  
  # Internal constants (not exposed as arguments)
  cma_uid  <- "24462"  # Montreal CMA
  dataset  <- "CA21"   # 2021 census
  agg_fact <- 10L      # larger = coarser raster = lighter in memory
  
  # Names for the four canopy share variables
  canopy_names <- c(
    "canopy_mineral_low",   # class 1
    "canopy_mineral_high",  # class 2
    "canopy_veg_low",       # class 3
    "canopy_veg_high"       # class 4
  )
  
  ## 0) Clip canopy raster to Montreal CMA -------------------------------
  cma_sf <- cancensus::get_census(
    dataset    = dataset,
    regions    = list(CMA = cma_uid),
    level      = "CMA",
    use_cache  = TRUE,
    geo_format = "sf",
    quiet      = TRUE
  )
  
  cma_sf   <- sf::st_transform(cma_sf, terra::crs(canopy, proj = TRUE))
  cma_vect <- terra::vect(cma_sf)
  
  canopy_cma <- terra::crop(canopy, cma_vect)
  
  ## 1) Optional aggregation to reduce raster size -----------------------
  if (!is.null(agg_fact) && agg_fact > 1L) {
    message("Aggregating canopy raster with factor = ", agg_fact, " ...")
    canopy_cma <- terra::aggregate(
      canopy_cma,
      fact = agg_fact,
      fun  = "modal",
      na.rm = TRUE
    )
  }
  
  ## 2) Helper to compute shares of canopy categories 1–4 ----------------
  cat_fun <- function(vals, ...) {
    # Drop NA and category 5 (no canopy / other)
    vals <- vals[!is.na(vals) & vals %in% 1:4]
    
    out <- numeric(4L)
    names(out) <- canopy_names
    
    if (!length(vals)) {
      # If no valid pixels, return 0 for all categories
      return(out)
    }
    
    tab  <- table(factor(vals, levels = 1:4))
    prop <- as.numeric(tab) / sum(tab)
    names(prop) <- canopy_names
    prop
  }
  
  ## 3) Loop over requested census levels --------------------------------
  levels    <- unique(levels)
  res_list  <- vector("list", length(levels))
  names(res_list) <- levels
  
  for (lvl in levels) {
    message("Processing level: ", lvl, " ...")
    
    # Download geometries for this level inside Montreal CMA
    geo_sf <- cancensus::get_census(
      dataset    = dataset,
      regions    = list(CMA = cma_uid),
      level      = lvl,
      use_cache  = TRUE,
      geo_format = "sf",
      quiet      = TRUE
    )
    
    geo_sf   <- sf::st_transform(geo_sf, terra::crs(canopy_cma, proj = TRUE))
    geo_vect <- terra::vect(geo_sf)
    
    n <- nrow(geo_sf)
    if (n == 0L) {
      warning("No geometries found for level ", lvl, ". Skipping.")
      res_list[[lvl]] <- geo_sf
      next
    }
    
    ## Special case: CMA = single polygon --------------------------------
    if (lvl == "CMA") {
      ext <- terra::extract(
        canopy_cma,
        geo_vect,
        fun = cat_fun
      )
      
      ext_df <- tibble::as_tibble(ext)[, -1, drop = FALSE]
      
      # Force column names for canopy shares
      if (ncol(ext_df) == 4L) {
        names(ext_df) <- canopy_names
      }
      
      out <- dplyr::bind_cols(geo_sf, ext_df)
      
      res_list[[lvl]] <- out |>
        dplyr::select(
          id = GeoUID,
          dplyr::all_of(canopy_names),
          geometry
        )
      
      next
    }
    
    ## Other levels: process in chunks to control memory -----------------
    idx_chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
    
    chunk_results <- lapply(idx_chunks, function(idx) {
      ext <- terra::extract(
        canopy_cma,
        geo_vect[idx, ],
        fun = cat_fun
      )
      
      ext_df <- tibble::as_tibble(ext)[, -1, drop = FALSE]
      
      if (ncol(ext_df) == 4L) {
        names(ext_df) <- canopy_names
      }
      
      ext_df
    })
    
    ext_all <- dplyr::bind_rows(chunk_results)
    
    if (nrow(ext_all) != nrow(geo_sf)) {
      stop(
        "Mismatch between number of polygons and extracted rows for level ",
        lvl, ".", call. = FALSE
      )
    }
    
    out <- dplyr::bind_cols(geo_sf, ext_all)
    
    res_list[[lvl]] <- out |>
      dplyr::select(
        id = GeoUID,
        dplyr::all_of(canopy_names),
        geometry
      )
  }
  
  res_list
}

#' Download CMM forest cover tiles for a given year
#'
#' Downloads all ZIP archives of the forest cover (Couvert forestier) for a
#' given year from the Observatoire de la CMM web page, unzips them under
#' \code{out_dir}, then recursively unzips all internal ZIPs (one per
#' municipality).
#'
#' @param year Integer. Forest cover year (e.g., 2023).
#' @param out_dir Character. Local directory where the unzipped folders will
#'   be stored. Created if it does not exist.
#'
#' @return Invisibly, a character vector of top-level unzipped directory paths.
#' @export
forest_download_cmm <- function(
    year    = 2023L,
    out_dir = "data/cmm_forest_cover"
) {
  overwrite  <- FALSE
  unzip_arch <- TRUE
  remove_zip <- TRUE
  
  page_url <- "https://observatoire.cmm.qc.ca/produits/donnees-georeferencees/#couvert_forestier"
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1) Fetch HTML --------------------------------------------------------
  resp <- httr::GET(
    page_url,
    httr::user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    ),
    httr::accept("text/html")
  )
  
  if (httr::status_code(resp) != 200L) {
    stop(
      "HTTP error ", httr::status_code(resp),
      " when fetching ", page_url, ".",
      call. = FALSE
    )
  }
  
  html_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  doc      <- xml2::read_html(html_txt)
  
  # 2) Extract links and keep forest cover ZIPs for the requested year ---
  hrefs <- rvest::html_elements(doc, "a") |>
    rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  
  zip_pattern <- sprintf("CouvertForestier_%d_SHP\\.zip$", year)
  zip_urls    <- unique(hrefs[grepl(zip_pattern, hrefs)])
  
  if (!length(zip_urls)) {
    stop("No forest cover ZIP links found for year ", year, ".", call. = FALSE)
  }
  
  zip_urls <- xml2::url_absolute(zip_urls, page_url)
  message("Found ", length(zip_urls), " forest cover ZIP files for year ", year, ".")
  
  unzipped_dirs <- character(0)
  
  # 3) Download / unzip top-level ZIPs -----------------------------------
  for (u in zip_urls) {
    zip_name <- basename(u)
    dest_zip <- file.path(out_dir, zip_name)
    
    message(">>> Downloading: ", zip_name)
    
    if (!file.exists(dest_zip) || isTRUE(overwrite)) {
      curl::curl_download(u, destfile = dest_zip, quiet = FALSE)
    } else {
      message("    (already exists, skipping download)")
    }
    
    unzip_dir <- file.path(
      out_dir,
      tools::file_path_sans_ext(zip_name)
    )
    
    if (isTRUE(unzip_arch)) {
      dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(dest_zip, exdir = unzip_dir)
      unzipped_dirs <- c(unzipped_dirs, unzip_dir)
    }
    
    if (isTRUE(remove_zip) && file.exists(dest_zip)) {
      unlink(dest_zip)
    }
  }
  
  # 4) Recursively unzip internal ZIPs (one per municipality) ------------
  inner_zips <- list.files(
    out_dir,
    pattern    = "\\.zip$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  if (length(inner_zips)) {
    message("Unzipping ", length(inner_zips), " internal municipal ZIP files ...")
    
    for (z in inner_zips) {
      utils::unzip(z, exdir = dirname(z))
      if (isTRUE(remove_zip)) {
        unlink(z)
      }
    }
  }
  
  invisible(unzipped_dirs)
}


#' Load and merge CMM forest cover polygons
#'
#' Given a directory containing all unzipped forest cover SHP folders/files
#' from the CMM, this function lists all .shp (case-insensitive), reads them
#' as sf, and merges them into a single sf object.
#'
#' @param shp_dir Character. Root directory where all unzipped forest cover
#'   shapefiles are stored.
#'
#' @return An \code{sf} object with all forest cover polygons.
#' @export
forest_load_polygons <- function(
    shp_dir
) {
  stopifnot(dir.exists(shp_dir))
  
  # 1) Tous les .shp (insensible à la casse) -----------------------------
  shp_files <- list.files(
    shp_dir,
    pattern    = "\\.[sS][hH][pP]$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  if (!length(shp_files)) {
    stop("No .shp files found in 'shp_dir'.", call. = FALSE)
  }
  
  # 2) Ne garder que les shapefiles agrégés (3 chiffres au début) --------
  bn_low <- tolower(basename(shp_files))
  
  keep <- grepl(
    "^\\d{3}_couvertforestier_\\d{4}\\.shp$",
    bn_low
  )
  
  shp_files <- shp_files[keep]
  
  if (!length(shp_files)) {
    stop(
      "No top-level forest cover shapefiles (###_CouvertForestier_YYYY.shp) found.",
      call. = FALSE
    )
  }
  
  # 3) Lecture des couches -----------------------------------------------
  layers <- lapply(shp_files, function(f) {
    s <- try(sf::st_read(f, quiet = TRUE), silent = TRUE)
    if (inherits(s, "try-error")) {
      message("Ignoring problematic SHP: ", f)
      return(NULL)
    }
    s
  })
  
  layers <- Filter(Negate(is.null), layers)
  
  if (!length(layers)) {
    stop("No valid forest cover layers could be read.", call. = FALSE)
  }
  
  forest <- suppressMessages(do.call(rbind, layers))
  
  forest <- sf::st_make_valid(forest)
  
  # CMM forest cover = NAD83(CSRS) / MTM zone 8 (EPSG:2950)
  if (is.na(sf::st_crs(forest))) {
    forest <- sf::st_set_crs(forest, 2950)
  }
  
  forest
}


#' Compute forest cover share by census geography for Montreal CMA
#'
#' Uses a forest cover layer (polygons, sf) and returns, for each requested
#' census level, the share of polygon area covered by the forest cover.
#'
#' @param forest sf object with forest cover polygons.
#' @param levels Character vector of census levels. Any of:
#'   "DB", "DA", "CT", "CSD", "CMA".
#' @param chunk_size Integer. Number of polygons per batch in the
#'   intersection with the forest cover. Smaller values use less memory
#'   but are slower.
#'
#' @return Named list of sf objects, one per requested level, each including:
#'   \code{id}, \code{forest_cover_share}, and \code{geometry}.
#' @export
forest_get_shares <- function(
    forest,
    levels     = c("DB", "DA", "CT", "CSD", "CMA"),
    chunk_size = 200L
) {
  if (!inherits(forest, "sf")) {
    stop("'forest' must be an sf object.", call. = FALSE)
  }
  
  # Internal constants ---------------------------------------------------
  cma_uid <- "24462"  # Montreal CMA
  dataset <- "CA21"   # 2021 census
  
  forest_sf <- sf::st_make_valid(forest)
  
  if (is.na(sf::st_crs(forest_sf))) {
    forest_sf <- sf::st_set_crs(forest_sf, 2950)
  }
  
  forest_crs <- sf::st_crs(forest_sf)
  
  levels   <- unique(levels)
  res_list <- vector("list", length(levels))
  names(res_list) <- levels
  
  for (lvl in levels) {
    message("Processing level: ", lvl, " ...")
    
    geo_sf <- cancensus::get_census(
      dataset    = dataset,
      regions    = list(CMA = cma_uid),
      level      = lvl,
      use_cache  = TRUE,
      geo_format = "sf",
      quiet      = TRUE
    )
    
    if (!nrow(geo_sf)) {
      warning("No geometries found for level ", lvl, ". Skipping.")
      res_list[[lvl]] <- geo_sf
      next
    }
    
    geo_sf <- sf::st_transform(geo_sf, forest_crs)
    geo_sf <- sf::st_make_valid(geo_sf)
    
    n <- nrow(geo_sf)
    
    area_total   <- as.numeric(sf::st_area(geo_sf))
    forest_share <- numeric(n)
    names(forest_share) <- geo_sf$GeoUID
    
    idx_chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
    
    for (idx in idx_chunks) {
      geo_chunk <- geo_sf[idx, ]
      
      inter <- suppressWarnings(
        sf::st_intersection(
          geo_chunk[, c("GeoUID")],
          forest_sf
        )
      )
      
      if (!nrow(inter)) {
        next
      }
      
      inter$area <- as.numeric(sf::st_area(inter))
      
      agg <- inter |>
        sf::st_drop_geometry() |>
        dplyr::group_by(GeoUID) |>
        dplyr::summarise(area = sum(area, na.rm = TRUE), .groups = "drop")
      
      idx_match <- match(agg$GeoUID, geo_sf$GeoUID)
      
      forest_share[idx_match] <-
        forest_share[idx_match] + agg$area / area_total[idx_match]
    }
    
    geo_sf$forest_cover_share <- forest_share
    
    res_list[[lvl]] <- geo_sf |>
      dplyr::select(
        id = GeoUID,
        forest_cover_share,
        geometry
      )
  }
  
  res_list
}

