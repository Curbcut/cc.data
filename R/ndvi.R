#' Generate a Sequence of Years from 2013 to Current Year
#'
#' This function creates a sequence of years from 2013 to the current year.
#' It is used for NDVI import, as the first satellite images were from 2013.
#'
#' @return A numeric vector containing the sequence of years from 2013 to the current year.
#' @export
ndvi_years <- function() {
  2013:as.integer(format(Sys.Date(), "%Y"))
}

nasa_earthdata_auth <- function() {
  up <- Sys.getenv("USERPROFILE")
  if (up == "") {
    up <- Sys.getenv("HOME")
    if (up == "") {
      cat('USERPROFILE/HOME directories need to be set up. Please type Sys.setenv("HOME" = "YOURDIRECTORY") or Sys.setenv("USERPROFILE" = "YOURDIRECTORY") in your console and type your USERPROFILE/HOME directory instead of "YOURDIRECTORY". Next, run the code chunk again.')
      return()
    } else {
      Sys.setenv("userprofile" = up)
    }
  }
  Sys.setenv("HOME" = up)

  netrc_path <- file.path(up, ".netrc")

  if (!file.exists(netrc_path) || !any(grepl("urs.earthdata.nasa.gov", readLines(netrc_path)))) {
    username <- readline(prompt = "Enter NASA Earthdata Login Username: ")
    password <- readline(prompt = "Enter NASA Earthdata Login Password: ")

    netrc_conn <- file(netrc_path)
    writeLines(c("machine urs.earthdata.nasa.gov",
                 sprintf("login %s", username),
                 sprintf("password %s", password)), netrc_conn)
    close(netrc_conn)
  } else {
    lines <- readLines(netrc_path)
    for (i in seq_along(lines)) {
      if (lines[i] == "machine urs.earthdata.nasa.gov") {
        username <- strsplit(lines[i + 1], " ")[[1]][2]
        un <- readline(prompt = paste0("Is your NASA Earthdata Login Username: ", username, "\n\n Type yes or no."))
        if (tolower(un) == 'yes') {
          password <- readline(prompt = "Enter NASA Earthdata Login Password: ")
          lines[i + 2] <- sprintf("password %s", password)
        } else {
          username <- readline(prompt = "Enter NASA Earthdata Login Username: ")
          password <- readline(prompt = "Enter NASA Earthdata Login Password: ")
          lines[i + 1] <- sprintf("login %s", username)
          lines[i + 2] <- sprintf("password %s", password)
        }
        writeLines(lines, netrc_path)
        break
      }
    }
  }
}

#' Retrieve Items for a Specific Year at Growing Season
#'
#' This function retrieves collection items for a given year and limit within
#' the growing season (from May 1st to August 31st). The function first performs
#' an initial retrieval to determine the total number of pages, and then utilizes
#' parallel processing to fetch the required data.
#'
#' @param year <`numeric`> The year for which the HLS items are to be retrieved.
#' @param limit <`numeric`> Maximum number of items to retrieve per page.
#' Default is 25.
#' @param zone_bbox <`numeric vector`> Bounding box coordinates (xmin, ymin,
#' xmax, ymax) of the area to retrieve. EPSG4326
#' @param collections <`list`> Collections to get data from, e.g.
#' `list("HLSS30.v2.0", "HLSL30.v2.0")`
#'
#' @return A list of retrieved HLS items.
ndvi_get_items <- function(year, limit = 25, zone_bbox, collections) {

  # Define the URL for searching data
  search_URL <- 'https://cmr.earthdata.nasa.gov/stac/LPCLOUD/search'

  # Create datetime string for growing season
  zone_datetime <- sprintf('%s-05-01T00:00:00Z/%s-08-31T23:59:59Z', year, year)

  # First retrieval to know how many pages there are
  search_body <- list(limit = limit,
                      page = 1,
                      datetime = zone_datetime,
                      bbox = zone_bbox,
                      collections = collections)
  retrievals <- httr::RETRY("POST", search_URL, body = search_body, encode = "json") |>
    httr::content(as = "text") |>
    jsonlite::fromJSON()
  total_pages <- ceiling(retrievals$context$matched / limit) - 1
  retrievals <- list(retrievals)

  # Using future_lapply to parallelize the retrieval process
  if (total_pages > 0) {
      more_retrievals <- future.apply::future_lapply(seq_len(total_pages), \(page_number) {
        search_body <- list(limit = limit,
                            page = page_number,
                            datetime = zone_datetime,
                            bbox = zone_bbox,
                            collections = collections)
        search_req <- tryCatch(
          httr::RETRY("POST", search_URL, body = search_body, encode = "json") |>
            httr::content(as = "text") |>
            jsonlite::fromJSON()
          , error = function(e) return(NULL))
        return(search_req)
      })
      retrievals <- c(retrievals, more_retrievals)
  }

  return(retrievals)
}

#' Retrieve HLS Items for a Specific Year and Season
#'
#' This function retrieves Harmonized Landsat Sentinel-2 (HLS) items
#' for a given year and limit within the growing season (from May 1st to August
#' 31st). The function first performs an initial retrieval to determine the total
#' number of pages, and then utilizes parallel processing to fetch the required
#' data.
#'
#' @param year <`numeric`> The year for which the HLS items are to be retrieved.
#' @param limit <`numeric`> Maximum number of items to retrieve per page.
#' Default is 25.
#' @param zone_bbox <`numeric vector`> Bounding box coordinates (xmin, ymin,
#' xmax, ymax) of the area to retrieve. EPSG4326
#'
#' @return A list of retrieved HLS items.
ndvi_get_items_hls <- function(year, limit = 25, zone_bbox) {
  ndvi_get_items(year = year, limit = limit, zone_bbox = zone_bbox,
                 collections = list("HLSS30.v2.0", "HLSL30.v2.0"))
}

#' Retrieve MODIS Items for a Specific Year and Season
#'
#' This function retrieves MODIS items
#' for a given year and limit within the growing season (from May 1st to August
#' 31st). The function first performs an initial retrieval to determine the total
#' number of pages, and then utilizes parallel processing to fetch the required
#' data.
#'
#' @param year <`numeric`> The year for which the HLS items are to be retrieved.
#' @param limit <`numeric`> Maximum number of items to retrieve per page.
#' Default is 25.
#' @param zone_bbox <`numeric vector`> Bounding box coordinates (xmin, ymin,
#' xmax, ymax) of the area to retrieve. EPSG4326
#'
#' @return A list of retrieved HLS items.
ndvi_get_items_modis <- function(year, limit = 25, zone_bbox) {
  ndvi_get_items(year = year, limit = limit, zone_bbox = zone_bbox,
                 collections = list("MOD11A1"))
}

#' Retrieve Harmonized Landsat Sentinel-2Features
#'
#' This function retrieves Harmonized Landsat Sentinel-2 (HLS) features for
#' the specified years and saves them
#' in the specified output path. The data is collected from the CMR Earth Data
#' STAC API, with search criteria including HLS collection names, bounding box,
#' and growing season time range. The retrieved data is parallelized using
#' future_lapply and saved in the specified output path.
#'
#' @param years <`numeric vector`> A vector of years for which NDVI features are
#' to be retrieved. Defaults to the result of \code{ndvi_years()}.
#' @param output_path <`character`> A character string specifying the path where the
#' retrieved data will be saved. Defaults to "calculated_ignore/ndvi/".
#' @return NULL The function performs the retrieval and saving operations but
#' does not return any value.
ndvi_get_features <- function(years = ndvi_years(),
                              output_path = "calculated_ignore/ndvi/") {

  # Get Canada shapefile
  latest_census <- cc.data::census_years[length(cc.data::census_years)]
  canada <- cancensus::get_census(gsub("^..", "CA", latest_census),
                                  region = list(C = "01"),
                                  level = "C",
                                  geo_format = "sf")["geometry"]

  # Read GeoJSON polygon representing the zone
  zone <- terra::vect(canada)

  # Extract the bounding box of the region of interest
  roi <- terra::ext(zone)
  zone_bbox <- paste(roi[1], roi[3], roi[2], roi[4], sep = ',')

  lapply(years, \(year) {

    # Get all the items
    retrievals <- ndvi_get_items_hls(year = year, zone_bbox = zone_bbox)

    qs::qsave(retrievals, sprintf("%sretrievals_%s.qs", output_path, year))
    return(NULL)
  })

}

#' Convert HLS Features to Point Data
#'
#' This function takes a data frame of HLS features and processes them into point
#' data for further analysis. It includes downloading, cropping, and masking raster
#' data, along with calculating the Normalized Difference Vegetation Index (NDVI) and
#' quality filtering.
#'
#' @param master_polygon <`sf`> Naster polygon as cartographic (DA_carto)
#' @param features <`data.frame`> A data.frame of `features` subsets retrieved
#' from collecetions. The output of \code{\link{ndvi_get_items_hls}}`. It must
#' include information about collections, granule IDs, cloud cover, bands, and
#' asset links.
#' @param temp_folder <`character`> Defaults to `tempdir()`. However, to save disk
#' space, it can be a ink to a managed temporary folder in a dedicated
#' directory. This allows to have complete control over permissions and other attributes
#' when paralleling. Temporary files are not removed otherwise, and the usual temporary
#' folder becomes to heavy.
#' @param year <`numeric`> Which years are the features from?
#' @param crs <`numeric`> CRS of the region.
#'
#' @return An `sf` object containing the NDVI points. The result includes the NDVI
#' values for each valid raster layer, and it is filtered to exclude poor quality
#' pixels.
ndvi_features_to_point <- function(master_polygon, features,
                                   temp_folder = tempdir(), year, crs) {
  # Function to process each item
  process_item <- function(item) {
    # Determine which NDVI bands to use based on collection
    if (features[item,]$collection == 'HLSS30.v2.0') {
      ndvi_bands <- c('B8A','B04','Fmask')
    } else {
      ndvi_bands <- c('B05','B04','Fmask')
    }

    # Function to process each band
    process_band <- function(b) {
      f <- features[item,]
      b_assets <- f$assets[[b]]$href

      # Create a data frame with granule information
      df <- data.frame(Collection = f$collection,
                       Granule_ID = f$id,
                       Cloud_Cover = f$properties$`eo:cloud_cover`,
                       band = b,
                       Asset_Link = b_assets,
                       stringsAsFactors=FALSE)
      return(df)
    }

    # Apply the process_band function to each band
    band_data <- lapply(ndvi_bands, process_band)

    return(band_data)
  }

  # Apply the process_item function to each item
  granule_list <- lapply(row.names(features), process_item)

  # Flatten the list
  granule_list <- do.call(c, granule_list)

  # Concatenate granule information into a single data frame
  search_df <- data.table::rbindlist(granule_list) |> tibble::as_tibble()
  search_df <- unique(search_df)
  if (nrow(search_df) == 0) {
    return(NULL)
  }

  # Create raster stacks for three bands ------------------------------------

  # This section of code creates raster stacks for three bands of interest: Red, NIR, and Fmask.
  # It reads, crops, and masks raster data according to the area of interest (zone).
  # These stacks will be used to calculate the Normalized Difference Vegetation Index (NDVI) and mask
  # for cloud contamination.

  # The Red band processing also extracts the date and classifies it into two types ('S' or 'L'),
  # and this information is stored in the date_list. The data is retrieved from the Cumulus cloud archive,
  # and a progress bar is used to track the status of this time-consuming process.

  # The three functions, process_red, process_fmask, and process_nir, handle the specific logic for each band:
  # - process_red extracts and crops the Red band, and formats the date information.
  # - process_fmask extracts and crops the Fmask band.
  # - process_nir extracts and crops the NIR band.

  # The final results are stored in red_stack, fmask_stack, nir_stack, and date_list, which are lists of raster
  # layers representing the Red, Fmask, and NIR bands, and the corresponding dates, respectively.

  download_raster <- function(grandule_id, band_link, band_nm) {

    destfile <- paste0(temp_folder, grandule_id, band_nm, ".tif")

    httr::RETRY("GET",
                band_link,
                httr::write_disk(destfile, overwrite = TRUE),
                httr::config(netrc = TRUE),
                httr::authenticate(user = .nasa_earthdata_user, password = .nasa_earthdata_pw))

    out <- terra::rast(destfile)

    return(list(out, destfile))
  }

  # Function to process the red band
  process_red <- function(band) {
    downloaded <- download_raster(grandule_id = band$Granule_ID,
                                  band_link = band$Asset_Link,
                                  band_nm = "red")
    red <- downloaded[[1]]

    doy_time <- strsplit(band$Granule_ID, "[.]")[[1]][4]
    doy <- substr(doy_time, 1, 7)
    date <- as.Date(as.integer(substr(doy,5,7)), origin = paste0(substr(doy,1,4), "-01-01"))

    date_code <- if (strsplit(band$Granule_ID, "[.]")[[1]][2] == 'S30') {
      paste0('S', as.character(date))
    } else {
      paste0('L', as.character(date))
    }
    # We wrap so we can instantly destroy the file (disk space), or
    # so we can parallelize if needed
    red <- terra::wrap(red)

    file.remove(downloaded[[2]])
    return(list(red, date_code))
  }

  # Function to process the fmask band
  process_fmask <- function(band) {
    downloaded <- download_raster(grandule_id = band$Granule_ID,
                                  band_link = band$Asset_Link,
                                  band_nm = "fmask")
    fmask <- downloaded[[1]]
    fmask <- terra::wrap(fmask)

    file.remove(downloaded[[2]])
    return(fmask)
  }

  # Function to process the nir band
  process_nir <- function(band) {
    downloaded <- download_raster(grandule_id = band$Granule_ID,
                                  band_link = band$Asset_Link,
                                  band_nm = "nir")
    nir <- downloaded[[1]]
    nir <- terra::wrap(nir)

    file.remove(downloaded[[2]])
    return(nir)
  }

  # Process red band rows
  # progressr::with_progress({
  # pb <- progressr::progressor(nrow(search_df))

  red_bands <- search_df[search_df$band == 'B04',]
  result_red <- lapply(seq_len(nrow(red_bands)), function(i) {
    # pb()
    process_red(band = red_bands[i,])
  })
  red_stack <- lapply(result_red, `[[`, 1)
  date_list <- lapply(result_red, `[[`, 2)

  # Process fmask band rows
  fmask_bands <- search_df[search_df$band == 'Fmask',]
  fmask_stack <- lapply(seq_len(nrow(fmask_bands)), function(i) {
    # pb()
    process_fmask(band = fmask_bands[i,])
  })

  # Process nir band rows
  nir_bands <- search_df[!search_df$band %in% c('B04', 'Fmask'),]
  nir_stack <- lapply(seq_len(nrow(nir_bands)), function(i) {
    # pb()
    process_nir(band = nir_bands[i,])
  })

  # })

  red_stack <- lapply(red_stack, terra::unwrap)
  fmask_stack <- lapply(fmask_stack, terra::unwrap)
  nir_stack <- lapply(nir_stack, terra::unwrap)


  # Calculate NDVI ----------------------------------------------------------

  # Define a function to calculate NDVI
  calculate_NDVI <- function(nir, red){
    ndvi <- (nir-red)/(nir+red) # NDVI formula
    return(ndvi)
  }

  # Calculate NDVI for each stacked Red and NIR raster and exclude Inf and -Inf values
  ndvi_stack <- mapply(\(nir, red, date) {
    nir$ndvi <- calculate_NDVI(as.vector(nir), as.vector(red))
    ndvi <- nir["ndvi"]
    ndvi[ndvi == Inf] <- NA
    ndvi[ndvi == -Inf] <- NA
    names(ndvi) <- date
    return(ndvi)
  }, nir_stack, red_stack, date_list)


  ndvi_stacks <- terra::rast(ndvi_stack) # Create a stack of NDVI

  # Project stacks to the desired CRS before any number manipulation
  ndvi_stacks <- terra::project(ndvi_stacks, sprintf("EPSG:%s", crs))
  fmask_stack <- lapply(fmask_stack, terra::project, sprintf("EPSG:%s", crs))

  # Quality Filtering -------------------------------------------------------

  mask_raster <- list()
  ndvi_filtered <- list()

  # In HLS, both value of 0 and 64 in the Fmask layer indicate the pixel without cloud,
  # cloud shadow, water, or snow/ice. We will use these values to mask out poor quality
  # pixels from the ndvi_stacks. HLS quality information can be found in section 6.5
  # of the [HLS V2.0 User Guide](https://lpdaac.usgs.gov/documents/1118/HLS_User_Guide_V2.pdf).
  for (i in 1:length(fmask_stack)) {
    mask_raster[[i]] <- fmask_stack[[i]]
    # Identify the values to be replaced (not in 0 or 64)
    values_to_replace <- !terra::values(mask_raster[[i]]) %in% c(0, 64)

    # Replace these values with -9999 in the mask_raster
    ndvi_filtered[[i]] <- ndvi_stacks[[i]]
    ndvi_filtered[[i]][values_to_replace] <- -9999

    # # Apply the modified mask to the ndvi_stacks
    # # Replace values in ndvi_stacks with -9999 where mask_raster is -9999
    # ndvi_filtered[[i]] <- terra::cover(ndvi_stacks[[i]], mask_raster[[i]], values-9999)
    # names(ndvi_filtered[[i]]) <- names(ndvi_stacks[[i]])
  }

  ndvi_filtered_stacks <- terra::rast(ndvi_filtered)

  # Save the original extent of ndvi_filtered_stacks
  original_extent <- terra::ext(ndvi_filtered_stacks)

  # Crop to the extent of mp, then mask
  ndvi_filtered_stacks_cropped <- terra::crop(ndvi_filtered_stacks, terra::ext(master_polygon))
  ndvi_filtered_stacks_masked <- terra::mask(ndvi_filtered_stacks_cropped, master_polygon)

  # Extend the masked raster back to its original extent
  # This step sets values outside mp but within the original extent to NA
  ndvi_filtered_stacks_ext <- terra::extend(ndvi_filtered_stacks_masked, original_extent)

  ndvi_filtered_stacks <- ndvi_filtered_stacks_ext

  # 1. Count NA values per cell across layers
  invalid_count_per_cell <- apply(terra::values(ndvi_filtered_stacks), 1, function(x) sum(x == -9999, na.rm = T))

  # 2. Identify cells with at least two NAs
  valid_cells <- invalid_count_per_cell > 2

  # NA cells (Which means it's OUTSIDE the current master_polygon)
  na_count_per_cell <- apply(terra::values(ndvi_filtered_stacks), 1, function(x) sum(is.na(x)))
  only_NA_cell <- na_count_per_cell == ncol(terra::values(ndvi_filtered_stacks))

  # 3. Function to calculate the mean after dropping min and max
  mean_without_min_max <- function(x) {
    x <- stats::na.omit(x)  # Remove NA values
    x <- x[x != -9999]
    x <- x[x > 0 & x < 1]
    x <- x[x != min(x) & x != max(x)]  # Remove min and max
    if (length(x) > 0) mean(x) else -9999  # Calculate mean or return NA if no data left
  }

  # 4. Apply the function to each cell, excluding those with at least two NAs
  mean_values <- apply(terra::values(ndvi_filtered_stacks)[valid_cells, , drop = FALSE], 1, mean_without_min_max)

  # 5. Create a new raster for the mean values
  # Initialize a raster filled with NAs
  mean_raster <- terra::setValues(ndvi_filtered_stacks[[1]], rep(NA, terra::ncell(ndvi_filtered_stacks)))

  # Fill in the mean values in the appropriate cells
  mean_raster[valid_cells] <- mean_values
  mean_raster[!valid_cells] <- -9999
  mean_raster[only_NA_cell] <- NA

  names(mean_raster) <- sprintf("ndvi_%s", year)

  return(mean_raster)

}

#' Import NDVI Data for Specific Months and Process It
#'
#' This function imports NDVI data for specific months (May to August) and performs
#' various processing tasks. The function is intended to work with NDVI data for
#' analyzing vegetation patterns. It imports, filters, and processes the data
#' to be used in further analyses.
#'
#' @param years <`numeric vector`> A vector of years for which NDVI features are
#' to be retrieved. Defaults to the result of \code{ndvi_years()}.
#' @param output_path <`character`> A character string specifying the path
#' where the calculated NDVI data should be saved. Defaults to "calculated_ignore/ndvi/".
#' @param temp_folder <`character`> Link to a managed temporary folder in a dedicated
#' directory. This allows to have complete control over permissions and other attributes
#' when paralleling. Temporary files are not removed otherwise, and the usual temporary
#' folder becomes to heavy.
#'
#' @details
#' This function relies on several sub-functions to perform various tasks. The overall
#' functionality includes retrieving specific date ranges, importing NDVI data,
#' processing individual bands, calculating NDVI, and performing quality filtering.
#'
#' @return NDVI raters (.tiff) are saved in the output path.
#' @export
ndvi_import <- function(years = ndvi_years(),
                        output_path = "calculated_ignore/ndvi/",
                        temp_folder = "calculated_ignore/ndvi/tmp/") {

  if (!exists(".nasa_earthdata_user") || !exists(".nasa_earthdata_pw")) {
    stop(paste0("User and password objects (an account on Nasa Earthdata ",
                "portal) must exist before running the download. Please make ",
                "sure they are set."))
  }

  # Set GDAL configuration options
  terra::setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
  terra::setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
  terra::setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")

  # Get already processed files
  ndvi_files <- unname(sapply(
    aws.s3::get_bucket(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      bucket = "curbcut.rawdata"
    ), `[[`, "Key"
  ))
  ndvi_files <- ndvi_files[grepl("^ndvi/", ndvi_files)]

  lapply(rev(years), \(year) {

    retrievals <- qs::qread(sprintf("%sretrievals_%s.qs", output_path, year))

    # Get the features only
    retrievals_features <- lapply(retrievals, `[[`, "features")

    # Identify common columns across all data frames
    common_columns <- Reduce(intersect, lapply(retrievals_features, colnames))

    # Select only common columns and bind the data frames
    searches_features <- dplyr::bind_rows(lapply(retrievals_features, \(df) dplyr::select(df, common_columns)))
    searches_features <- tibble::as_tibble(searches_features)

    # Avoid unnecessary noise and reduce the computational load by filtering out
    # highly cloudy granules.
    searches_features <- searches_features[searches_features$properties$`eo:cloud_cover` < 30, ]

    # Extract the tile info
    # Extract the tile info
    pattern <- "T[0-9]{2}[A-Z]{3}"
    unique_tile <- gsub(paste0(".*(", pattern, ").*"), "\\1", unique(searches_features$id))

    progressr::with_progress({
      pb <- progressr::progressor(length(unique_tile))

      future.apply::future_lapply(unique_tile, \(tile) {

        # Only get the features that file with the tile
        tile_features <- searches_features[grepl(tile, searches_features$id), ]

        # Assuming `coords` is the object from retrievals
        tile_polygon <- lapply(seq_along(tile_features$id), \(r) {
          coords <- tile_features[r,]$geometry$coordinates[[1]]
          # Create a matrix with x and y coordinates
          polygon_matrix <- rbind(
            cbind(coords[, , 1], coords[, , 2]),
            c(coords[1, 1, 1], coords[1, 1, 2]) # Close the polygon
          )

          # Create an sf polygon object
          sf_polygon <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(list(polygon_matrix))),
            crs = 4326 # Specify the appropriate CRS
          )

          sf_polygon
        })

        tile_polygon <- Reduce(rbind, tile_polygon)
        tile_polygon <- sf::st_make_valid(tile_polygon)
        tile_polygon <- sf::st_union(tile_polygon)

        # Get the bbox and see if it's already processed
        bbox <- as.vector(sf::st_bbox(tile_polygon))
        bbox <- round(bbox, digits = 2)
        bbox <- sprintf("%s;%s;%s;%s", bbox[1],bbox[2],bbox[3],bbox[4])

        # If already processed, do not go over again
        year_bbox <- sprintf("%s/%s.qs", year, bbox)
        if (sum(grepl(year_bbox, ndvi_files)) > 0) return({
          pb()
          NULL
        })

        ndvi_points <- ndvi_features_to_point(features = tile_features,
                                              temp_folder = temp_folder)

        # Save to disk
        file_path <- sprintf("%s%s.qs", temp_folder, bbox)
        qs::qsave(ndvi_points, file = file_path)

        aws.s3::put_object(bucket = "curbcut.rawdata",
                           object = sprintf("ndvi/%s/%s.qs", year, bbox),
                           file = normalizePath(file_path),
                           region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                           key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                           secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"))

        file.remove(file_path)

        gc()
        pb()
        return(NULL)
      })
    })
  })
}

#' Import NDVI from Master Polygon
#'
#' This function reads the master polygon of a region and extracts NDVI data
#' for specified years. The process involves extracting the bounding box of the
#' master polygon, retrieving features related to the specified region, and
#' filtering the NDVI points based on tile information. It saves the yearly NDVI
#' values in the `output_path/year` with centroid of every 30m cells as the ID.
#' The satellite is very precise, and every raster is exactly the same centroid
#' value, which can be used as an ID. We keep geometry for the latest year,
#' and remove it from other years.
#'
#' @param master_polygon <`sf polygon`> The master polygon of the region. Should
#' be cartographic master polygon (DA_carto).
#' @param years <`character vector`> A vector specifying the years for which
#' NDVI data is to be retrieved. Defaults to the output of \code{ndvi_years()}.
#' @param output_path <`character`> Path where data should be downloaded
#' @param temp_folder <`character`> A temporary folder path to store intermediate data.
#' Defaults to the system's temporary directory. However, to save disk
#' space, it can be a ink to a managed temporary folder in a dedicated
#' directory. This allows to have complete control over permissions and other attributes
#' when paralleling. Temporary files are not removed otherwise, and the usual temporary
#' folder becomes to heavy.
#' @param overwrite <`logical`> Should data be overwritten?
#' @param crs <`numeric`> CRS of the region.
#'
#' @return NULL. The function processes the NDVI points and applies relevant
#' filtering and transformation but does not return a value.
#' @export
ndvi_import_from_masterpolygon <- function(master_polygon, years = ndvi_years(),
                                           output_path, temp_folder = tempdir(),
                                           overwrite = FALSE, crs) {

  # Read GeoJSON polygon representing the zone
  master_polygon <- sf::st_transform(master_polygon, 4326)
  zone <- terra::vect(master_polygon)

  # Extract the bounding box of the region of interest
  roi <- terra::ext(zone)
  zone_bbox <- paste(roi[1], roi[3], roi[2], roi[4], sep = ',')

  # Get the tiles that must be extracted (they are the same at every year)
  last_year <- max(years)

  # Retrievel HLS items
  retrievals <- ndvi_get_items_hls(year = last_year, zone_bbox = zone_bbox)

  # Get the features only
  retrievals_features <- lapply(retrievals, `[[`, "features")

  # Identify common columns across all data frames
  common_columns <- Reduce(intersect, lapply(retrievals_features, colnames))

  # Select only common columns and bind the data frames
  searches_features <- dplyr::bind_rows(lapply(retrievals_features, \(df) dplyr::select(df, common_columns)))
  searches_features <- tibble::as_tibble(searches_features)

  # Extract the tile info
  # Extract the tile info
  pattern <- "T[0-9]{2}[A-Z]{3}"
  unique_tile <- gsub(paste0(".*(", pattern, ").*"), "\\1", unique(searches_features$id))
  unique_tile <- unique(unique_tile)

  mp <- sf::st_transform(master_polygon, crs = crs)
  mp <- terra::vect(mp)

  # All existing files in the output path
  all_files <- list.files(output_path, full.names = TRUE)

  progressr::with_progress({
    pb <- progressr::progressor(length(unique_tile) * length(years) + length(unique_tile))

    lapply(unique_tile, \(tile) {

      # For every tile, iterate over all the years
      tile_data <- lapply(rev(years), \(year) {
        file_name <- sprintf("%s%s_%s.tif", output_path, tile, year)
        if (!overwrite & file_name %in% all_files) {
          pb()
          return(terra::rast(file_name))
        }

        retrievals <- ndvi_get_items_hls(year = year, zone_bbox = zone_bbox)

        # Get the features only
        retrievals_features <- lapply(retrievals, `[[`, "features")

        # Identify common columns across all data frames
        common_columns <- Reduce(intersect, lapply(retrievals_features, colnames))

        # Select only common columns and bind the data frames
        searches_features <- dplyr::bind_rows(lapply(retrievals_features, \(df) dplyr::select(df, common_columns)))
        searches_features <- tibble::as_tibble(searches_features)

        # Which features are part of this tile
        tile_features <- searches_features[grepl(tile, searches_features$id), ]

        out <- ndvi_features_to_point(
          master_polygon = mp,
          features = tile_features,
          temp_folder = temp_folder,
          year = year,
          crs = crs)

        # Save the file (save advancements)
        terra::writeRaster(out, file = file_name, overwrite = TRUE)

        pb()

        out
      })

      # If the files all already exist, return nothing
      files <- sprintf("%sgrd30_%s.tif", output_path, tile)
      if (overwrite | !all(files %in% all_files)) {

        # Reduce to only one set of raster
        grd30 <- Reduce(c, tile_data)

        terra::writeRaster(grd30, file = sprintf("%sgrd30_%s.tif", output_path, tile),
                           overwrite = TRUE)

      }

      pb()

      return(NULL)

    })

  })

  # Grab all the tiles and make one raster
  all_files_after_tiles <- list.files(output_path, full.names = TRUE)
  grd_files <- grep("grd30", all_files_after_tiles, value = TRUE)
  grd_files <- grep("tif$", grd_files, value = TRUE)

  final_tifs <- sprintf("%s%s.tif", output_path, c("grd30", "grd60", "grd120", "grd300"))
  if (overwrite | !all(final_tifs %in% list.files(output_path))) {

    # Combine all data
    rasters <- lapply(grd_files, terra::rast)
    raster <- Reduce(terra::merge, rasters)
    trimmed <- terra::trim(raster)
    for (i in names(trimmed)) {
      layer_values <- terra::values(trimmed[[i]])
      layer_values[layer_values < 0 & !is.na(layer_values)] <- -9999
      trimmed[[i]] <- as.numeric(layer_values)
    }
    grd30 <- trimmed

    # From the trimmed raster, create the other raster sizes
    mean_fun_ignore <- \(...) {
      values <- c(...)
      if (all(is.na(values))) return(NA)
      values <- na.omit(values)
      # Ignore invalid values if more than half of the values is
      if (sum(values == -9999) == (length(values))) return(-9999)
      values <- values[values != -9999]
      mean(values)
    }

    grd60 <- terra::aggregate(grd30, fact = 2, fun = mean_fun_ignore)
    grd120 <- terra::aggregate(grd30, fact = 4, fun = mean_fun_ignore)
    grd300 <- terra::aggregate(grd30, fact = 10, fun = mean_fun_ignore)

    # Calculate delta directly at the raster scale
    year_combinations <- t(utils::combn(years, 2))
    year_combinations <- split(year_combinations, seq(nrow(year_combinations)))

    for (i in year_combinations) {
      first_year <- sprintf("ndvi_%s", i[[1]])
      second_year <- sprintf("ndvi_%s", i[[2]])

      first_year_values <- terra::values(grd30[[first_year]])
      second_year_values <- terra::values(grd30[[second_year]])

      # Replace -9999 with NA
      first_year_values[first_year_values == -9999] <- NA
      second_year_values[second_year_values == -9999] <- NA

      # Perform the calculation
      vec <- second_year_values - first_year_values
      vec <- vec / first_year_values
      vec <- as.numeric(vec)

      # Assign the calculated values back to grd30
      grd30[[sprintf("ndvi_delta_%s_%s", i[[1]], i[[2]])]] <- vec

      # Clean up
      rm(vec)
      gc()
    }

    for (i in year_combinations) {
      first_year <- sprintf("ndvi_%s", i[[1]])
      second_year <- sprintf("ndvi_%s", i[[2]])

      first_year_values <- terra::values(grd60[[first_year]])
      second_year_values <- terra::values(grd60[[second_year]])

      # Replace -9999 with NA
      first_year_values[first_year_values == -9999] <- NA
      second_year_values[second_year_values == -9999] <- NA

      # Perform the calculation
      vec <- second_year_values - first_year_values
      vec <- vec / first_year_values
      vec <- as.numeric(vec)

      # Assign the calculated values back to grd60
      grd60[[sprintf("ndvi_delta_%s_%s", i[[1]], i[[2]])]] <- vec

      # Clean up
      rm(vec)
      gc()
    }

    for (i in year_combinations) {
      first_year <- sprintf("ndvi_%s", i[[1]])
      second_year <- sprintf("ndvi_%s", i[[2]])

      first_year_values <- terra::values(grd120[[first_year]])
      second_year_values <- terra::values(grd120[[second_year]])

      # Replace -9999 with NA
      first_year_values[first_year_values == -9999] <- NA
      second_year_values[second_year_values == -9999] <- NA

      # Perform the calculation
      vec <- second_year_values - first_year_values
      vec <- vec / first_year_values
      vec <- as.numeric(vec)

      # Assign the calculated values back to grd120
      grd120[[sprintf("ndvi_delta_%s_%s", i[[1]], i[[2]])]] <- vec

      # Clean up
      rm(vec)
      gc()
    }

    for (i in year_combinations) {
      first_year <- sprintf("ndvi_%s", i[[1]])
      second_year <- sprintf("ndvi_%s", i[[2]])

      first_year_values <- terra::values(grd300[[first_year]])
      second_year_values <- terra::values(grd300[[second_year]])

      # Replace -9999 with NA
      first_year_values[first_year_values == -9999] <- NA
      second_year_values[second_year_values == -9999] <- NA

      # Perform the calculation
      vec <- second_year_values - first_year_values
      vec <- vec / first_year_values
      vec <- as.numeric(vec)

      # Assign the calculated values back to grd300
      grd300[[sprintf("ndvi_delta_%s_%s", i[[1]], i[[2]])]] <- vec

      # Clean up
      rm(vec)
      gc()
    }

    # Save the raster
    terra::writeRaster(grd30, file = sprintf("%sgrd30.tif", output_path),
                       overwrite = TRUE)
    terra::writeRaster(grd60, file = sprintf("%sgrd60.tif", output_path),
                       overwrite = TRUE)
    terra::writeRaster(grd120, file = sprintf("%sgrd120.tif", output_path),
                       overwrite = TRUE)
    terra::writeRaster(grd300, file = sprintf("%sgrd300.tif", output_path),
                       overwrite = TRUE)

  }


  # Switch the rasters to polygons, intersect with the masterpolygon (DA_carto)
  lapply(c("grd30", "grd60", "grd120", "grd300"), \(x) {
    grd <- terra::rast(sprintf("%s%s.tif", output_path, x))
    output_file <- sprintf("%s%s.qs", output_path, x)
    if (!overwrite & output_file %in% all_files) {
      return(NULL)
    }

    # Initialize an empty list to store reprojected layers
    grd_reprojected_list <- list()

    # Loop through each layer
    for (i in 1:terra::nlyr(grd)) {
      # Select the current layer
      layer <- grd[[i]]

      # Create a mask for invalid values (-9999) in the current layer
      invalid_mask <- layer == -9999

      # Replace -9999 with NA in the current layer
      terra::values(layer)[terra::values(invalid_mask)] <- NA

      # Reproject the current layer to EPSG:2950
      layer_reprojected <- terra::project(layer, "EPSG:2950")

      # Reproject the mask raster to the same CRS, using nearest neighbor
      mask_reprojected <- terra::project(invalid_mask, "EPSG:2950", method = "near")

      # Apply the reprojected mask to reintroduce -9999 values in the reprojected layer
      terra::values(layer_reprojected)[
        terra::values(mask_reprojected) & !is.na(terra::values(mask_reprojected))] <- -9999

      # Add the reprojected layer to the list
      grd_reprojected_list[[i]] <- layer_reprojected
    }

    # Combine the reprojected layers back into a single raster
    grd <- terra::rast(grd_reprojected_list)

    grid_sf <- sf::st_make_grid(terra::ext(grd),
                                cellsize = terra::res(grd),
                                crs = 2950)

    # Create a tible of SF
    tb <- tibble::tibble(ID = sprintf("%s_%s", x, seq_along(grid_sf)))
    tb$geometry <- grid_sf
    tb <- sf::st_as_sf(tb)

    # Get the number of 'rows' and 'columns' of the grid spatial feature
    cell_size <- terra::res(grd)[[1]]
    num_cells_x <- (sf::st_bbox(tb)$xmax - sf::st_bbox(tb)$xmin) / cell_size
    num_cells_y <- (sf::st_bbox(tb)$ymax - sf::st_bbox(tb)$ymin) / cell_size
    num_cells_x <- round(as.numeric(num_cells_x))
    num_cells_y <- round(as.numeric(num_cells_y))

    ind <- nrow(tb):1
    group_factor <- rev((ind - 1) %/% num_cells_x)
    grps <- split(ind, group_factor)
    grps <- lapply(grps, rev)
    new_ind <- Reduce(c, grps)

    tb <- tb[new_ind, ]

    for (n in names(grd)) {
      tb[[n]] <- terra::values(grd[[n]]) |> as.numeric()
    }

    # Remove NAs (which in this case is outside of the cartographic
    # master_polygonwater). We only have to look at the most recent year,
    # as it's a sure thing NA is outside the master_polygon at all years
    latest_ndvi <- sprintf("ndvi_%s", max(years))
    na_rows <- is.nan(tb[[latest_ndvi]]) | is.na(tb[[latest_ndvi]])
    tb <- tb[!na_rows, ]

    # Remove invalid values (for all columns)
    ndvi_cols <- names(tb)[!names(tb) %in% c("ID", "geometry")]
    for (n in ndvi_cols) {
      is_invalid <- tb[[n]] == -9999
      tb[[n]][is_invalid] <- NA
    }

    # Save
    qs::qsave(tb, sprintf("%s%s.qs", output_path, x))
  })

  return(NULL)

}

