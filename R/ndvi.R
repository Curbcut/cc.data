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
    progressr::with_progress({
      pb <- progressr::progressor(total_pages)
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

        pb()
        return(search_req)
      })
      retrievals <- c(retrievals, more_retrievals)
    })
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
#' @param features <`data.frame`> A data.frame of `features` subsets retrieved
#' from collecetions. The output of \code{\link{ndvi_get_items_hls}}`. It must
#' include information about collections, granule IDs, cloud cover, bands, and
#' asset links.
#' @param temp_folder <`character`> Defaults to `tempdir()`. However, to save disk
#' space, it can be a ink to a managed temporary folder in a dedicated
#' directory. This allows to have complete control over permissions and other attributes
#' when paralleling. Temporary files are not removed otherwise, and the usual temporary
#' folder becomes to heavy.
#' @param remove_NAs <`logical`> Should NA raster get removed?
#'
#' @return An `sf` object containing the NDVI points. The result includes the NDVI
#' values for each valid raster layer, and it is filtered to exclude poor quality
#' pixels.
ndvi_features_to_point <- function(features, temp_folder = tempdir(), remove_NAs = TRUE) {
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


  # Quality Filtering -------------------------------------------------------

  mask_raster <- list()
  ndvi_filtered <- list()

  # In HLS, both value of 0 and 64 in the Fmask layer indicate the pixel without cloud,
  # cloud shadow, water, or snow/ice. We will use these values to mask out poor quality
  # pixels from the ndvi_stacks. HLS quality information can be found in section 6.5
  # of the [HLS V2.0 User Guide](https://lpdaac.usgs.gov/documents/1118/HLS_User_Guide_V2.pdf).
  for (i in 1:length(fmask_stack)) {
    mask_raster[[i]] <- fmask_stack[[i]]
    mask_raster[[i]][!terra::values(mask_raster[[i]]) %in% c(0, 64)] <- NA
    ndvi_filtered[[i]] <- terra::mask(ndvi_stacks[[i]], mask_raster[[i]], maskvalue=NA )
    names(ndvi_filtered[[i]]) <- names(ndvi_stacks[[i]])
  }

  ndvi_filtered_stacks <- terra::rast(ndvi_filtered)

  # Only keep layers with non-NA values
  if (remove_NAs) {
    lyrn <- 1:terra::nlyr(ndvi_filtered_stacks)
    valid_layers <- sapply(lyrn, function(i) {
      !all(is.na(terra::values(ndvi_filtered_stacks[[i]])))
    })
    ndvi_filtered_stacks <- ndvi_filtered_stacks[[which(valid_layers)]]
  }

  # Switch to sf points
  ndvi_points <- terra::as.points(ndvi_filtered_stacks, na.rm = remove_NAs, na.all = remove_NAs)
  ndvi_points <- sf::st_as_sf(ndvi_points)

  na_count_per_row <- rowSums(is.na(ndvi_points))
  at_least_two <- na_count_per_row > 2
  at_least_two <- ndvi_points[at_least_two, ]

  # Function to calculate the mean after dropping min and max
  mean_without_min_max <- function(row) {
    # Remove NA values if needed
    row <- na.omit(row)
    # Remove the minimum and maximum values
    row <- row[row != min(row) & row != max(row)]
    # Calculate the mean of the remaining values
    mean(row)
  }

  # Apply the function to each row (after dropping geometry if it's an sf object)
  ndvi_points$ndvi <- apply(sf::st_drop_geometry(ndvi_points), 1, mean_without_min_max)
  ndvi_points <- ndvi_points["ndvi"]

  return(ndvi_points)
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
#' @param master_polygon <`sf polygon`> The master polygon of the region.
#' @param years <`character vector`> A vector specifying the years for which
#' NDVI data is to be retrieved. Defaults to the output of \code{ndvi_years()}.
#' @param temp_folder <`character`> A temporary folder path to store intermediate data.
#' Defaults to the system's temporary directory. However, to save disk
#' space, it can be a ink to a managed temporary folder in a dedicated
#' directory. This allows to have complete control over permissions and other attributes
#' when paralleling. Temporary files are not removed otherwise, and the usual temporary
#' folder becomes to heavy.
#' @return NULL. The function processes the NDVI points and applies relevant
#' filtering and transformation but does not return a value.
#' @export
ndvi_import_from_masterpolygon <- function(master_polygon, years = ndvi_years(),
                                           output_path, temp_folder = tempdir(),
                                           overwrite = FALSE, filter_cloudy_tiles = TRUE) {

  # Read GeoJSON polygon representing the zone
  zone <- terra::vect(master_polygon)

  # Extract the bounding box of the region of interest
  roi <- terra::ext(zone)
  zone_bbox <- paste(roi[1], roi[3], roi[2], roi[4], sep = ',')

  lapply(rev(years), \(year) {

    latest_year <- year == max(years)

    dir.create(sprintf("%s%s", output_path, year)) |> suppressWarnings()

    retrievals <- ndvi_get_items_hls(year = year, zone_bbox = zone_bbox)

    # Get the features only
    retrievals_features <- lapply(retrievals, `[[`, "features")

    # Identify common columns across all data frames
    common_columns <- Reduce(intersect, lapply(retrievals_features, colnames))

    # Select only common columns and bind the data frames
    searches_features <- dplyr::bind_rows(lapply(retrievals_features, \(df) dplyr::select(df, common_columns)))
    searches_features <- tibble::as_tibble(searches_features)

    # Avoid unnecessary noise and reduce the computational load by filtering out
    # highly cloudy granules.
    if (filter_cloudy_tiles)
      searches_features <- searches_features[searches_features$properties$`eo:cloud_cover` < 30, ]

    # Extract the tile info
    # Extract the tile info
    pattern <- "T[0-9]{2}[A-Z]{3}"
    unique_tile <- gsub(paste0(".*(", pattern, ").*"), "\\1", unique(searches_features$id))
    unique_tile <- unique(unique_tile)

    if (!overwrite) {
      all_files <- list.files(sprintf("%s%s", output_path, year))
      all_files <- gsub(".qs", "", all_files)
      if (length(all_files) != 0) {
        unique_tile <- unique_tile[!unique_tile %in% all_files]
      }
    }

    progressr::with_progress({

      pb <- progressr::progressor(length(unique_tile))

      lapply(unique_tile, \(tile) {
        tile_features <- searches_features[grepl(tile, searches_features$id), ]
        ndvi_points <- ndvi_features_to_point(
          features = tile_features,
          temp_folder = temp_folder,
          remove_NAs = if (latest_year) FALSE else TRUE)

        # Remove geometry features except for the most recent year
        coords <- sf::st_coordinates(ndvi_points)
        if (!latest_year) ndvi_points <- sf::st_drop_geometry(ndvi_points)

        # For the most recent year, switch from points to gemetries
        if (latest_year) {
          # Function to create a grid cell from a centroid
          create_grid_cell <- function(centroid, size) {
            # Calculate half of the grid size
            half_size <- size / 2

            # Extract longitude and latitude
            lon <- centroid[1]
            lat <- centroid[2]

            # Calculate corner coordinates
            xmin <- lon - half_size
            xmax <- lon + half_size
            ymin <- lat - half_size
            ymax <- lat + half_size

            # Create a POLYGON from these coordinates
            sf::st_polygon(list(matrix(c(xmin, ymin,
                                         xmin, ymax,
                                         xmax, ymax,
                                         xmax, ymin,
                                         xmin, ymin),
                                       ncol = 2, byrow = TRUE)))
          }

          # Create the grid cells
          ndvi_points_crs <- sf::st_crs(ndvi_points)
          geoms <- lapply(ndvi_points$geometry, create_grid_cell, size = 30)
          geometry <- sf::st_sfc(geoms, crs = ndvi_points_crs)
          grid_sf <- sf::st_sf(geometry)
          ndvi_points <- grid_sf
        }

        # Keep the ID (centroid)
        ndvi_points$ID <- apply(coords, 1, paste0, collapse = "_")
        ndvi_points[[sprintf("ndvi_%s", year)]] <- ndvi_points$ndvi
        ndvi_points$ndvi <- NULL

        pb()
        qs::qsave(ndvi_points, file = sprintf("%s%s/%s.qs", output_path, year, tile))
      })

    })

    return(NULL)
  })

}
