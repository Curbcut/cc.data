#' Generate a Sequence of Years from 2013 to Current Year
#'
#' This function creates a sequence of years from 2013 to the current year.
#' It is used for NDVI import, as the first satellite images were from 2013.
#'
#' @return A numeric vector containing the sequence of years from 2013 to the current year.
#' @export
ndvi_years <- function() {
  # Get the current year and month
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- as.integer(format(Sys.Date(), "%m"))

  # Check if the current month is past August
  if (current_month > 8) {
    year_to_use <- current_year
  } else {
    year_to_use <- current_year - 1
  }

  2014:year_to_use
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
    # If it already exists and is 'big enough' (somtimes a few B get downloaded)
    if (file.exists(destfile) && (file.info(destfile)$size >= 500)) {
      return({
        out <- terra::rast(destfile)
        list(out, destfile)
      })
    }

    # If not, retry grabbing
    httr::RETRY("GET",
                band_link,
                httr::write_disk(destfile, overwrite = TRUE),
                httr::config(netrc = TRUE),
                httr::add_headers(Authorization = paste("Bearer", .nasa_earthdata_token)),
                # httr::authenticate(user = .nasa_earthdata_user, password = .nasa_earthdata_pw),
                times = 10)

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

    return(list(red, date_code, destfile = downloaded[[2]]))
  }

  # Function to process the fmask band
  process_fmask <- function(band) {
    downloaded <- download_raster(grandule_id = band$Granule_ID,
                                  band_link = band$Asset_Link,
                                  band_nm = "fmask")
    fmask <- downloaded[[1]]

    return(list(fmask, destfile = downloaded[[2]]))
  }

  # Function to process the nir band
  process_nir <- function(band) {
    downloaded <- download_raster(grandule_id = band$Granule_ID,
                                  band_link = band$Asset_Link,
                                  band_nm = "nir")
    nir <- downloaded[[1]]

    return(list(nir, destfile = downloaded[[2]]))
  }

  # Define a function to calculate NDVI
  calculate_NDVI <- function(red, nir){
    ndvi <- (nir-red) / (nir+red) # NDVI formula
    ndvi[ndvi < 0 | ndvi > 1] <- NA_integer_
    ndvi[is.infinite(ndvi)] <- NA_integer_
    return(c(ndvi))
  }

  red_bands <- search_df[search_df$band == 'B04',]
  fmask_bands <- search_df[search_df$band == 'Fmask',]
  nir_bands <- search_df[!search_df$band %in% c('B04', 'Fmask'),]

  # Function to apply terra::project with retry logic
  project_with_retry <- function(stack, crs) {
    success <- FALSE
    result <- NULL

    while (!success) {
      tryCatch({
        # Attempt to project the stack
        result <- terra::project(stack, sprintf("EPSG:%s", crs))

        # If no error occurs, set success to TRUE
        success <- TRUE
      }, error = function(e) {
        # Error handling: print the error message
        message("An error occurred: ", e$message, ". Retrying.")

        # Optional: add a pause before retrying
        # Sys.sleep(time_in_seconds)

        # Keep success as FALSE so the loop continues
      })
    }

    return(result)
  }

  # Get the table for quality assessment
  qa_table <- decode_qa_values()

  progressr::with_progress({
    pb <- progressr::progressor(nrow(red_bands))
    lapply(seq_len(nrow(red_bands)), \(i) {

      # Download the necessary tif files
      result_red <- process_red(band = red_bands[i,])
      red <- result_red[[1]]
      date <- result_red[[2]]

      # If the file already exist, move to the next
      file_name <- sprintf("%sndvipointer_%s.tif", temp_folder, date)
      if (file.exists(file_name)) {
        pb()
        # Remove raw files downloaded
        file.remove(result_red$destfile)
        return(NULL)
      }

      # Download all the necessary tif files
      result_fmask <- process_fmask(band = fmask_bands[i,])
      fmask <- result_fmask[[1]]
      result_nir <- process_nir(band = nir_bands[i,])
      nir <- result_nir[[1]]


      # Calculate the NDVI matrix for that tile
      stack <- calculate_NDVI(red, nir)
      stack <- project_with_retry(stack, crs)

      # Fmask values (quality filtering)
      # Refer to Quality Assessment layer https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf
      fmask <- project_with_retry(fmask, crs)
      fmask_values <- terra::values(fmask)[,1]


      cloudy <- qa_table$qa_value[qa_table$cloud == 1 | qa_table$water == 1 |
                                    qa_table$snow_ice == 1]
      stack[[1]][fmask_values %in% cloudy] <- -9999

      # Add date as the name
      names(stack) <- date

      # Save, remove from environment, garbage collection. SAVE MEMORY
      terra::writeRaster(stack, file_name, overwrite = TRUE)

      # Remove raw files downloaded
      file.remove(result_red$destfile)
      file.remove(result_fmask$destfile)
      file.remove(result_nir$destfile)

      rm(stack)
      gc()
      pb()
    })
  })


  # Build the whole ndvi stack
  files <- list.files(temp_folder, full.names = TRUE)
  pointers <- grep("ndvipointer_", files, value = TRUE)
  ndvi_stacks <- terra::rast(lapply(pointers, terra::rast))


  # Quality Filtering -------------------------------------------------------

  # Save the original extent of ndvi_filtered_stacks
  original_extent <- terra::ext(ndvi_stacks)

  # Crop to the extent of mp, then mask
  ndvi_filtered_stacks_cropped <- terra::crop(ndvi_stacks, terra::ext(master_polygon))
  ndvi_filtered_stacks_masked <- terra::mask(ndvi_filtered_stacks_cropped, master_polygon)

  # Extend the masked raster back to its original extent
  # This step sets values outside mp but within the original extent to NA
  ndvi_filtered_stacks_ext <- terra::extend(ndvi_filtered_stacks_masked, original_extent)

  ndvi_filtered_stacks <- ndvi_filtered_stacks_ext

  # Function to calculate the mean NDVI value after dropping min and max
  mean_without_min_max <- function(x) {
    x <- stats::na.omit(x)  # Remove NA values
    x <- x[x != -9999]
    x <- x[x > 0 & x < 1]
    x <- x[x != min(x) & x != max(x)]  # Remove min and max
    if (length(x) > 0) mean(x) else -9999  # Calculate mean or return NA if no data left
  }

  # Function to extract year from column name
  extract_year <- function(name) {
    as.integer(substr(name, 2, 5))
  }

  # Get the years from the column names
  years <- unique(sapply(names(ndvi_filtered_stacks), extract_year))

  # Initialize an empty stack with one layer per year
  yearly_stack <- terra::rast(ndvi_filtered_stacks[[1]], nlyr = length(years))
  names(yearly_stack) <- paste0("ndvi_", years)

  # Loop over each year
  progressr::with_progress({
    pb <- progressr::progressor(length(years))
    for (year in years) {
      # Select layers for the current year
      current_year_layers <- grep(paste0(".", year), names(ndvi_filtered_stacks), value = TRUE)
      current_year_stack <- terra::subset(ndvi_filtered_stacks, current_year_layers)

      # Steps 1 and 2: Handling NA and invalid values
      invalid_count_per_cell <- apply(terra::values(current_year_stack),  1,
                                      \(x) sum(x == -9999, na.rm = TRUE))
      valid_cells <- invalid_count_per_cell > 2

      na_count_per_cell <- apply(terra::values(current_year_stack), 1, \(x) sum(is.na(x)))
      only_NA_cell <- na_count_per_cell == ncol(terra::values(current_year_stack))

      # Step 3: Apply the mean_without_min_max function to each cell for the current year
      mean_values <- apply(terra::values(current_year_stack)[valid_cells, , drop = FALSE],
                           1, mean_without_min_max)

      # Step 4: Create a layer for the mean values of the current year
      mean_layer <- rep(NA, terra::ncell(ndvi_filtered_stacks))
      mean_layer[valid_cells] <- mean_values
      mean_layer[!valid_cells] <- -9999
      mean_layer[only_NA_cell] <- NA

      # Step 5: Add the layer to the yearly stack
      yearly_stack[[paste0("ndvi_", year)]] <- mean_layer
      pb()

      rm(mean_layer, mean_values, only_NA_cell, na_count_per_cell, valid_cells,
         invalid_count_per_cell, current_year_stack, current_year_layers)
      gc()
    }
  })

  return(yearly_stack)

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
#' @param grid_sizes <`character vector`> What should the size of the grid cells?
#'
#' @return NULL. The function processes the NDVI points and applies relevant
#' filtering and transformation but does not return a value.
#' @export
ndvi_import_from_masterpolygon <- function(master_polygon, years = ndvi_years(),
                                           output_path, temp_folder = tempdir(),
                                           overwrite = FALSE, crs, grid_sizes) {

  # If the folder doesn't exist, create it
  dir.create(temp_folder, showWarnings = FALSE, recursive = TRUE)

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

  lapply(unique_tile, \(tile) {

    # For every tile, iterate over all the years
    file_name <- sprintf("%s%s.tif", output_path, tile)
    if (!overwrite & file_name %in% all_files) {
      return(terra::rast(file_name))
    }

    tile_features <- lapply(rev(years), \(year) {
      retrievals <- cc.data:::ndvi_get_items_hls(year = year, zone_bbox = zone_bbox)

      # Get the features only
      retrievals_features <- lapply(retrievals, `[[`, "features")

      # Identify common columns across all data frames
      common_columns <- Reduce(intersect, lapply(retrievals_features, colnames))

      # Select only common columns and bind the data frames
      searches_features <- dplyr::bind_rows(lapply(retrievals_features, \(df) dplyr::select(df, common_columns)))
      searches_features <- tibble::as_tibble(searches_features)

      # Which features are part of this tile
      searches_features[grepl(tile, searches_features$id), ]
    })

    # Identify common columns across all data frames
    common_columns <- Reduce(intersect, lapply(tile_features, colnames))

    # Select only common columns and bind the data frames
    tile_features <- dplyr::bind_rows(lapply(tile_features, \(df) dplyr::select(df, common_columns)))
    tile_features <- tibble::as_tibble(tile_features)

    out <- ndvi_features_to_point(
      master_polygon = mp,
      features = tile_features,
      temp_folder = temp_folder,
      year = year,
      crs = crs)

    # Empty the temp folder to save some disk space
    lapply(list.files(temp_folder, full.names = TRUE), file.remove)

    # Save the file
    terra::writeRaster(out, file = file_name, overwrite = TRUE)

    return(NULL)

  })

  # Grab all the tiles and make one raster
  all_files_after_tiles <- list.files(output_path, full.names = TRUE)
  grd_files <- grep("tif$", all_files_after_tiles, value = TRUE)

  grid_chr <- paste0("grd", grid_sizes)

  final_tifs <- sprintf("%s%s.tif", output_path, grid_chr)
  if (overwrite | !all(final_tifs %in% list.files(output_path, full.names = TRUE))) {

    # Combine all data
    rasters <- lapply(grd_files, terra::rast)

    # Sometimes tiles overlap. When it does, (I assume) resolution is not the same.
    # If resolutions aren't the same, tiles automatically won't be mergeable. We
    # use only the tiles that have a resolution of 30x30. In the process of changing
    # CRS and other manipulation, under a common CRS, a tile might have a resolution
    # of 29.9984 instead of 30. Both then can't be merged.
    resolution <- sapply(rasters, terra::res, simplify = FALSE)
    rasters <- rasters[sapply(resolution, identical, c(30, 30))]
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

    lapply(grid_sizes, \(size) {
      grd <- if (size == 30) {
        grd30
      } else {
        terra::aggregate(grd30, fact = size / 30, fun = mean_fun_ignore)
      }

      # Save the raster
      terra::writeRaster(grd, file = sprintf("%sgrd%s.tif", output_path, size),
                         overwrite = TRUE)

    })

  }


  # Switch the rasters to polygons, intersect with the masterpolygon (DA_carto)
  lapply(rev(grid_chr), \(x) {
    grd <- terra::rast(sprintf("%s%s.tif", output_path, x))
    output_file <- sprintf("%s%s.qs", output_path, x)
    if (!overwrite & output_file %in% all_files) {
      return(NULL)
    }

    # Convert SpatRaster to RasterLayer (if it has multiple layers, take the first one)
    r <- raster::raster(grd)

    # Create polygons from the raster
    sp_polygons <- raster::rasterToPolygons(r, dissolve = FALSE)

    # Convert the SpatialPolygonsDataFrame to an sf object
    sf_grid <- sf::st_as_sf(sp_polygons)
    sf_grid <- tibble::as_tibble(sf_grid)
    sf_grid <- sf::st_as_sf(sf_grid)

    for (n in names(grd)) {
      vls <- terra::values(grd[[n]])[,1]
      vls <- vls[!is.na(vls)]
      vls[vls == -9999] <- NA
      sf_grid[[n]] <- vls
    }

    tb <- sf::st_transform(sf_grid, crs)
    tb$ID <- sprintf("%s_%s", x, seq_along(tb$geometry))
    tb <- tb[c("ID", grep("ndvi", names(tb), value = TRUE), "geometry")]

    #   year_combinations <- t(utils::combn(years, 2))
    #   year_combinations <- split(year_combinations, seq(nrow(year_combinations)))
    #
    #   for (i in year_combinations) {
    #     first_year <- sprintf("ndvi_%s", i[[1]])
    #     second_year <- sprintf("ndvi_%s", i[[2]])
    #
    #     first_year_values <- tb[[first_year]]
    #     second_year_values <- tb[[second_year]]
    #
    #     # Replace -9999 with NA
    #     first_year_values[first_year_values == -9999] <- NA
    #     second_year_values[second_year_values == -9999] <- NA
    #
    #     # Perform the calculation
    #     vec <- second_year_values - first_year_values
    #     vec <- vec / first_year_values
    #     vec <- as.numeric(vec)
    #
    #     # Assign the calculated values back to grd30
    #     tb[[sprintf("ndvi_delta_%s_%s", i[[1]], i[[2]])]] <- vec
    #
    #     # Clean up
    #     rm(vec, first_year_values, second_year_values)
    #     gc()
    #   }

    # Save
    qs::qsave(tb, output_file)
  })


  return(NULL)

}


# Helper function to decode a specific bit
decode_bit <- function(value, bit_position) {
  quotient <- value %/% (2^bit_position)
  bit_value <- quotient %% 2
  return(bit_value)
}

# Define a function to decode QA values and form a table
decode_qa_values <- function() {
  # Define QA attributes and their bit positions
  qa_attributes <- list(
    "aerosol_quality" = c(6, 7),  # Bits 6-7
    "water" = 5,                  # Bit 5
    "snow_ice" = 4,               # Bit 4
    "cloud_shadow" = 3,           # Bit 3
    "adjacent_cloud" = 2,         # Bit 2
    "cloud" = 1,                  # Bit 1
    "cirrus" = 0                  # Bit 0
  )

  # Initialize a dataframe to store the results
  qa_values <- 0:255
  qa_table <- data.frame(qa_value = qa_values, stringsAsFactors = FALSE)

  # Decode each attribute for each QA value
  for (attribute in names(qa_attributes)) {
    if (length(qa_attributes[[attribute]]) == 2) {
      # For attributes with two bits (e.g., aerosol quality)
      bit1 <- qa_attributes[[attribute]][1]
      bit2 <- qa_attributes[[attribute]][2]
      qa_table[[attribute]] <- sapply(qa_values, function(value) {
        decode_bit(value, bit1) + 2 * decode_bit(value, bit2)
      })
    } else {
      # For attributes with one bit
      bit_position <- qa_attributes[[attribute]]
      qa_table[[attribute]] <- sapply(qa_values, decode_bit, bit_position = bit_position)
    }
  }

  # Add descriptions for aerosol quality
  qa_table$aerosol_quality_desc <- sapply(qa_table$aerosol_quality, function(x) {
    switch(as.character(x),
           "0" = "Climatology",
           "1" = "Low",
           "2" = "Average",
           "3" = "High")
  })

  return(qa_table)
}

