#' Retrieve GeoJSON Data from ArcGIS REST Services
#'
#' Makes paginated GET requests to an ArcGIS REST service and retrieves data
#' as GeoJSON (or JSON fallback). Optionally returns spatial features using \pkg{sf}.
#'
#' @param url character. Base URL of the ArcGIS REST service endpoint.
#'   If it does not end with "/query", the function appends it.
#' @param limit integer. Maximum number of records per request (default: 1000).
#'   Controls page size for the API.
#' @param sf logical. If \code{TRUE} (default), returns an \code{sf} object
#'   with geometry; if \code{FALSE}, returns a tibble without geometry.
#'
#' @return If \code{sf = TRUE}, an \code{sf} object (rows bound across pages).
#'   If \code{sf = FALSE}, a \code{tibble} (rows bound across pages).
#'
#' @export
arcgis_rest_services_ret <- function(url, limit = 1000, sf = TRUE) {
  # Ensure URL ends with "query"
  if (!grepl("/query$", url)) {
    if (!grepl("/$", url)) {
      url <- paste0(url, "/")
    }
    url <- paste0(url, "query")
  }

  # Function to prepare URL and make GET request with pagination
  make_request <- function(url, format, offset = 0, limit = 1000, sf) {
    # Prepare the query with parameters including paging
    query_params <- list(
      where = "1=1", # to get all the data; no filter
      outFields = "*", # to get all fields
      outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
      f = format, # output format
      returnGeometry = ifelse(sf, "true", "false"), # conditionally include spatial data
      resultOffset = offset, # start from this record
      resultRecordCount = limit # number of records to return
    )

    # Make the GET request
    response <- httr::GET(url = url, query = query_params)

    # Check if the response is an error (HTML)
    content_type <- httr::headers(response)$`content-type`
    if (grepl("text/html", content_type)) {
      stop("Received HTML content instead of JSON/GeoJSON. Check if the URL is correct or the request exceeds server limits.")
    }

    return(response)
  }

  # Function to get total record count
  get_total_count <- function(url) {
    query_params <- list(
      where = "1=1", # to get all the data; no filter
      returnCountOnly = "true", # request only the count of records
      f = "json" # output format
    )

    response <- httr::GET(url = url, query = query_params)
    content <- httr::content(response, "text")

    # Parse the response and extract the count
    json_data <- jsonlite::fromJSON(content)
    if (!is.null(json_data$error)) {
      stop("Error fetching record count: ", json_data$error$message)
    }

    return(json_data$count)
  }

  # Get total record count
  total_records <- get_total_count(url)

  # Initialize variables for paging
  offset <- 0
  all_data <- list()
  format <- "geojson"

  while (offset < total_records) {
    response <- make_request(url, format, offset, limit, sf)
    json_data <- httr::content(response, "text")

    # Check if the response contains an error
    if (grepl("\"error\"", json_data)) {
      # Retry with 'json' format if 'geojson' failed
      format <- "json"
      response <- make_request(url, format, offset, limit, sf)
      json_data <- httr::content(response, "text")

      # Check again for error
      if (grepl("\"error\"", json_data)) {
        stop("Error in response: ", json_data)
      }
    }

    # If geometry is requested (sf = TRUE), try reading the data as spatial dataframe using sf
    if (sf) {
      sf_data <- sf::st_read(json_data, quiet = TRUE)

      # If no data is returned, we are done
      if (nrow(sf_data) == 0) {
        message("All data retrieved.")
        break
      }

      # Append data
      all_data <- append(all_data, list(sf_data))
    } else {
      # Extract and unnest all 'list' columns dynamically
      json_data_parsed <- jsonlite::fromJSON(json_data)

      # Identify columns that are lists and unnest them
      unnested_data <- json_data_parsed$features |>
        tidyr::unnest_wider(where(is.list)) |>
        tibble::as_tibble()

      all_data <- append(all_data, list(unnested_data))
    }

    # If the offset + default limit is equal to the total_records, it means
    # all data has been fetched already.
    if ((offset + limit) == total_records) break

    # Update offset for next page
    offset <- offset + limit

    # If the new offset + default limit would exceed total_records, update the limit
    if ((offset + limit) > total_records) {
      limit <- total_records - offset
    }
  }

  # Combine all chunks into one spatial dataframe or list
  if (sf) {
    combined_data <- do.call(rbind, all_data)
  } else {
    combined_data <- do.call(rbind, all_data)
  }

  return(combined_data)
}

