#' Retrieve GeoJSON Data from ArcGIS REST Services
#'
#' This function makes a GET request to an ArcGIS REST service and retrieves
#' data in GeoJSON format. It is designed to fetch all features and fields from
#' the specified service endpoint. The retrieved data is returned as a spatial
#' dataframe using the `sf` package.
#'
#' @param url <`character`> The base URL of the ArcGIS REST service endpoint.
#' The function ensures that the URL ends with "/query". If the URL does
#' not end with "/query", the function modifies it accordingly.
#'
#' @return An object of class `sf` representing the spatial dataframe containing
#'         the fetched data.
#' @export
arcgis_rest_services_ret <- function(url) {
  # Ensure URL ends with "query"
  if (!grepl("/query$", url)) {
    if (!grepl("/$", url)) {
      url <- paste0(url, "/")
    }
    url <- paste0(url, "query")
  }

  # Function to prepare URL and make GET request
  make_request <- function(url, format) {

    # Prepare the query with parameters
    query_params <- list(
      where = "1=1", # to get all the data; no filter
      outFields = "*", # to get all fields
      outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
      f = format, # output format
      returnGeometry = "true" # to ensure geometry is included
    )

    # Make the GET request
    response <- httr::GET(url = url, query = query_params)
    return(response)
  }

  # Try fetching data with 'geojson' format
  response <- make_request(url, "geojson")
  json_data <- httr::content(response, "text")

  # Check if the response contains an error
  if (grepl("\"error\"", json_data)) {
    # Retry with 'json' format if 'geojson' failed
    response <- make_request(url, "json")
    json_data <- httr::content(response, "text")

    # Check again for error
    if (grepl("\"error\"", json_data)) {
      stop("Error in response: ", json_data)
    }
  }

  # Convert the successful response to a spatial dataframe using sf
  sf_data <- sf::st_read(json_data, quiet = TRUE)
}
