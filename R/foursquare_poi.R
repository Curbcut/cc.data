#' @title Connect to Foursquare Database
#' @description Establishes a connection to the Snowflake Foursquare database using environment variables.
#' @return A database connection object.
#' @export
foursquare_connect <- function() {
  message("Connecting to Foursquare Snowflake database...")
  DBI::dbConnect(
    odbc::odbc(),
    Driver = "SnowflakeDSIIDriver",
    Server = Sys.getenv("SNOWFLAKE_SERVER"),
    UID = Sys.getenv("SNOWFLAKE_UID"),
    PWD = Sys.getenv("SNOWFLAKE_PWD"),
    Database = Sys.getenv("SNOWFLAKE_DB"),
    Schema = Sys.getenv("SNOWFLAKE_SCHEMA")
  )
}

#' @title Get All Foursquare POIs in Canada
#' @description Retrieves all Points of Interest (POIs) from the Foursquare open source database for Canada.
#' @param conn A database connection object from `foursquare_connect`.
#' @return A data frame containing POI attributes including id, name, category, coordinates, location, and dates.
foursquare_get_canada <- function(conn) {
  message("Retrieving Foursquare POIs for Canada...")
  query <- "
    SELECT fsq_place_id, name, fsq_category_labels, latitude, longitude,
           locality, region, country,
           date_created, date_refreshed, date_closed
    FROM FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES
    WHERE COUNTRY = 'CA';
  "
  DBI::dbGetQuery(conn, query)
}

#' @title Process POI Evolution
#' @description Matches POI data to geographic boundaries using spatial index
#' and calculates yearly cumulative POI counts.
#' @param poi_df A data frame of Foursquare POIs with geographic coordinates and creation/closure dates.
#' @param geo_sf An sf object containing geographic boundaries (must include `ID` or `geouid`).
#' @return A wide-format data frame of yearly POI totals by region, with `id`.
poi_data <- function(poi_df, geo_sf) {
  message("Converting POI data to spatial format...")
  poi_sf <- poi_df |>
    dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  message("Finding intersections using st_intersects()...")
  matched <- sf::st_intersects(poi_sf, geo_sf)
  
  message("Assigning geographic IDs to POIs...")
  poi_sf$id <- sapply(matched, function(x) {
    if (length(x) > 0) geo_sf$geouid[x[1]] else NA
  })
  
  message("Processing POI creation and closure dates...")
  poi_data_no_geom <- poi_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(id)) |>
    dplyr::mutate(
      year_created = lubridate::year(lubridate::ymd(DATE_CREATED)),
      year_closed = lubridate::year(lubridate::ymd(DATE_CLOSED))
    ) |>
    dplyr::select(id, year_created, year_closed)
  
  years_range <- seq(min(poi_data_no_geom$year_created, na.rm = TRUE),
                     max(lubridate::year(Sys.Date())), 1)
  
  message("Counting created POIs per year...")
  poi_counts_created <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_created)) |>
    dplyr::count(id, year_created) |>
    dplyr::rename(year = year_created, poi_added = n)
  
  message("Counting closed POIs per year...")
  poi_counts_closed <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_closed)) |>
    dplyr::count(id, year_closed) |>
    dplyr::rename(year = year_closed, poi_closed = n)
  
  message("Computing cumulative POI totals by year...")
  poi_evolution <- dplyr::full_join(poi_counts_created, poi_counts_closed, by = c("id", "year")) |>
    dplyr::mutate(across(c(poi_added, poi_closed), ~ replace(., is.na(.), 0))) |>
    dplyr::arrange(id, year) |>
    dplyr::group_by(id) |>
    dplyr::mutate(poi_total = cumsum(poi_added) - cumsum(poi_closed)) |>
    dplyr::ungroup()
  
  message("Reshaping output to wide format...")
  poi_evolution |>
    dplyr::select(id, year, poi_total) |>
    dplyr::mutate(year = paste0("point_of_interest_", year)) |>
    tidyr::pivot_wider(names_from = year, values_from = poi_total, values_fill = list(poi_total = 0))
}


#' @title Process POI Evolution for Multiple Geographies
#' @description Connects to the Foursquare database, retrieves POIs for Canada,
#' and processes each geographic layer to calculate yearly cumulative POI counts.
#' @param geo_list A named list of geographic `sf` objects (e.g., list(pr = pr_sf, cma = cma_sf)).
#'                 Each object must include a column named `ID` for regional grouping.
#' @return A named list of data frames, one per geography, each in wide format
#'         with cumulative POI totals per year and per region (ID).
#' @export
poi_run_evolution <- function(geo_list) {
  message("Starting POI evolution processing...")
  
  conn <- foursquare_connect()
  poi_df <- foursquare_get_canada(conn)
  
  results <- lapply(names(geo_list), function(name) {
    message(paste0("Processing layer: ", name))
    geo_sf <- geo_list[[name]]
    result <- poi_data(poi_df = poi_df, geo_sf = geo_sf)
    return(result)
  })
  
  names(results) <- names(geo_list)
  message("All layers processed successfully.")
  return(results)
}
