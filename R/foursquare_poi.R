#' @title Connect to Foursquare Database
#' @description Establishes a connection to the Snowflake Foursquare database using environment variables.
#' @return A database connection object.
#' @export
foursquare_connect <- function() {
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
#' @return A data frame containing POI attributes including ID, name, category, coordinates, location, and dates.
foursquare_get_canada <- function(conn) {
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
#' @description Intersects POI data with geographic boundaries and calculates yearly cumulative POI counts.
#' @param poi_df A data frame of Foursquare POIs with geographic coordinates and creation/closure dates.
#' @param geo_sf An sf object containing geographic boundaries (must include `ID`).
#' @return A wide-format data frame of yearly POI totals by region, with `ID`.
poi_data <- function(poi_df, geo_sf) {
  poi_sf <- poi_df |>
    dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  poi_intersect <- sf::st_intersection(poi_sf, geo_sf)
  
  poi_data_no_geom <- poi_intersect |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      year_created = lubridate::year(lubridate::ymd(DATE_CREATED)),
      year_closed = lubridate::year(lubridate::ymd(DATE_CLOSED))
    ) |>
    dplyr::select(ID, year_created, year_closed)
  
  years_range <- seq(min(poi_data_no_geom$year_created, na.rm = TRUE),
                     max(lubridate::year(Sys.Date())), 1)
  
  poi_counts_created <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_created)) |>
    dplyr::count(ID, year_created) |>
    dplyr::rename(year = year_created, poi_added = n)
  
  poi_counts_closed <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_closed)) |>
    dplyr::count(ID, year_closed) |>
    dplyr::rename(year = year_closed, poi_closed = n)
  
  poi_evolution <- dplyr::full_join(poi_counts_created, poi_counts_closed, by = c("id", "year")) |>
    (\(df) dplyr::mutate(df, across(everything(), ~ replace(., is.na(.), 0))))() |>
    dplyr::arrange(ID, year)
  
  poi_evolution <- poi_evolution |>
    dplyr::group_by(ID) |>
    dplyr::mutate(poi_total = cumsum(poi_added) - cumsum(poi_closed)) |>
    dplyr::ungroup()
  
  poi_evolution <- poi_evolution |>
    dplyr::select(ID, year, poi_total) |>
    dplyr::mutate(year = paste0("poi_", year)) |>
    tidyr::pivot_wider(names_from = year, values_from = poi_total, values_fill = list(poi_total = 0))
  
}

#' @title Process POI Evolution
#' @description Intersects POI data with geographic boundaries and calculates yearly cumulative POI counts.
#' @param poi_df A data frame of Foursquare POIs with geographic coordinates and creation/closure dates.
#' @param geo_sf An sf object containing geographic boundaries (must include `ID`).
#' @return A data.frame in wide format with one row per region (`ID`) and one column per year (`poi_YYYY`) representing the cumulative number of active POIs for that year.
#' @export
poi_run_evolution <- function(geo_list) {
  conn <- foursquare_connect()
  poi_df <- foursquare_get_canada(conn)
  
  results <- lapply(geo_list, function(geo_sf) {
    poi_data(poi_df = poi_df, geo_sf = geo_sf)
  })
  
  return(results)
}


