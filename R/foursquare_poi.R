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

#' @title Get Census Canada Geometry
#' @description Downloads census geometry for Canada (level "C") as an sf object.
#' @param census_year A string for the census dataset code (default = "CA21").
#' @return An sf object with `id`, and `geometry`.
census_canada_boundaries <- function(census_year = "CA21") {
  region_code <- cancensus::list_census_regions(census_year) |>
    dplyr::filter(level == "C")
  cancensus::get_census(dataset = census_year,
                        regions = setNames(list(region_code$region), "C"),
                        level = "C", vectors = NULL, geo_format = "sf") |>
    dplyr::select(id = GeoUID)
}

#' @title Get Province Geometry
#' @description Downloads census geometry for provinces (level "PR") as an sf object.
#' @param census_year A string for the census dataset code (default = "CA21").
#' @return An sf object with `id`, and `geometry`.
census_province_boundaries <- function(census_year = "CA21") {
  region_code <- cancensus::list_census_regions(census_year) |>
    dplyr::filter(level == "PR")
  cancensus::get_census(dataset = census_year,
                        regions = setNames(list(region_code$region), "PR"),
                        level = "PR", vectors = NULL, geo_format = "sf") |>
    dplyr::select(id = GeoUID)
}

#' @title Get CSD Geometry
#' @description Downloads census geometry for Census Subdivisions (CSD) as an sf object.
#' @param census_year A string for the census dataset code (default = "CA21").
#' @return An sf object with `id`, and `geometry`.
census_csd_boundaries <- function(census_year = "CA21") {
  region_code <- cancensus::list_census_regions(census_year) |>
    dplyr::filter(level == "CSD")
  cancensus::get_census(dataset = census_year,
                        regions = setNames(list(region_code$region), "CSD"),
                        level = "CSD", vectors = NULL, geo_format = "sf") |>
    dplyr::select(id = GeoUID)
}

#' @title Get CT Geometry
#' @description Downloads census geometry for Census Tracts (CT) as an sf object.
#' @param census_year A string for the census dataset code (default = "CA21").
#' @return An sf object with `id`, and `geometry`.
census_ct_boundaries <- function(census_year = "CA21") {
  region_code <- cancensus::list_census_regions(census_year) |>
    dplyr::filter(level == "CT")
  cancensus::get_census(dataset = census_year,
                        regions = setNames(list(region_code$region), "CT"),
                        level = "CT", vectors = NULL, geo_format = "sf") |>
    dplyr::select(id = GeoUID)
}

#' @title Get DA Geometry
#' @description Downloads census geometry for Dissemination Areas (DA) as an sf object.
#' @param census_year A string for the census dataset code (default = "CA21").
#' @return An sf object with `id`, and `geometry`.
census_da_boundaries <- function(census_year = "CA21") {
  region_code <- cancensus::list_census_regions(census_year) |>
    dplyr::filter(level == "DA")
  cancensus::get_census(dataset = census_year,
                        regions = setNames(list(region_code$region), "DA"),
                        level = "DA", vectors = NULL, geo_format = "sf") |>
    dplyr::select(id = GeoUID)
}

#' @title Process POI Evolution
#' @description Intersects POI data with geographic boundaries and calculates yearly cumulative POI counts.
#' @param poi_df A data frame of Foursquare POIs with geographic coordinates and creation/closure dates.
#' @param geo_sf An sf object containing geographic boundaries (must include `id`).
#' @return A wide-format data frame of yearly POI totals by region, with `id`.
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
    dplyr::select( id, year_created, year_closed)
  
  years_range <- seq(min(poi_data_no_geom$year_created, na.rm = TRUE),
                     max(lubridate::year(Sys.Date())), 1)
  
  poi_counts_created <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_created)) |>
    dplyr::count(id, year_created) |>
    dplyr::rename(year = year_created, poi_added = n)
  
  poi_counts_closed <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_closed)) |>
    dplyr::count(id, year_closed) |>
    dplyr::rename(year = year_closed, poi_closed = n)
  
  poi_evolution <- dplyr::full_join(poi_counts_created, poi_counts_closed, by = c("id", "year")) |>
    (\(df) dplyr::mutate(df, across(everything(), ~ replace(., is.na(.), 0))))() |>
    dplyr::arrange(id, year)
  
  poi_evolution <- poi_evolution |>
    dplyr::group_by(id) |>
    dplyr::mutate(poi_total = cumsum(poi_added) - cumsum(poi_closed)) |>
    dplyr::ungroup()
  
  poi_evolution <- poi_evolution |>
    dplyr::select(id, year, poi_total) |>
    dplyr::mutate(year = paste0("poi_", year)) |>
    tidyr::pivot_wider(names_from = year, values_from = poi_total, values_fill = list(poi_total = 0))
  
}

#' @title Process POI Evolution
#' @description Connects to Foursquare, retrieves POIs, downloads the selected census geometry, intersects both, and returns a wide-format dataset of cumulative POIs by year and region.
#' @param geo_level A string specifying the census level: "C", "PR", "CMA", "CSD", "CT", or "DA".
#' @param census_year A string specifying the census dataset code (default = "CA21").
#' @return An `sf` object with columns: `id`, yearly POI totals.
#' @export
poi_run_evolution <- function(geo_level, census_year) {
  conn <- foursquare_connect()
  poi_df <- foursquare_get_canada(conn)
  
  geo_sf <- switch(
    geo_level,
    "C"   = census_canada_boundaries(census_year),
    "PR"  = census_province_boundaries(census_year),
    "CMA" = cma_nash,
    "CSD" = census_csd_boundaries(census_year),
    "CT"  = census_ct_boundaries(census_year),
    "DA"  = census_da_boundaries(census_year),
    stop("Invalid geo_level. Use one of: 'C', 'PR', 'CMA', 'CSD', 'CT', 'DA'")
  )
  
  poi_data(poi_df = poi_df, geo_sf = geo_sf)
}


