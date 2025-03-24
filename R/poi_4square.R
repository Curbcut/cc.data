#' @title Connect to Snowflake Database
#' @description This function establishes a connection to the Snowflake database using credentials stored in `.Renviron`.
#' @return A `DBI` connection object.
#' @export
connect_to_db <- function() {
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SnowflakeDSIIDriver",
    Server = Sys.getenv("SNOWFLAKE_SERVER"),
    UID = Sys.getenv("SNOWFLAKE_UID"),
    PWD = Sys.getenv("SNOWFLAKE_PWD"),
    Database = Sys.getenv("SNOWFLAKE_DB"),
    Schema = Sys.getenv("SNOWFLAKE_SCHEMA")
  )
  return(conn)
}


#' @title Extract and Process Points of Interest (POI) for Canadian CMAs
#' @description This function retrieves Points of Interest from a database, associates them with CMAs,
#' calculates the number of active POIs per year, and creates a structured dataset.
#' @param conn A database connection object obtained from `connect_to_db()`.
#' @param cma_nash A spatial dataframe (`sf`) containing Canadian CMAs with geometries.
#' @return An `sf` object containing POI evolution data per year for each CMA.
#' @export
process_poi_cma <- function(conn, cma_nash) {
  
  # Initialize a list to store results
  results_list <- list()
  
  # Process each CMA in the dataset
  for (i in 1:nrow(cma_nash)) {
    
    cma_name <- cma_nash$name[i]
    cma_geometry <- cma_nash[i, ]  # Extract CMA geometry
    
    message("Processing CMA: ", cma_name)  # Display the CMA being processed
    
    # Define an expanded bounding box around the CMA
    cma_bbox <- sf::st_bbox(cma_geometry)
    lat_min <- cma_bbox$ymin - 0.5
    lat_max <- cma_bbox$ymax + 0.5
    lon_min <- cma_bbox$xmin - 0.5
    lon_max <- cma_bbox$xmax + 0.5
    
    # Construct SQL query to retrieve POIs within the bounding box
    query_cma <- sprintf("
    SELECT fsq_place_id, name, fsq_category_labels, latitude, longitude, locality, region, country,
           date_created, date_refreshed, date_closed
    FROM FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES
    WHERE COUNTRY = 'CA'
    AND LATITUDE BETWEEN %f AND %f
    AND LONGITUDE BETWEEN %f AND %f;
    ", lat_min, lat_max, lon_min, lon_max)
    
    # Execute SQL query and retrieve POI data
    poi_cma_df <- DBI::dbGetQuery(conn, query_cma)
    
    if (nrow(poi_cma_df) == 0) next  # Skip if no POI data is found for the CMA
    
    # Convert retrieved POI data into spatial format
    poi_cma_sf <- sf::st_as_sf(poi_cma_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    
    # Perform spatial intersection to keep only POIs inside the CMA
    poi_cma_final <- sf::st_intersection(poi_cma_sf, cma_geometry)
    
    # Add CMA name
    poi_cma_final <- poi_cma_final |> dplyr::mutate(CMA = cma_name)
    
    # Store processed results for the CMA
    results_list[[cma_name]] <- poi_cma_final
  }
  
  # Merge all CMA POI data into a single `sf` object
  all_cma_poi_sf <- do.call(rbind, results_list)
  
  # Remove geometry for analysis
  poi_data_no_geom <- all_cma_poi_sf |>
    sf::st_drop_geometry() |>  # Remove spatial component
    dplyr::mutate(
      year_created = lubridate::year(lubridate::ymd(DATE_CREATED)),  # Extract year from creation date
      year_closed = lubridate::year(lubridate::ymd(DATE_CLOSED))     # Extract year from closure date
    )
  
  # Determine the full range of years
  years_range <- seq(min(poi_data_no_geom$year_created, na.rm = TRUE), 
                     max(lubridate::year(Sys.Date())), 1)
  
  # Count POI openings and closures per year per CMA
  poi_counts_created <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_created)) |>
    dplyr::count(CMA, year_created) |>
    dplyr::rename(year = year_created, poi_added = n)
  
  poi_counts_closed <- poi_data_no_geom |>
    dplyr::filter(!is.na(year_closed)) |>
    dplyr::count(CMA, year_closed) |>
    dplyr::rename(year = year_closed, poi_closed = n)
  
  # Merge counts and fill missing values with 0
  poi_evolution <- dplyr::full_join(poi_counts_created, poi_counts_closed, by = c("CMA", "year")) |>
    (\(df) dplyr::mutate(df, across(everything(), ~ replace(., is.na(.), 0))))() |>
    dplyr::arrange(CMA, year)
  
  # Compute cumulative active POI count
  poi_evolution <- poi_evolution |>
    dplyr::group_by(CMA) |>
    dplyr::mutate(poi_total = cumsum(poi_added) - cumsum(poi_closed)) |>
    dplyr::ungroup()
  
  # Pivot to have years as columns
  poi_evolution <- poi_evolution |>
    dplyr::select(CMA, year, poi_total) |>
    dplyr::mutate(year = paste0("poi_", year)) |>
    tidyr::pivot_wider(names_from = year, values_from = poi_total, values_fill = list(poi_total = 0))
  
  # Ensure all CMAs are included (even if no POI in some years)
  poi_evolution <- cma_nash |>
    (\(df) df[, c("name","geouid", "geometry")])() |>
    (\(df) { colnames(df)[colnames(df) == "name"] <- "CMA"; df })() |>
    (\(df) merge(df, poi_evolution, by = "CMA", all.x = TRUE))() |>
    (\(df) { df[is.na(df)] <- 0; df })()
  
  
  return(poi_evolution)
}