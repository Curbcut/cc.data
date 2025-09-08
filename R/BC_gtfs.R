#' Download from GTFS (Only British Columbia)
#'
#' Downloads GTFS data from https://database.mobilitydata.org/.
#' Filters for Canada and active zip files and downloads them to dest_folder.
#'
#' @param dest_folder Path to the directory to save the downloaded zip files.
#' If not provided, default is the system's temporary directory.
#'
#' @return Returns the `dest_folder`. Files are downloaded to the `dest_folder`
#' with a `gtfs_transit_feed` pattern in the file name.
#' @export
gtfs_download_BC <- function(dest_folder = tempdir()) {

  # Download CSV of GTFS links from here https://database.mobilitydata.org/
  temp <- tempfile()
  utils::download.file("https://bit.ly/catalogs-csv", temp, quiet = TRUE)
  zip_links <- tibble::as_tibble(utils::read.csv(temp))

  # Filter Canada and active zips
  zips <- zip_links[
    zip_links$location.country_code == "CA" &
      zip_links$data_type == "gtfs" &
      !zip_links$status %in% c("deprecated") &
      zip_links$location.subdivision_name == "British Columbia",
  ]

  # Download all zips
  sapply(seq_len(nrow(zips)), \(x) {
    prov <- tolower(zips$provider[x])
    prov <- gsub("[^a-z.]", "", prov)
    tmp <- tempfile(pattern = "gtfs_transit_feed", tmpdir = tempdir(),
                    fileext = ".zip")
    utils::download.file(url = zips$urls.latest[x],
                         destfile = tmp,
                         mode = "wb",
                         quiet = TRUE
    )
  }, USE.NAMES = FALSE) |> invisible()

  # Return the location
  return(dest_folder)
}

#' Identify High-Frequency GTFS Stops in British Columbia
#'
#' This function downloads the GTFS (General Transit Feed Specification) data
#' for British Columbia, combines it, and identifies stops that are serviced
#' frequently (every 15 minutes on average) during specific time windows on
#' weekdays, Saturdays, and Sundays. This will be used for the new
#' small-scale, multi-unit housing policy.
#'
#' @details The function first downloads the GTFS data specific to British
#' Columbia using `gtfs_download_BC` and combines it with `gtfs_combine`.
#' The GTFS data is then read and processed. The `stop_times` are ordered
#' chronologically. Arrival and departure times are converted to seconds.
#' The function `is_high_freq_stop` checks if a stop is serviced at least
#' 48 times (weekdays) or 32 times (weekends) between specified time ranges,
#' indicating high frequency (every 15 minutes). The function iterates over each stop and route
#' to assess the frequency. Finally, stops meeting the high-frequency criteria
#' across all specified time ranges are returned as an `sf` object with
#' coordinates in the CRS format (EPSG:4326).
#' @param DA_table <sf data.frame> Polygones/centroïdes utilisés pour agréger les arrêts

#'
#' @return An sf object of stops in British Columbia that are serviced
#' every 15 minutes on average during the specified time windows on
#' weekdays, Saturdays, and Sundays. The sf object includes stop coordinates
#' in longitude and latitude.
#'
#' @export
gtfs_frequent_stops_BC <- function(DA_table) {

  # Download the GTFS for BC
  BC_zips <- gtfs_download_BC()
  gtfs <- gtfs_combine(BC_zips)
  gtfs_data <- gtfstools::read_gtfs(gtfs)
  gtfs_data <- lapply(gtfs_data, tibble::as_tibble)

  # Ensure that stop_times are in chronological order
  # DOES NOT PASS R CHECK
  # gtfs_data$stop_times <- gtfs_data$stop_times[with(gtfs_data$stop_times, order(trip_id, arrival_time)), ]
  gtfs_data$stop_times <- gtfs_data$stop_times[order(gtfs_data$stop_times$trip_id, gtfs_data$stop_times$arrival_time), ]

  # Convert time fields to seconds for easier calculation
  gtfs_data$stop_times$arrival_time <- sapply(gtfs_data$stop_times$arrival_time, gtfstools:::string_to_seconds)
  gtfs_data$stop_times$departure_time <- sapply(gtfs_data$stop_times$departure_time, gtfstools:::string_to_seconds)

  # Get the service IDs that are on weekday and oin weekend
  weekday_service_ids <- gtfs_data$calendar[
    gtfs_data$calendar$monday == 1 |
      gtfs_data$calendar$tuesday == 1 |
      gtfs_data$calendar$wednesday == 1 |
      gtfs_data$calendar$thursday == 1 |
      gtfs_data$calendar$friday == 1,
  ]
  weekend_service_ids <- gtfs_data$calendar[
    gtfs_data$calendar$saturday == 1 |
      gtfs_data$calendar$sunday == 1,
  ]

  # Function to check if a stop is serviced every 15 minutes on average (depending
  # if it's wekday or not)
  is_high_freq_stop <- function(trip_ids, weekday = TRUE, stop_times, start_time, end_time, times = 48) {

    # Is the trip passing to that stop from monday to friday? Or both saturday and sunday?
    service_ids <- gtfs_data$trips$service_id[gtfs_data$trips$trip_id %in% trip_ids]

    services <- if (weekday) weekday_service_ids else weekend_service_ids
    these_services <- services[services$service_id %in% service_ids, ]

    days <- if (weekday) c("monday", "tuesday", "wednesday", "thursday", "friday") else c("saturday", "sunday")
    all_days <- all(apply(these_services[, days], 1, function(x) any(x == 1)))

    if (!all_days) return(FALSE)

    # Subset stop_times for the relevant time window
    relevant_stop_times <- subset(stop_times, arrival_time >= start_time & arrival_time <= end_time)

    # Remove service_id as sometimes it's different depending on the day of the week
    relevant_stop_times <- unique(relevant_stop_times[2:ncol(relevant_stop_times)])

    # If the same route passes 48 times between 7 am and 7pm, then it passes at least
    # every 15 minutes on average
    nrow(relevant_stop_times) >= times
  }

  # Define time ranges in seconds
  times <- list(
    weekdays = c(gtfstools:::string_to_seconds("07:00:00"), gtfstools:::string_to_seconds("19:00:00")),
    saturday = c(gtfstools:::string_to_seconds("10:00:00"), gtfstools:::string_to_seconds("18:00:00")),
    sunday = c(gtfstools:::string_to_seconds("10:00:00"), gtfstools:::string_to_seconds("18:00:00"))
  )

  # Initialize a list to store high frequency stops by route
  high_freq_stops <- list()

  # Assuming gtfs_data$stops$stop_id is a vector of stop IDs
  stop_ids <- unique(gtfs_data$stops$stop_id)

  progressr::with_progress({
    pb <- progressr::progressor(length(stop_ids))
    results <- future.apply::future_lapply(stop_ids, function(stop_id) {
      stop_times <- gtfs_data$stop_times[gtfs_data$stop_times$stop_id == stop_id, ]

      agency <- gsub("___.*", "", stop_id)
      possible_routes <- gtfs_data$routes$route_id[
        grepl(agency, gtfs_data$routes$route_id)
      ]

      out <- lapply(unique(possible_routes), function(route_id) {
        trip_ids <- gtfs_data$trips$trip_id[gtfs_data$trips$route_id == route_id]
        route_stop_times <- stop_times[stop_times$trip_id %in% trip_ids, ]

        tibble::tibble(
          stop_id = stop_id,
          route_id = route_id,
          weekday_freq = is_high_freq_stop(trip_ids, weekday = TRUE,
                                           route_stop_times, times$weekdays[1],
                                           times$weekdays[2]),
          saturday_freq = is_high_freq_stop(trip_ids, weekday = FALSE,
                                            route_stop_times, times$saturday[1],
                                            times$saturday[2], times = 32),
          sunday_freq = is_high_freq_stop(trip_ids, weekday = FALSE,
                                          route_stop_times, times$sunday[1],
                                          times$sunday[2], times = 32)
        )
      })

      pb()
      data.table::rbindlist(out)

    })
  })

  # Convert the list to a data frame
  high_freq_stops_df <- data.table::rbindlist(results)

  # Filter stops that meet the frequency criteria for all specified time ranges
  high_freq_stops_df <- high_freq_stops_df[high_freq_stops_df$weekday_freq & high_freq_stops_df$saturday_freq & high_freq_stops_df$sunday_freq, ]

  # Unique stops
  stops <- unique(high_freq_stops_df$stop_id)
  stops <- gtfs_data$stops[gtfs_data$stops$stop_id %in% stops, ]
  stops <- sf::st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

  # Add DA ID
  DA_table <- sf::st_transform(DA_table, 4326)
  stops_DA <- sf::st_join(stops, DA_table)
  stops_DA <- stops_DA["ID"]
  names(stops_DA)[1] <- "DA_ID"

  # Write to AWS table
  stops_DA <- sf::st_transform(stops_DA, 3347)
  db_write_table(stops_DA, tb_name = "BC_frequent_bus_stops", index = "DA_ID")

  return(stops)
}
