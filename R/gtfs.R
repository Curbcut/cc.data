#' Download from GTFS
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
gtfs_download <- function(dest_folder = tempdir()) {
  # Download CSV of GTFS links from here https://database.mobilitydata.org/
  temp <- tempfile()
  utils::download.file("https://bit.ly/catalogs-csv", temp, quiet = TRUE)
  zip_links <- tibble::as_tibble(utils::read.csv(temp))

  # Filter Canada and active zips
  zips <- zip_links[
    zip_links$location.country_code == "CA" &
      zip_links$data_type == "gtfs" &
      !zip_links$status %in% c("deprecated"),
  ]

  # Download all zips
  sapply(
    seq_len(nrow(zips)),
    \(x) {
      prov <- tolower(zips$provider[x])
      prov <- gsub("[^a-z.]", "", prov)
      tmp <- tempfile(
        pattern = "gtfs_transit_feed",
        tmpdir = tempdir(),
        fileext = ".zip"
      )
      utils::download.file(
        url = zips$urls.latest[x],
        destfile = tmp,
        mode = "wb",
        quiet = TRUE
      )
    },
    USE.NAMES = FALSE
  ) |>
    invisible()

  # Return the location
  return(dest_folder)
}

#' Combine gtfs zips to a unique zip
#'
#' Reads in all GTFS zip files in `dest_folder`, merges them, and writes the
#' resulting merged GTFS data to a new zip file in dest_folder.
#'
#' @param dest_folder Path to the directory where the gtfs zip files are saved,
#' and where the final combined gtfs zip file should be. If not provided,
#' the gtfs zip files are downloaded using \code{\link[cc.data]{gtfs_download}}
#'
#' @return Path to the zip file containing the merged GTFS data.
#' @export
gtfs_combine <- function(dest_folder = gtfs_download()) {
  if (!requireNamespace("gtfstools", quietly = TRUE)) {
    stop(
      "Package \"gtfstools\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Read in the GTFS zip files
  gtfs_zip <- list.files(
    dest_folder,
    full.names = TRUE,
    pattern = "gtfs_transit_feed"
  )
  gtfs_zip <- gtfs_zip[sapply(
    gtfs_zip,
    \(x) {
      z <- utils::unzip(x, list = TRUE)
      all(
        paste0(
          c(
            "agency",
            "routes",
            "trips",
            "calendar",
            "calendar_dates",
            "stop_times",
            "shapes",
            "stops"
          ),
          ".txt"
        ) %in%
          z$Name
      )
    },
    USE.NAMES = FALSE
  )]

  # Process each GTFS feed
  gtfs <- lapply(gtfs_zip, \(zip_path) {
    # Read in the necessary files from the GTFS feed
    gtfs_data <- gtfstools::read_gtfs(
      zip_path,
      files = c(
        "agency",
        "routes",
        "trips",
        "calendar",
        "calendar_dates",
        "stop_times",
        "shapes",
        "stops"
      )
    )

    # Check if there is exactly one agency; if not, handle accordingly
    if (nrow(gtfs_data$agency) != 1) {
      stop("Each GTFS feed must have exactly one agency.", call. = FALSE)
    }

    # Prepend agency_id to stop_id.
    agency_id <- gtfs_data$agency$agency_name[1]

    for (i in names(gtfs_data)) {
      if (!"stop_id" %in% names(gtfs_data[[i]])) {
        next
      }

      gtfs_data[[i]]$stop_id <- paste0(agency_id, "___", gtfs_data[[i]]$stop_id)
    }

    # Same for route_id
    for (i in names(gtfs_data)) {
      if (!"route_id" %in% names(gtfs_data[[i]])) {
        next
      }

      gtfs_data[[i]]$route_id <- paste0(
        agency_id,
        "___",
        gtfs_data[[i]]$route_id
      )
    }

    # Same for trip_id
    for (i in names(gtfs_data)) {
      if (!"trip_id" %in% names(gtfs_data[[i]])) {
        next
      }

      gtfs_data[[i]]$trip_id <- paste0(agency_id, "___", gtfs_data[[i]]$trip_id)
    }

    # Same for service_id
    for (i in names(gtfs_data)) {
      if (!"service_id" %in% names(gtfs_data[[i]])) {
        next
      }

      gtfs_data[[i]]$service_id <- paste0(
        agency_id,
        "___",
        gtfs_data[[i]]$service_id
      )
    }

    return(gtfs_data)
  })

  gtfs <- gtfstools::merge_gtfs(gtfs)

  tmp <- tempfile(fileext = ".zip")

  # Return the final zip file location
  gtfstools::write_gtfs(
    gtfs = gtfs,
    path = tmp,
    quiet = TRUE,
    standard_only = TRUE
  )
  return(tmp)
}

#' Extract GTFS to create a GTFS object
#'
#' @param gtfs_zip Path to the GTFS zip file to extract and create a transfer
#' table for. If not provided, the gtfs zip file is built using
#' \code{\link[cc.data]{gtfs_combine}}.
#' @param maximum_walk_time Maximum walking time in seconds for transfer connections.
#'
#' @return Returns a GTFS object.
#' @export
gtfs_extract <- function(gtfs_zip = gtfs_combine(), maximum_walk_time = 800) {
  if (!requireNamespace("gtfsrouter", quietly = TRUE)) {
    stop(
      "Package \"gtfsrouter\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Extract the GTFS zip
  gtfs <- suppressWarnings(gtfsrouter::extract_gtfs(gtfs_zip, quiet = TRUE))

  # Create a transfer table
  gtfs <-
    gtfsrouter::gtfs_transfer_table(gtfs = gtfs, d_limit = maximum_walk_time)

  # Return
  return(gtfs)
}

#' Get transit traveltimes from all transit stop to every transit stop.
#'
#' Calculates travel times from a given GTFS zip file for a specific day
#' and time period. Returns a list of data frames, each containing the start
#' time, duration, and destination stop ID for each travel time.
#'
#' @param gtfs GTFS object used for calculating travel times.
#' The output of \code{\link[cc.data]{gtfs_extract}}.
#' @param day Day of the week to calculate travel times for.
#' Valid options are "monday", "tuesday", "wednesday", "thursday", "friday",
#' "saturday", and "sunday".
#' @param start_time_limits Two-element numeric vector specifying the start
#' time range in seconds since midnight to calculate travel times for.
#' @param max_traveltime Maximum travel time in seconds.
#'
#' @return Named list of data frames, each containing the start time, duration,
#' and destination stop ID for each travel time.
#' @export
gtfs_get_traveltimes <- function(
  gtfs,
  day = "wednesday",
  start_time_limits = c(8, 8.5) * 3600,
  max_traveltime = 60 * 60
) {
  if (!requireNamespace("gtfsrouter", quietly = TRUE)) {
    stop(
      "Package \"gtfsrouter\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Filter just the day
  gtfs_day_filtered <- gtfsrouter::gtfs_timetable(gtfs, day = day)

  # Calculate all the travel times for a time period (day and time)
  progressr::with_progress({
    pb <- progressr::progressor(
      steps = length(unique(gtfs_day_filtered$stops$stop_id))
    )
    traveltimes <-
      sapply(
        unique(gtfs_day_filtered$stops$stop_id),
        \(x) {
          out <- gtfsrouter::gtfs_traveltimes(
            gtfs = gtfs_day_filtered,
            from = x,
            from_is_id = TRUE,
            # day = day,
            start_time_limits = start_time_limits,
            max_traveltime = max_traveltime
          )
          pb()
          return(out)
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )
  })

  # Subset only relevant columns and filter out negative durations in
  # traveltimes (bug most possibly from gtfsrouter::gtfs_traveltimes)
  traveltimes <- lapply(traveltimes, \(x) {
    if (is.data.frame(x) && nrow(x) > 0) {
      return(tibble::as_tibble(x[
        x$duration > 0,
        c("start_time", "duration", "stop_id")
      ]))
    }
    return(NULL)
  })

  # If empty, filter out
  traveltimes <- traveltimes[!sapply(traveltimes, is.null)]

  # Return
  return(traveltimes)
}

#' Preparation to get traveltime transit matrix
#'
#' PART 1 OF 2 FOR TRAVEL TIME MATRICE CALCULATIONS. PART 2:
#' \code{\link[cc.data]{gtfs_traveltime_matrix_final}}
#' The two functions calculate travel times from a set of origin points to a set of
#' destination points using transit. It takes in a GTFS list, a traveltimes list,
#' and a table of origin/destination points (referred to as DA_table) and returns
#' a named list which, for every DA, lists every reachable DA and the travel time.
#' The user can also specify a maximum walk distance (default is 800 meters) and a
#' routing server for walking time from the origin/destination point to the transit
#' stops (default is on local machine using OSRM: "http://localhost:5000/". See
#' \code{\link[cc.data]{tt_local_osrm}}).
#'
#' @param gtfs <`gtfs`> A GTFS list containing information on transit. See
#' \code{\link[cc.data]{gtfs_combine}}
#' @param traveltimes <`named list`> Contains information on travel times between
#' transit stops. See \code{\link[cc.data]{gtfs_get_traveltimes}}
#' @param DA_table <`sf data.frame`> Object containing origin/destination
#' polygons. The origin/destination points will be the centroids.
#' @param maximum_walk_dist A numeric value specifying the maximum amount of
#' meters that a person is willing to walk to a transit stop.
#' Default is 800.
#' @param routing_server A string specifying the url of the routing server to
#' use for calculating walking times. Default is local machine using OSRM:
#' "http://localhost:5000/". See \code{\link[cc.data]{tt_local_osrm}})
#'
#' @return Final part \code{\link[cc.data]{gtfs_traveltime_matrix_final}}: A
#' named list containing the travel times from each origin point to
#' each destination point (both are IDs of `DA_table`).
#' @export
gtfs_traveltime_matrix_prep <- function(
  gtfs,
  traveltimes,
  DA_table,
  maximum_walk_dist = 800,
  routing_server = "http://localhost:5000/"
) {
  # On reste en sf, pas de sp/rgeos
  # Assure un CRS métrique pour les buffers/intersections
  DA_table <- DA_table["ID"]
  if (sf::st_is_longlat(DA_table)) {
    DA_table <- sf::st_transform(DA_table, 3347) # NAD83 / StatCan Lambert (mètres)
  }

  DA_centroids_m <- sf::st_centroid(DA_table) # mètres
  DA_buffer_m <- sf::st_buffer(DA_centroids_m, maximum_walk_dist)

  # Copie lon/lat pour les appels OSRM (coords attendus en WGS84)
  DA_centroids_ll <- sf::st_transform(DA_centroids_m, 4326)

  # Stops en sf
  all_stops_sf_ll <- sf::st_as_sf(
    tibble::as_tibble(gtfs$stops),
    coords = c("stop_lon", "stop_lat"),
    crs = 4326
  )[, c("stop_id", "stop_name")]

  # Pour l’intersection, travailler en métrique
  all_stops_sf_m <- sf::st_transform(all_stops_sf_ll, sf::st_crs(DA_buffer_m))

  DA_ids <- DA_table$ID

  progressr::with_progress({
    pb <- progressr::progressor(steps = length(DA_ids))

    DA_stops <- future.apply::future_sapply(
      DA_ids,
      function(ID) {
        this_buf <- DA_buffer_m[DA_buffer_m$ID == ID, ]
        idx <- sf::st_intersects(this_buf, all_stops_sf_m, sparse = TRUE)[[1]]
        pb()
        if (length(idx) == 0L) {
          return(NULL)
        }

        list(
          DA = DA_centroids_ll[DA_centroids_ll$ID == ID, ], # lon/lat pour OSRM
          stops = all_stops_sf_ll[idx, c("stop_id")] # garde stop_id + geom lon/lat
        )
      },
      simplify = FALSE,
      USE.NAMES = TRUE,
      future.seed = NULL
    )

    DA_stops <- DA_stops[!vapply(DA_stops, is.null, logical(1))]

    # Garder seulement les DAs où au moins un stop d’origine existe dans 'traveltimes'
    DA_stops <- DA_stops[vapply(
      DA_stops,
      function(origin) {
        any(names(traveltimes) %in% origin$stops$stop_id)
      },
      logical(1)
    )]

    # Temps de marche DA -> stops (OSRM "lon,lat;lon,lat")
    DA_stops_walk <- future.apply::future_lapply(
      DA_stops,
      function(df) {
        da_xy <- tibble::as_tibble(sf::st_coordinates(df$DA))
        stop_xy <- tibble::as_tibble(sf::st_coordinates(df$stops))
        df$stops$coords <- paste0(stop_xy$X, ",", stop_xy$Y)
        dest <- paste0(da_xy$X, ",", da_xy$Y)

        walk <- vapply(
          df$stops$coords,
          function(x) {
            coords <- paste0(x, ";", dest)
            res <- httr::GET(paste0(routing_server, "route/v1/foot/", coords))
            cont <- httr::content(res)
            cont$routes[[1]]$duration
          },
          numeric(1)
        )

        tibble::tibble(stop_id = df$stops$stop_id, walking_time = walk)
      },
      future.seed = NULL
    )

    # Répartir la charge
    DA_stops <- sample(DA_stops)

    # Sortie (toujours un objet de listes)
    list(
      DA_table_centroid = DA_centroids_ll, # sf
      DA_stops = DA_stops,
      DA_stops_walk = DA_stops_walk,
      traveltimes = traveltimes
    )
  })
}


#' Get traveltime transit matrix
#'
#' PART 2 OF 2 FOR TRAVEL TIME MATRICE CALCULATIONS. PART 1:
#' \code{\link[cc.data]{gtfs_traveltime_matrix_prep}}
#' The two functions calculate travel times from a set of origin points to a set of
#' destination points using transit. It takes in a GTFS list, a traveltimes list,
#' and a table of origin/destination points (referred to as DA_table) and returns
#' a named list which, for every DA, lists every reachable DA and the travel time.
#' The user can also specify a maximum walk distance (default is 800 meters) and a
#' routing server for walking time from the origin/destination point to the transit
#' stops (default is on local machine using OSRM: "http://localhost:5000/". See
#' \code{\link[cc.data]{tt_local_osrm}}).
#'
#' @param prep_output <`list`> Output of
#' \code{\link[cc.data]{gtfs_traveltime_matrix_prep}}
#'
#' @return A named list containing the travel times from each origin point to
#' each destination point (both are IDs of `DA_table`).
#' @export
gtfs_traveltime_matrix_final <- function(prep_output) {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop(
      'Package "lubridate" must be installed to use this function.',
      call. = FALSE
    )
  }

  # Calcul du temps minimal pour un couple (origine, destination)
  min_travel_time <- function(origin_ID, dest_ID, travels_from_origin, dest) {
    possibilities <- lapply(travels_from_origin, function(x) {
      out <- x[x$stop_id %in% dest, c("stop_id", "duration")]
      if (nrow(out) == 0) {
        return(NULL)
      }
      out
    })
    travels <- possibilities[!vapply(possibilities, is.null, logical(1))]
    if (length(travels) == 0) {
      return(NULL)
    }

    origin_walk_time <- prep_output$DA_stops_walk[[origin_ID]]
    origin_walk_time <- origin_walk_time[
      origin_walk_time$stop_id %in% names(travels),
    ]

    dest_walk_time <- prep_output$DA_stops_walk[[dest_ID]]
    relevant_stops <- unique(unlist(lapply(travels, `[[`, "stop_id")))
    dest_walk_time <- dest_walk_time[
      dest_walk_time$stop_id %in% relevant_stops,
    ]

    travels_min_time <- vapply(
      seq_along(travels),
      function(t) {
        origin_stop <- names(travels)[[t]]
        walk_from_origin <- origin_walk_time$walking_time[
          origin_walk_time$stop_id == origin_stop
        ]
        all_dest_stops <- merge(travels[[t]], dest_walk_time, by = "stop_id")
        names(all_dest_stops)[3] <- "walk_time_to_dest"
        all_dest_stops$transit_plus_dest <-
          as.numeric(lubridate::hms(all_dest_stops$duration)) +
          all_dest_stops$walk_time_to_dest

        as.numeric(min(all_dest_stops$transit_plus_dest)) + walk_from_origin
      },
      numeric(1)
    )

    min(travels_min_time)
  }

  DA_with_stops <- names(prep_output$DA_stops)

  # Centroides en métrique pour le filtre 100km
  cent_m <- sf::st_transform(prep_output$DA_table_centroid, 3347)

  progressr::with_progress({
    pb <- progressr::progressor(steps = length(prep_output$DA_stops))

    out <- future.apply::future_lapply(
      prep_output$DA_stops,
      function(origin) {
        pb()

        origin_ID <- origin$DA$ID
        origin_stop_ids <- origin$stops$stop_id
        travels_from_origin <- prep_output$traveltimes[
          names(prep_output$traveltimes) %in% origin_stop_ids
        ]
        if (length(travels_from_origin) == 0) {
          return(NULL)
        }

        # Heuristique: destinations dans un rayon de 100 km (buffer en mètres)
        origin_buf_m <- sf::st_buffer(sf::st_transform(origin$DA, 3347), 100000)
        idx <- sf::st_intersects(cent_m, origin_buf_m, sparse = TRUE)[[1]]
        all_dests <- prep_output$DA_table_centroid$ID[idx]
        all_dests <- all_dests[all_dests %in% DA_with_stops]

        min_to_dests <- lapply(all_dests, function(dest_ID) {
          dest_index <- names(prep_output$DA_stops) == dest_ID
          if (!any(dest_index)) {
            return(NULL)
          }

          dest <- prep_output$DA_stops[dest_index][[1]]$stops$stop_id
          if (length(dest) == 0) {
            return(NULL)
          }

          min_travel_time(origin_ID, dest_ID, travels_from_origin, dest)
        })
        names(min_to_dests) <- all_dests
        min_to_dests <- min_to_dests[!vapply(min_to_dests, is.null, logical(1))]
        if (length(min_to_dests) == 0) {
          return(NULL)
        }

        tibble::tibble(DA_ID = names(min_to_dests)) |>
          dplyr::mutate(!!origin_ID := unlist(min_to_dests, use.names = FALSE))
      },
      future.seed = NULL
    )

    # Retire les NULL
    out[!vapply(out, is.null, logical(1))]
  })
}
