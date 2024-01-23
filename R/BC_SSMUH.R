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


#' Retrieve Urban Containment Boundaries (UCB) for British Columbia Regions
#'
#' This function downloads the Urban Containment Boundaries (UCB) for various
#' regions in the province of British Columbia.
#'
#' @param crs <`character`> To re-project the datasets. Defaults to 32610.
#'
#' @return A list containing UCB data for the Capital Region District (`crd`),
#' Metro Vancouver (`metro_van`), and Nanaimo (`nanaimo`).
#' @export
BC_UCB <- function(crs = 32610) {

  # THERE ARE 10 ADOPTED REGIONAL GROWTH STRATEGY IN THE PROVINCE
  # https://www2.gov.bc.ca/gov/content/governments/local-governments/planning-land-use/local-government-planning/regional-growth-strategies/status-of-regional-growth-strategies


  # For the Capital Region District, download the UCB directly from their
  # ArcGIS REST Services Directory
  crd <- arcgis_rest_services_ret(paste0("https://mapservices.crd.bc.ca/arcgis",
                                         "/rest/services/Root/RegionalGrowthSt",
                                         "rategy/MapServer/1"))
  crd <- sf::st_transform(crd, crs)

  # Metro Vancouver
  metro_van <- bucket_read_object_zip_shp("Metro_2050_Urban_Containment_Boundary.zip",
                                          "curbcut.bc.zoning")
  metro_van <- sf::st_transform(metro_van, crs)

  # Nanaimo
  nanaimo <- bucket_read_object_zip_shp("RGS_ContainmentBoundaries_97.zip",
                                        "curbcut.bc.zoning")
  nanaimo <- sf::st_transform(nanaimo, crs)


  merged <- Reduce(rbind, list(crd["geometry"],
                     metro_van["geometry"],
                     nanaimo["geometry"]))
  merged <- sf::st_cast(merged, "MULTIPOLYGON")

  return(merged)
}

# https://www2.gov.bc.ca/gov/content/housing-tenancy/local-governments-and-housing/housing-initiatives/transit-oriented-development-areas
BC_TOA <- function() {
  page <- rvest::read_html("https://www2.gov.bc.ca/gov/content/housing-tenancy/local-governments-and-housing/housing-initiatives/transit-oriented-development-areas")

  # Extract the data from each panel
  panels <- page |>
    rvest::html_nodes('.accordion-container .panel')

  # Initialize an empty data frame to store results
  all_data <- data.frame(PanelTitle = character(),
                         ListItem = character(),
                         stringsAsFactors = FALSE)

  # Loop through each panel and extract the title and list items
  for (panel in panels) {
    panel_title <- panel |>
      rvest::html_node('.panel-title') |>
      rvest::html_text(trim = TRUE)

    list_items <- panel |>
      rvest::html_node('.panel-text ul') |>
      rvest::html_children() |>
      rvest::html_text()

    pdf_links <- panel |>
      rvest::html_node('.panel-text ul') |>
      rvest::html_children() |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    pdf_links <- paste0("https://www2.gov.bc.ca", pdf_links)

    # Create a data frame for the current panel
    panel_data <- data.frame(PanelTitle = rep(panel_title, length(list_items)),
                             ListItem = list_items,
                             PDFLink = pdf_links,
                             stringsAsFactors = FALSE)

    # Combine the current panel data with the overall data
    all_data <- rbind(all_data, panel_data)
  }

  all_data <- tibble::as_tibble(all_data)

  # Geocode it
  all_data$ListItem <- gsub(" \\(overlaps .*\\)", "", all_data$ListItem)
  all_data$ListItem <- gsub(" \\(PDF, .*\\)", "", all_data$ListItem)

  all_data$geometry <- lapply(seq_len(nrow(all_data)), \(x) {
    # Special cases
    station <- all_data$ListItem[x]
    if (station == "Downtown Chilliwack Exchange") {
      return(sf::st_point(c(-121.955454, 49.168541)))
    }
    if (station == "Uptown Exchange") {
      return(sf::st_point(c(-123.37743333685627, 48.45609759473162)))
    }
    if (station == "152nd Street Station") {
      return(sf::st_point(c(-122.80278265654255, 49.16675082882255)))
    }
    if (station == "160th Street Station") {
      return(sf::st_point(c(-122.77771379318246, 49.15677999236711)))
    }
    if (station == "166th Street Station") {
      return(sf::st_point(c(-122.76314300848576, 49.15130913239408)))
    }
    if (station == "184th Street Station") {
      return(sf::st_point(c(-122.71328587020672, 49.12912287356195)))
    }
    if (station == "190th Street Station") {
      return(sf::st_point(c(-122.69676368270977, 49.12157508680435)))
    }
    if (station == "Stadium – Chinatown Station") {
      return(sf::st_point(c(-123.10979267631564, 49.27963750860846)))
    }

    toa <- sprintf("%s, %s", station, all_data$PanelTitle[x])
    geometry <- geocode_localhost(toa)

    if (sf::st_is_empty(geometry)) {
      toa <- sprintf("%s, British Columbia", station)
      geometry <- geocode_localhost(toa)
    }

    return(geometry)
  })
  all_data <- sf::st_as_sf(all_data, crs = 4326)

  # Configure Tesseract to use English language and assume it's numerics we
  # are extracting
  eng <- tesseract::tesseract("eng", options = list(classify_bln_numeric_mode = TRUE,
                                                    tessedit_char_whitelist = "0123456789"))

  # Grab the maximum meter Tier (buffer) making the TOA area
  all_data$buffer <- sapply(all_data$PDFLink, \(pdf_path) {
    file <- tempfile(fileext = ".png")
    img <- pdftools::pdf_convert(pdf_path, dpi = 600, pages = 1, filenames = file)

    # Use Magick to enhance the image
    tmp_img <- magick::image_read(img) |>
      magick::image_crop(geometry = "450x1900+900+2500") |>
      magick::image_write(format = 'png', path = tempfile(fileext = ".png"))

    # Perform OCR on the enhanced image
    text <- tesseract::ocr(image = tmp_img, engine = eng)

    # Grab the last set of numerics
    text <- stringr::str_extract_all(text, "\\d{3}", simplify = TRUE)
    as.numeric(text[length(text)])
  }, USE.NAMES = FALSE)

  # Remove duplicate (overlaps from a city to another)
  duplicates <- duplicated(all_data[c("ListItem", "geometry", "buffer")])
  all_data <- all_data[!duplicates, ]

  # Make the buffer
  all_data <- sf::st_buffer(all_data, all_data$buffer)

  # Send to AWS
  all_data <- sf::st_transform(all_data, 3347)
  db_write_table(all_data, tb_name = "BC_TOA")

}
