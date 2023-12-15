#' Create a docker local OSRM instance for faster travel time matrix
#'
#' For further information, read the
#' \href{https://github.com/Project-OSRM/osrm-backend}{Project-OSRM/osrm-backend}
#' documentation
#'
#' @param dest_folder <`character`> Destination folder where to download
#' and process the necessary data for the travel time matrix calculations.
#' @param mode <`character`> Options are `bicycle`, `car` and `foot`. Here
#' are the profiles:
#' \href{https://github.com/Project-OSRM/osrm-backend/tree/master/profiles}{OSRM profiles}
#' @param osm_pbf <`character`> Link to the wanted version of OSM. Defaults to
#' Canada.
#'
#' @return Opens a terminal from which the docker image is created.
#' @export
tt_local_osrm <- function(dest_folder = "test", mode = "car", port = 5001L,
                          osm_pbf = "north-america/canada-latest.osm.pbf") {

  # Error catch
  if (!mode %in% c("bicycle", "car", "foot"))
    stop("Only available modes are `bicycle`, `car` or `foot`.")
  if (!Sys.info()["sysname"] %in% c("Windows", "Darwin"))
    stop("As of now, this function is only adapted for Windows and macOS.")

  # Set destination folder and create if needed
  if (!file.exists(dest_folder)) dir.create(dest_folder)
  dest_folder <- paste(dest_folder, mode, sep = "/")
  if (!file.exists(dest_folder)) dir.create(dest_folder)
  if (!grepl("/$", dest_folder)) dest_folder <- paste0(dest_folder, "/")

  # Download Canada osm pbf if needed
  if (file.exists(paste0(dest_folder, "geofabrik_canada.osm.pbf"))) {
    message("OSM downloaded already detected.")
  } else {
    old_timeout <- options("timeout" = Inf)
    on.exit(options(old_timeout))
    osm_link <- tryCatch(
      suppressWarnings(osmextract::oe_download(
        file_basename = "canada.osm.pbf",
        file_url = paste0("http://download.geofabrik.de/", osm_pbf),
        download_directory = dest_folder,
        quiet = FALSE,
        max_file_size = Inf)),
      error = function(e) {
        file.remove(paste0(dest_folder, "geofabrik_canada.osm.pbf"))
        stop("OSM download failed.", call. = FALSE)
      })
  }

  # Container name
  cont_name <- paste0('osrm_', mode, '_', gsub('/|\\.|-', "_", osm_pbf))
  cont_name <- sub('_osm_pbf$', '', cont_name)
  # shell(paste0('docker rm --force ', cont_name))

  # Check for running docker container
  if (Sys.info()["sysname"] == "Windows") {
    if (length(shell(paste0("docker ps -aq -f name=^", cont_name, "$"),
                     intern = TRUE)) > 0) {
      shell(paste0("docker start ", cont_name))
      return(message("Container already exists. It's been started."))
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    if (length(tryCatch(
      system(paste0("docker ps -aq -f name=^", cont_name, "$"), intern = TRUE),
      warning = function(w) stop("Docker is not running.", call. = FALSE))
      ) > 0) {
      system(paste0("docker start ", cont_name))
      return(message("Container already exists. It's been started."))
    }
  }

  # Create the docker image
  message("Initiating the docker container ...")
  docker_initiated <- FALSE

  # Windows
  if (Sys.info()["sysname"] == "Windows") {
    local_osrm <- paste0(
      'cd ', gsub("/", "\\\\", paste0(getwd(), "/",  dest_folder)), '\n',
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-extract -p /opt/', mode, '.lua /data/geofabrik_canada.osm.pbf',
      '\n',
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-partition /data/geofabrik_canada.osrm', '\n',
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-customize /data/geofabrik_canada.osrm', '\n',
      'docker run -t -i -p 5001:5000 --name ', mode, '_osrm',
      ' -v "${PWD}:/data" ',
      'ghcr.io/project-osrm/osrm-backend osrm-routed --algorithm mld ',
      '/data/geofabrik_canada.osrm', '\n')

    tmp <- tempfile(fileext = ".ps1")
    writeLines(local_osrm, tmp)

    shell(paste0(
      "start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
      gsub("\\\\", "/", paste0(tmp))))

    while (!docker_initiated) {
      z <- shell(paste0("docker ps -aq -f name=^", mode, "_osrm$"),
                 intern = TRUE)
      if (length(z) < 1) Sys.sleep(10) else docker_initiated <- TRUE
    }

  }

  # macOS
  else if (Sys.info()["sysname"] == "Darwin") {

    local_osrm <- paste0(
      'cd ', getwd(), '/',  dest_folder, '\n',
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-extract -p /opt/', mode, '.lua /data/geofabrik_canada.osm.pbf',
      '\n',
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-partition /data/geofabrik_canada.osrm', '\n',
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      'osrm-customize /data/geofabrik_canada.osrm', '\n',
      'docker run -i -p 5001:5000 --name ', cont_name,
      ' -v "$(pwd):/data" ',
      'ghcr.io/project-osrm/osrm-backend osrm-routed --algorithm mld ',
      '/data/geofabrik_canada.osrm', '\n'
    )
    system(local_osrm)

    while (!docker_initiated) {
      z <- system(paste0("docker ps -aq -f name=^", cont_name, "$"),
                  intern = TRUE)
      if (length(z) < 1) Sys.sleep(10) else docker_initiated <- TRUE
    }

  }

  # shell(paste0("docker run ", mode, "_osrm"))

}

#' Create a travel time matrix using Dissemination Areas or Dissemination Blocks
#'
#' @param DA_table_centroids <`sf data.frame`> A \code{DA} or \code{DB} point
#' sf data.frame
#' @param max_dist <`numeric`> The maximum distance used to calculate or not
#' the destination time from a point to another. For a 60 minutes maximum, we
#' can set the `car` default to 120,000 (for 120km/h max), the `bicycle` default
#' to 30,000 (for 30km/h max) and the `foot` default to 10,000 (for 10km/h max).
#' As calculation is very fast, no reason not to let a certain buffer.
#' @param routing_server <`character`> The base URL of the routing server. If
#' a server has been initiated using \code{\link[cc.data]{tt_local_osrm}},
#' the default URL will be `http://localhost:5001/`. Other services can be used
#' like `http://router.project-osrm.org/`.
#'
#' @return The travel time matrix
#' @export
tt_calculate <- function(centroids, max_dist = 120000,
                         routing_server = "http://localhost:5001/") {

  # Error checking
  if (sf::st_crs(centroids)$input != "EPSG:4326")
    centroids <- suppressWarnings(sf::st_transform(centroids, 4326))
  if (!"ID" %in% names(centroids))
    stop("`ID` column must exist in `centroids`")

  # Split the dataframe in smaller dataframes for faster paralleled calculations
  list_centroids <- split(centroids, seq_len(nrow(centroids) / 500)) |>
    suppressWarnings()

  # Get all IDs on which to iterate, and shuffle it
  all_ids <- centroids$ID
  all_ids <- sample(all_ids)

  # Iterate over every DA and merge to create the travel time matrix
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(all_ids))
    out <- future.apply::future_sapply(all_ids, \(id) {

        id_geo <- sf:::`[.sf`(centroids, centroids$ID == id, )
        first_coords <- id_geo |>
          sf::st_coordinates() |>
          tibble::as_tibble()

        first_coords <- paste0(first_coords$X, ",", first_coords$Y)

        it <- lapply(list_centroids, \(df) {

          dist <- nngeo::st_nn(id_geo,
                               df,
                               k = nrow(df),
                               maxdist = max_dist,
                               progress = FALSE) |>
            suppressMessages()
          near_df <- df[unlist(dist), ]

          if (nrow(near_df) == 0) return(NULL)
          near_df <- near_df[near_df$ID != id, ]
          if (nrow(near_df) == 0) return(NULL)

          samp <- sf::st_coordinates(near_df) |>
            tibble::as_tibble()

          coords <- paste0(mapply(paste0, samp$X, ",", samp$Y), collapse = ";")
          coords <- paste0(first_coords, ";", coords)

          time <- httr::GET(paste0(routing_server, "table/v1/mode/",
                                   coords, "?sources=0")) |>
            httr::content()
          time <- unlist(time$durations)

          tryCatch({
            out <- tibble::tibble(DA_ID = near_df$ID)
            out[[id]] <- time[2:length(time)]
            out}, error = function(e) NULL)

        })

        pb()

        it[!sapply(it, is.null)] |>
          data.table::rbindlist(fill = TRUE) |>
          tibble::as_tibble()
    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  })

  # Return
  out <- out[!sapply(out, \(x) nrow(x) == 0)]
  return(out)

}

#' Calculate the travel time matrix of all mode
#'
#' @param dest_folder <`character`> Destination folder where to download
#' and process the necessary data for the travel time matrix calculations.
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame
#' @param routing_server <`character`> The base URL of the routing server. If
#' a server has been initiated using \code{\link[cc.data]{tt_local_osrm}},
#' the default URL will be `http://localhost:5000/`. Other services can be used
#' like `http://router.project-osrm.org/`.
#' @param osm_pbf <`character`> Link to the wanted version of OSM. Defaults to
#' Canada.
#'
#' @return Returns a list of three each holding a data.frame where the first
#' column is the DA ID, and all others are all IDs. Every cell is the trip duration
#' from and to DAs in seconds.
#' @export
tt_calculate_all_modes <- function(dest_folder, DA_table,
                                   routing_server = "http://localhost:5000/",
                                   osm_pbf = "http://download.geofabrik.de/north-america/canada-220101.osm.pbf") {

  # Error checking
  if (!"ID" %in% names(DA_table))
    stop("`ID` column must exist in `DA_table`")

  # Create folder
  if (!file.exists(dest_folder)) dir.create(dest_folder)
  if (!grepl("/$", dest_folder)) dest_folder <- paste0(dest_folder, "/")


  # Calculate centroids -----------------------------------------------------

  # Get centroid
  DA_table <- suppressWarnings(sf::st_transform(DA_table, 3347))
  DA_table <- suppressWarnings(sf::st_centroid(DA_table))
  DA_table <- suppressWarnings(sf::st_transform(DA_table, 4326))


  # Foot mode ---------------------------------------------------------------

  # Create folder
  foot_folder <- paste0(dest_folder, "foot")
  if (!file.exists(foot_folder)) dir.create(foot_folder)
  if (!grepl("/$", foot_folder)) dest_folder <- paste0(foot_folder, "/")
  # Build the docker image for car routing
  tt_local_osrm(dest_folder = foot_folder, mode = "foot", osm_pbf = osm_pbf)
  # Create the travel time matrix
  foot <- tt_calculate(DA_table_centroids = DA_table, max_dist = 10000,
                       routing_server = routing_server)
  shell(paste0('docker rm --force foot_osrm'))


  # Bicycle mode ------------------------------------------------------------

  # Create folder
  bicycle_folder <- paste0(dest_folder, "bicycle")
  if (!file.exists(bicycle_folder)) dir.create(bicycle_folder)
  if (!grepl("/$", bicycle_folder)) dest_folder <- paste0(bicycle_folder, "/")
  # Build the docker image for car routing
  tt_local_osrm(dest_folder = bicycle_folder, mode = "bicycle", osm_pbf = osm_pbf)
  # Create the travel time matrix
  bicycle <- tt_calculate(DA_table_centroids = DA_table, max_dist = 30000,
                          routing_server = routing_server)
  shell(paste0('docker rm --force bicycle_osrm'))


  # Car mode ----------------------------------------------------------------

  # Create folder
  car_folder <- paste0(dest_folder, "car")
  if (!file.exists(car_folder)) dir.create(car_folder)
  if (!grepl("/$", car_folder)) dest_folder <- paste0(car_folder, "/")
  # Build the docker image for car routing
  tt_local_osrm(dest_folder = car_folder, mode = "car", osm_pbf = osm_pbf)
  # Create the travel time matrix
  car <- tt_calculate(DA_table_centroids = DA_table, max_dist = 120000,
                      routing_server = routing_server)
  shell(paste0('docker rm --force car_osrm'))


  # Return all --------------------------------------------------------------

  return(list(car = car, bicycle = bicycle, foot = foot))

}
