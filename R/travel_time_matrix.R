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
#' @param canada_osm_pbf <`character`> Link to the wanted version of Canada's OSM
#' file.
#'
#' @return Opens a terminal from which the docker image is created.
#' @export
tt_local_osrm <- function(dest_folder, mode = "car",
                          canada_osm_pbf = "http://download.geofabrik.de/north-america/canada-220101.osm.pbf") {

  # Error catch
  if (!mode %in% c("bicycle", "car", "foot"))
    stop("Only available modes are `bicycle`, `car` or `foot`.")
  if (Sys.info()["sysname"] != "Windows")
    stop("As of now, this function is only adapted for Windows.")

  # Create folder
  if (!file.exists(dest_folder)) dir.create(dest_folder)
  if (!grepl("/$", dest_folder)) dest_folder <- paste0(dest_folder, "/")

  if (length(shell(paste0("docker ps -aq -f name=^", mode, "_osrm$"),
                   intern = TRUE)) > 0) {
    shell(paste0("docker start ", mode, "_osrm"))
    return(message("Container already exists. It's been started."))
  }

  # Download Canada osm pbf
  osm_link <- osmextract::oe_download(
    file_basename = "canada.osm.pbf",
    file_url = canada_osm_pbf,
    download_directory = dest_folder,
    quiet = FALSE,
    max_file_size = Inf)

  # Container name
  cont_name <- paste0(mode, '_osrm')
  # shell(paste0('docker rm --force ', cont_name))

  # Create the docker image
  local_osrm <-
    paste0('cd ', gsub("/", "\\\\", paste0(getwd(), "/",  dest_folder)), '\n',
           'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
           'osrm-extract -p /opt/', mode, '.lua /data/geofabrik_canada.osm.pbf', '\n',
           'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
           'osrm-partition /data/geofabrik_canada.osrm', '\n',
           'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
           'osrm-customize /data/geofabrik_canada.osrm', '\n',
           'docker run -t -i -p 5000:5000 --name ', mode, '_osrm',
           ' -v "${PWD}:/data" ',
           'ghcr.io/project-osrm/osrm-backend osrm-routed --algorithm mld ',
           '/data/geofabrik_canada.osrm', '\n')
  tmp <- tempfile(fileext = ".ps1")
  writeLines(local_osrm, tmp)

  shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
               gsub("\\\\", "/", paste0(tmp))))

  message("Initiating the docker container ...")
  docker_initiated <- FALSE
  while (!docker_initiated) {
    z <- shell(paste0("docker ps -aq -f name=^", mode, "_osrm$"), intern = TRUE)
    if (length(z) < 1) Sys.sleep(10) else docker_initiated <- TRUE
  }

  shell(paste0("docker run ", mode, "_osrm"))

}

#' Create a travel time matrix using Disseination Blocks
#'
#' @param DA_table_centroids <`sf data.frame`> A \code{DA} point sf data.frame
#' @param max_dist <`numeric`> The maximum distance used to calculate or not
#' the destination time from a point to another. For a 60 minutes maximum, we
#' can set the `car` default to 120,000 (for 120km/h max), the `bicycle` default
#' to 30,000 (for 30km/h max) and the `foot` default to 10,000 (for 10km/h max).
#' As calculation is very fast, no reason not to let a certain buffer.
#' @param routing_server <`character`> The base URL of the routing server. If
#' a server has been initiated using \code{\link[cc.data]{tt_local_osrm}},
#' the default URL will be `http://localhost:5000/`. Other services can be used
#' like `http://router.project-osrm.org/`.
#'
#' @return The travel time matrix
#' @export
tt_calculate <- function(DA_table_centroids, max_dist = 120000,
                         routing_server = "http://localhost:5000/") {

  # Error checking
  if (sf::st_crs(DA_table)$input != "EPSG:4326")
    DA_table <- suppressWarnings(sf::st_transform(DA_table, 4326))
  if (!"ID" %in% names(DA_table))
    stop("`ID` column must exist in `DA_table`")

  # Split the dataframe in smaller dataframes for faster paralleled calculations
  list_DA_centroids <- split(DA_table_centroids,
                        seq_len(nrow(DA_table_centroids)/500)) |>
    suppressWarnings()

  # Get all IDs on which to iterate
  all_ids <- DA_table_centroids$ID

  # Iterate over every DA and merge to create the travel time matrix
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(all_ids))
    out <- sapply(all_ids, \(id) {

        id_geo <- DA_table_centroids[DA_table_centroids$ID == id, ]
        first_coords <- DA_table_centroids[DA_table_centroids$ID == id, ] |>
          sf::st_coordinates() |>
          tibble::as_tibble()

        first_coords <- paste0(first_coords$X, ",", first_coords$Y)

        it <- future.apply::future_lapply(list_DA_centroids, \(df) {

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

          time <- httr::GET(paste0(routing_server, "table/v1/driving/",
                                   coords, "?sources=0")) |>
            httr::content()
          time <- unlist(time$durations)

          tryCatch({
            out <- tibble::tibble(DA_ID = near_df$ID)
            out[[id]] <- time[2:length(time)]
            out}, error = function(e) NULL)

        }, future.seed = NULL)

        pb()

        it[!sapply(it, is.null)] |>
          data.table::rbindlist(fill = TRUE) |>
          tibble::as_tibble()
    }, simplify = FALSE, USE.NAMES = TRUE)
  })

  # Return
  out <- out[!sapply(out, \(x) nrow(x) == 0)]
  out
  # merge_ <- function(x, y) merge(x, y, all = TRUE, by = "DA_ID")
  # Reduce(merge_, z)

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
#'
#' @return Returns a list of three each holding a data.frame where the first
#' column is the DA ID, and all others are all IDs. Every cell is the trip duration
#' from and to DAs in seconds.
#' @export
tt_calculate_all_modes <- function(dest_folder, DA_table,
                                   routing_server = "http://localhost:5000/") {

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
  tt_local_osrm(dest_folder = foot_folder, mode = "foot")
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
  tt_local_osrm(dest_folder = bicycle_folder, mode = "bicycle")
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
  tt_local_osrm(dest_folder = car_folder, mode = "car")
  # Create the travel time matrix
  car <- tt_calculate(DA_table_centroids = DA_table, max_dist = 120000,
                      routing_server = routing_server)
  shell(paste0('docker rm --force car_osrm'))


  # Return all --------------------------------------------------------------

  return(list(car = car, bicycle = bicycle, foot = foot))

}
