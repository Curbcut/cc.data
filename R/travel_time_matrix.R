#' Create a local OSRM instance with Docker
#'
#' @param mode <`character`> One of "car", "bicycle", or "foot".
#' @param port <`integer`> Port for the OSRM HTTP server.
#' @param osm_pbf <`character`> Path to OSM .pbf file.
#' @param dest_folder <`character`> Directory for data.
#' @param threads <`integer`> OSRM routing threads (default: all cores - 1).
#' @param max_table_size <`integer`> Max coordinates per /table request.
#'
#' @return <`character`> (invisibly) the dest_folder.
#' @export
tt_local_osrm <- function(
  mode = "car",
  port = 5001L,
  osm_pbf = "north-america/canada-latest.osm.pbf",
  dest_folder = tempdir(),
  threads = parallel::detectCores() - 4L,
  max_table_size = 10000L
) {
  if (!mode %in% c("bicycle", "car", "foot")) {
    stop("Only available modes are bicycle, car or foot.")
  }
  # if (!Sys.info()["sysname"] %in% c("Windows", "Darwin")) {
  #   stop("As of now, this function is only adapted for Windows and macOS.")
  # }

  dest_folder <- paste0(dest_folder, if (!grepl("/$", dest_folder)) "/", mode)
  dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)

  # Download OSM if needed
  if (file.exists(paste0(dest_folder, "/geofabrik_canada.osm.pbf"))) {
    message("OSM download already detected.")
  } else {
    old_timeout <- options("timeout" = Inf)
    on.exit(options(old_timeout))
    tryCatch(
      suppressWarnings(osmextract::oe_download(
        file_basename = "canada.osm.pbf",
        file_url = paste0("http://download.geofabrik.de/", osm_pbf),
        download_directory = dest_folder,
        quiet = FALSE,
        max_file_size = Inf
      )),
      error = function(e) {
        file.remove(paste0(dest_folder, "/geofabrik_canada.osm.pbf"))
        stop("OSM download failed.", call. = FALSE)
      }
    )
  }

  cont_name <- paste0("osrm_", mode, "_", gsub("/|\\.|-", "_", osm_pbf))
  cont_name <- sub("_osm_pbf$", "", cont_name)

  # Clean up existing container
  existing <- system(
    paste0("docker ps -aq -f name=^", cont_name, "$"),
    intern = TRUE,
    ignore.stderr = TRUE
  )
  if (length(existing) > 0) {
    system(paste0("docker rm --force ", cont_name), ignore.stdout = TRUE)
  }

  # OSRM routing command with threads and table size
  osrm_routed_cmd <- sprintf(
    "osrm-routed --algorithm mld --threads %d --max-table-size %d /data/geofabrik_canada.osrm",
    threads,
    max_table_size
  )

  message(sprintf(
    "Starting OSRM with %d threads, max-table-size %d (can't do URLs that long anyway)...",
    threads,
    max_table_size
  ))

  if (Sys.info()["sysname"] == "Windows") {
    local_osrm <- paste0(
      "cd ",
      dest_folder,
      "\n",
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-extract -p /opt/",
      mode,
      ".lua /data/geofabrik_canada.osm.pbf\n",
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-partition /data/geofabrik_canada.osrm\n",
      'docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-customize /data/geofabrik_canada.osrm\n",
      "docker run -d -p ",
      port,
      ":5000 --name ",
      cont_name,
      ' -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend ',
      osrm_routed_cmd,
      "\n"
    )

    tmp <- tempfile(fileext = ".ps1")
    writeLines(local_osrm, tmp)
    shell(paste0(
      "start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
      gsub("\\\\", "/", tmp)
    ))
  } else {
    local_osrm <- paste0(
      "cd ",
      dest_folder,
      "\n",
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-extract -p /opt/",
      mode,
      ".lua /data/geofabrik_canada.osm.pbf\n",
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-partition /data/geofabrik_canada.osrm\n",
      'docker run -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      "osrm-customize /data/geofabrik_canada.osrm\n",
      "docker run -d -p ",
      port,
      ":5000 --name ",
      cont_name,
      ' -v "$(pwd):/data" ghcr.io/project-osrm/osrm-backend ',
      osrm_routed_cmd,
      "\n"
    )
    system(local_osrm)
  }

  # Wait for container
  docker_ready <- FALSE
  while (!docker_ready) {
    z <- system(
      paste0("docker ps -q -f name=^", cont_name, "$"),
      intern = TRUE,
      ignore.stderr = TRUE
    )
    if (length(z) > 0) docker_ready <- TRUE else Sys.sleep(5)
  }

  message("OSRM container running on port ", port)
  invisible(dest_folder)
}


#' Prepare data for dispatch_model-based request building
#'
#' @param centroids <`sf`> Points with `id` column
#' @param output_dir <`character`> Directory for incremental output (required)
#' @param max_dist <`numeric`> Max distance in meters for destination filtering
#' @param profile <`character`> OSRM routing profile (driving, foot, bicycle)
#' @param routing_server <`character`> OSRM server URL (must end with /)
#' @param max_url_coords <`integer`> Max coordinates per OSRM request
#' @param requests_dir <`character`|`NULL`> Subdirectory for stage 1 request
#'   metadata. If NULL, defaults to `file.path(output_dir, "_requests")`
#'
#' @return List with inputs, worker_args, and metadata for stage 2
#' @export
tt_prepare_dispatch <- function(
  centroids,
  output_dir,
  max_dist = 120000,
  profile = "car",
  routing_server = "http://127.0.0.1:5001/",
  max_url_coords = 200L,
  requests_dir = NULL
) {
  # --- Validation ---
  stopifnot(
    "id" %in% names(centroids),
    grepl("/$", routing_server)
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(requests_dir)) {
    requests_dir <- file.path(output_dir, "_requests")
  }
  dir.create(requests_dir, showWarnings = FALSE, recursive = TRUE)

  # Quick server check
  resp <- httr::GET(
    httr::modify_url(
      routing_server,
      path = sprintf("route/v1/%s/0,0;0,0", profile)
    ),
    httr::timeout(5)
  )
  if (httr::status_code(resp) >= 400) {
    stop("Routing server not responding at ", routing_server, call. = FALSE)
  }

  # --- Transform CRS once ---
  if (!identical(sf::st_crs(centroids)$epsg, 4326L)) {
    centroids <- sf::st_transform(centroids, 4326)
  }

  # --- Build indexed coordinate lookup ---
  coords_matrix <- sf::st_coordinates(centroids)
  coords_dt <- data.table::data.table(
    id = centroids$id,
    x = coords_matrix[, 1],
    y = coords_matrix[, 2]
  )
  data.table::setkey(coords_dt, id)

  # --- Pre-transform for spatial operations ---
  centroids_proj <- sf::st_transform(centroids, 3347)
  all_ids <- centroids$id

  # Pre-extract coordinate vectors for export to workers
  ids_vec <- coords_dt$id
  x_vec <- coords_dt$x
  y_vec <- coords_dt$y
  names(x_vec) <- ids_vec
  names(y_vec) <- ids_vec

  chunk_size_url <- max_url_coords - 1L

  # Prepare inputs (one per origin), skipping already-completed ones
  already_done <- file.exists(
    file.path(requests_dir, sprintf("%s.qs", all_ids))
  )
  n_skip <- sum(already_done)

  inputs <- lapply(sample(seq_along(all_ids)[!already_done]), function(i) {
    list(
      origin_idx = i,
      origin_id = all_ids[i]
    )
  })

  message(sprintf(
    "Prepared %d origins for dispatch (%d already complete, skipping).",
    length(inputs),
    n_skip
  ))

  # Return everything needed
  list(
    inputs = inputs,
    worker_args = list(
      centroids_proj = centroids_proj,
      all_ids = all_ids,
      x_vec = x_vec,
      y_vec = y_vec,
      max_dist = max_dist,
      chunk_size_url = chunk_size_url,
      routing_server = routing_server,
      profile = profile,
      requests_dir = requests_dir
    ),
    # Metadata for stage 2
    requests_dir = requests_dir,
    output_dir = output_dir,
    routing_server = routing_server,
    profile = profile
  )
}

#' Build OSRM request metadata for a single origin
#'
#' Worker function for use with [dispatch_model()] to compute spatial neighbors
#' and construct OSRM API request URLs for one origin point. Results are saved
#' to disk as `{origin_id}.qs` files containing request metadata (URLs,
#' destination IDs, coordinates).
#'
#' This function is called in parallel by [dispatch_model()], with each worker
#' processing one origin independently. The spatial neighbor computation
#' (via `sf::st_is_within_distance`) is done per-origin to avoid holding all
#' neighbor relationships in memory simultaneously.
#'
#' @param input <`list`> Single origin specification with elements:
#'   - `origin_idx`: Integer index position in the centroids dataset
#'   - `origin_id`: Character ID of the origin point
#' @param centroids_proj <`sf`> All centroids transformed to projected CRS
#'   (EPSG:3347) for distance calculations
#' @param all_ids <`character`> Vector of all centroid IDs (same order as
#'   `centroids_proj`)
#' @param x_vec <`numeric`> Named vector of longitudes (WGS84), names are IDs
#' @param y_vec <`numeric`> Named vector of latitudes (WGS84), names are IDs
#' @param max_dist <`numeric`> Maximum distance in meters for neighbor inclusion
#' @param chunk_size_url <`integer`> Maximum number of destinations per OSRM
#'   request URL (typically `max_url_coords - 1` to account for origin)
#' @param routing_server <`character`> OSRM server base URL (must end with `/`)
#' @param profile <`character`> OSRM routing profile (e.g., "car", "bicycle")
#' @param requests_dir <`character`> Directory path for saving request metadata
#'   files
#'
#' @return List with elements:
#'   - `origin_id`: Character ID of the processed origin
#'   - `status`: One of "success", "isolated", or "already_exists"
#'   - `n_requests`: Integer count of OSRM requests generated (NA if already
#'     existed)
#'
#' @details
#' ## Processing logic
#'
#' 1. **Resume detection**: Checks if `{origin_id}.qs` already exists. If yes,
#'    returns immediately without recomputation.
#'
#' 2. **Neighbor computation**: Uses `sf::st_is_within_distance()` to find all
#'    points within `max_dist` meters of this origin (excluding self).
#'
#' 3. **Isolated origins**: Origins with no neighbors get a single "isolated"
#'    request record (no URL, empty dest_ids).
#'
#' 4. **Request chunking**: If an origin has more neighbors than
#'    `chunk_size_url`, destinations are split across multiple OSRM requests.
#'    Each request includes the origin coordinates plus up to `chunk_size_url`
#'    destination coordinates.
#'
#' 5. **URL construction**: Builds OSRM table API URLs with format:
#'    `{server}table/v1/{profile}/{coords}?sources=0&annotations=distance,duration`
#'    where coords are semicolon-separated `lon,lat` pairs (origin first).
#'
#' 6. **Atomic save**: Request metadata saved to disk via `qs::qsave()` for
#'    later batch fetching.
#'
#' ## Output file format
#'
#' Each `{origin_id}.qs` file contains a list of request records. Each record:
#' - `origin_id`: Character ID of origin
#' - `dest_ids`: Character vector of destination IDs in this request
#' - `url`: Character OSRM API URL (or NULL if isolated)
#' - `chunk_index`: Integer chunk number (for multi-chunk origins)
#' - `isolated`: Logical, TRUE only for origins with no neighbors
#'
#' @keywords internal
build_origin_requests <- function(
  input,
  centroids_proj,
  all_ids,
  x_vec,
  y_vec,
  max_dist,
  chunk_size_url,
  routing_server,
  profile,
  requests_dir
) {
  origin_idx <- input$origin_idx
  origin_id <- input$origin_id

  # Check if already done
  output_file <- file.path(requests_dir, sprintf("%s.qs", origin_id))
  if (file.exists(output_file)) {
    return(list(
      origin_id = origin_id,
      status = "already_exists",
      n_requests = NA_integer_
    ))
  }

  # Find neighbors for this single origin
  neighbors_sparse <- sf::st_is_within_distance(
    centroids_proj[origin_idx, ],
    centroids_proj,
    dist = max_dist,
    sparse = TRUE
  )[[1]]

  # Remove self
  dest_idx <- neighbors_sparse[neighbors_sparse != origin_idx]

  # Handle isolated origins
  if (length(dest_idx) == 0) {
    requests <- list(list(
      origin_id = origin_id,
      dest_ids = character(0),
      url = NULL,
      isolated = TRUE
    ))

    qs::qsave(requests, output_file, preset = "fast")

    return(list(
      origin_id = origin_id,
      status = "isolated",
      n_requests = 0L
    ))
  }

  # Build request metadata
  dest_ids <- all_ids[dest_idx]
  origin_x <- x_vec[[origin_id]]
  origin_y <- y_vec[[origin_id]]
  dest_x <- x_vec[dest_ids]
  dest_y <- y_vec[dest_ids]

  # Filter valid coordinates
  valid <- !is.na(dest_x)
  dest_ids <- dest_ids[valid]
  dest_x <- dest_x[valid]
  dest_y <- dest_y[valid]

  n_dests <- length(dest_ids)
  n_chunks <- ceiling(n_dests / chunk_size_url)

  # Build chunked requests
  requests <- lapply(seq_len(n_chunks), function(i) {
    start_idx <- (i - 1L) * chunk_size_url + 1L
    end_idx <- min(i * chunk_size_url, n_dests)
    idx <- start_idx:end_idx

    coord_str <- paste(
      c(
        sprintf("%.6f,%.6f", origin_x, origin_y),
        sprintf("%.6f,%.6f", dest_x[idx], dest_y[idx])
      ),
      collapse = ";"
    )

    list(
      origin_id = origin_id,
      dest_ids = dest_ids[idx],
      url = sprintf(
        "%stable/v1/%s/%s?sources=0&annotations=distance,duration",
        routing_server,
        profile,
        coord_str
      ),
      chunk_index = i,
      isolated = FALSE
    )
  })

  # Save to disk
  qs::qsave(requests, output_file, preset = "fast")

  return(list(
    origin_id = origin_id,
    status = "success",
    n_requests = n_chunks
  ))
}


#' Fetch routes from prepared request metadata
#'
#' Stage 2 of the travel time matrix pipeline. Reads request metadata files
#' produced by [tt_prepare_dispatch()] + [build_origin_requests()], fetches
#' OSRM table API results in batches, and writes output either to local disk or
#' an S3 bucket.
#'
#' Supports resuming interrupted runs: batch assignments are shuffled once on
#' the first call and stored in the manifest so that a resumed run always uses
#' the same batch composition.
#'
#' @param prep <`list`|`NULL`> Output from [tt_prepare_dispatch()]. If provided,
#'   `requests_dir`, `output_dir`, `routing_server`, and `profile` are extracted
#'   from it and their individual arguments are ignored.
#' @param requests_dir <`character`|`NULL`> Directory containing `{id}.qs`
#'   request metadata files produced by [build_origin_requests()]. Ignored if
#'   `prep` is provided.
#' @param output_dir <`character`|`NULL`> Local directory for the resume
#'   manifest (`_manifest.qs`). When `bucket` is `NULL`, batch result files are
#'   also written here. Always required regardless of `bucket`. Ignored if
#'   `prep` is provided.
#' @param routing_server <`character`|`NULL`> OSRM server base URL (must end
#'   with `/`). Ignored if `prep` is provided.
#' @param profile <`character`|`NULL`> OSRM routing profile (e.g. `"car"`,
#'   `"bicycle"`, `"foot"`). Ignored if `prep` is provided.
#' @param n_concurrent <`integer`> Number of simultaneous HTTP connections to
#'   the OSRM server. Defaults to `parallel::detectCores() * 3`.
#' @param flush_every <`integer`> Number of origins to include per batch.
#'   Controls peak memory usage: lower values reduce RAM but increase overhead.
#'   Default `50000L` is a reasonable balance for most workloads.
#' @param bucket <`character`|`NULL`> Name of an S3 bucket to write batch
#'   result files to. If `NULL` (default), batch files are written locally to
#'   `output_dir`. If provided, batch files are serialized in memory and
#'   uploaded via multipart upload — nothing beyond the manifest is written to
#'   disk. Requires `aws.s3` and the `CURBCUT_BUCKET_ACCESS_ID`,
#'   `CURBCUT_BUCKET_ACCESS_KEY`, and `CURBCUT_BUCKET_DEFAULT_REGION`
#'   environment variables to be set.
#'
#' @return `invisible(output_dir)`
#' @export
tt_fetch_routes <- function(
  prep = NULL,
  requests_dir = NULL,
  output_dir = NULL,
  routing_server = NULL,
  profile = NULL,
  n_concurrent = parallel::detectCores() * 3,
  flush_every = 50000L,
  bucket = NULL
) {
  if (!is.null(prep)) {
    requests_dir <- prep$requests_dir
    output_dir <- prep$output_dir
    routing_server <- prep$routing_server
    profile <- prep$profile
  }

  stopifnot(
    !is.null(requests_dir),
    !is.null(output_dir),
    !is.null(routing_server),
    !is.null(profile)
  )

  if (!is.null(bucket)) {
    if (!requireNamespace("aws.s3", quietly = TRUE)) {
      stop(
        "Package \"aws.s3\" must be installed to write to a bucket.",
        call. = FALSE
      )
    }
    if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
      stop("You do not have Curbcut database user access.", call. = FALSE)
    }
  }

  message(sprintf(
    "STAGE 2: Fetching routes in batches (flush_every = %d origins)...",
    flush_every
  ))

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(output_dir, "_manifest.qs")

  all_request_files <- list.files(
    requests_dir,
    pattern = "\\.qs$",
    full.names = TRUE
  )

  if (length(all_request_files) == 0) {
    stop("No request files found in ", requests_dir, call. = FALSE)
  }

  run_fingerprint <- digest::digest(
    list(
      request_files = sort(basename(all_request_files)),
      profile = profile,
      flush_every = flush_every
    ),
    algo = "xxhash64"
  )

  # --- Resume detection and shuffle order restoration ---
  completed_batches <- integer(0)
  request_files <- NULL

  if (file.exists(manifest_path)) {
    prev_manifest <- tryCatch(qs::qread(manifest_path), error = function(e) {
      NULL
    })

    if (
      !is.null(prev_manifest) &&
        isTRUE(attr(prev_manifest, "fingerprint") == run_fingerprint)
    ) {
      request_files <- attr(prev_manifest, "request_files")
      completed_batches <- prev_manifest$batch[prev_manifest$status == "done"]

      message(sprintf(
        "Resuming stage 2: %d / %d batches already completed",
        length(completed_batches),
        nrow(prev_manifest)
      ))
    } else {
      warning(
        "Existing manifest fingerprint does not match. Starting fresh.",
        call. = FALSE
      )
    }
  }

  # First run or fingerprint mismatch: shuffle now
  if (is.null(request_files)) {
    request_files <- sample(all_request_files)
  }

  origin_batches <- split(
    seq_along(request_files),
    ceiling(seq_along(request_files) / flush_every)
  )
  total_batches <- length(origin_batches)
  remaining_idx <- setdiff(seq_len(total_batches), completed_batches)

  if (length(remaining_idx) == 0) {
    message("All batches already completed. Nothing to do.")
    return(invisible(output_dir))
  }

  for (batch_idx in remaining_idx) {
    file_indices <- origin_batches[[batch_idx]]
    batch_files <- request_files[file_indices]

    message(sprintf(
      "  Batch %d/%d: Loading requests from %d origins...",
      batch_idx,
      total_batches,
      length(batch_files)
    ))

    batch_requests <- unlist(lapply(batch_files, qs::qread), recursive = FALSE)

    message(sprintf(
      "  Batch %d/%d: Fetching %d requests...",
      batch_idx,
      total_batches,
      length(batch_requests)
    ))

    batch_results <- fetch_batch_async(
      all_requests = batch_requests,
      routing_server = routing_server,
      profile = profile,
      n_concurrent = n_concurrent
    )

    batch_filename <- sprintf("batch_%04d.qs", batch_idx)

    if (!is.null(bucket)) {
      # Serialize in memory — no local write for batch files
      raw_data <- qs::qserialize(batch_results, preset = "fast")
      aws.s3::put_object(
        what = raw_data,
        object = batch_filename,
        bucket = bucket,
        multipart = TRUE,
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY")
      ) |>
        suppressMessages()
      rm(raw_data)
    } else {
      # Atomic local write
      final_path <- file.path(output_dir, batch_filename)
      tmp_path <- paste0(final_path, ".tmp")
      qs::qsave(batch_results, tmp_path, preset = "fast")
      file.rename(tmp_path, final_path)
    }

    # Update manifest (always local — small, needed for resume)
    completed_batches <- c(completed_batches, batch_idx)
    manifest <- data.table::data.table(
      batch = seq_len(total_batches),
      file = sprintf("batch_%04d.qs", seq_len(total_batches)),
      n_origins = lengths(origin_batches),
      status = ifelse(
        seq_len(total_batches) %in% completed_batches,
        "done",
        "pending"
      )
    )
    attr(manifest, "fingerprint") <- run_fingerprint
    attr(manifest, "request_files") <- request_files

    manifest_tmp <- paste0(manifest_path, ".tmp")
    qs::qsave(manifest, manifest_tmp)
    file.rename(manifest_tmp, manifest_path)

    message(sprintf(
      "  Batch %d/%d written (%d requests) -> %s",
      batch_idx,
      total_batches,
      length(batch_requests),
      if (!is.null(bucket)) {
        paste0("s3://", bucket, "/", batch_filename)
      } else {
        batch_filename
      }
    ))

    rm(batch_requests, batch_results)
    gc()
  }

  message(sprintf(
    "Done. %d batches written to %s",
    total_batches,
    if (!is.null(bucket)) paste0("s3://", bucket) else output_dir
  ))

  return(invisible(output_dir))
}

#' Check if OSRM routing server is responsive
#'
#' @param routing_server OSRM server URL (e.g., "http://127.0.0.1:5001/")
#' @param profile Routing profile (bicycle, driving, foot)
#' @param timeout_sec Timeout in seconds for health check
#' @return TRUE if responsive, FALSE otherwise
check_osrm_responsive <- function(routing_server, profile, timeout_sec = 5) {
  test_url <- httr::modify_url(
    routing_server,
    path = sprintf("route/v1/%s/-73.9,45.5;-73.8,45.4", profile)
  )

  resp <- tryCatch(
    httr::GET(test_url, httr::timeout(timeout_sec)),
    error = function(e) NULL
  )

  !is.null(resp) && httr::status_code(resp) == 200
}

#' Restart OSRM Docker container based on profile
#'
#' @param profile Routing profile (bicycle, driving, foot)
#' @return TRUE if successfully restarted, stops with error otherwise
restart_osrm_container <- function(profile) {
  container_name <- sprintf("osrm_%s_north_america_canada_latest", profile)

  message(sprintf("⟳ Restarting OSRM container: %s", container_name))

  restart_output <- system2(
    "docker",
    c("restart", container_name),
    stdout = TRUE,
    stderr = TRUE
  )

  message("Waiting for OSRM to initialize...")
  Sys.sleep(10)

  # Verify it's actually back up
  for (i in 1:30) {
    if (check_osrm_responsive(routing_server, profile, timeout_sec = 5)) {
      message(sprintf("✓ OSRM is back online after %d seconds", 10 + i))
      return(TRUE)
    }
    Sys.sleep(1)
  }

  stop("OSRM failed to respond after restart", call. = FALSE)
}


#' Fetch travel times for multiple origins using async HTTP
#'
#' @param origin_ids <`character`> Vector of origin IDs to process
#' @param coords_dt <`data.table`> Keyed by id, with x/y columns
#' @param neighbor_lookup <`list`> Named list: origin_id -> dest_ids vector
#' @param routing_server <`character`> OSRM base URL
#' @param profile <`character`> OSRM profile
#' @param n_concurrent <`integer`> Max simultaneous connections
#' @param max_url_coords <`integer`> Max coords per request
#'
#' @return Named list of data.tables (origin_id -> results)
#' @keywords internal
fetch_batch_async <- function(
  all_requests,
  routing_server,
  profile,
  n_concurrent,
  max_url_coords,
  n_daemons
) {
  # --- Handle isolated origins ---
  results <- list()

  for (meta in Filter(function(m) m$isolated, all_requests)) {
    results[[meta$origin_id]] <- data.table::data.table(
      id = meta$origin_id,
      time = 0,
      distance = 0
    )
  }

  to_fetch <- Filter(function(m) !m$isolated, all_requests)
  rm(all_requests) # Free the full list

  if (length(to_fetch) == 0) {
    return(results)
  }

  # --- Async GET requests with retry ---
  pool <- curl::new_pool(total_con = n_concurrent, host_con = n_concurrent)
  responses <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$completed <- 0L
  total_requests <- length(to_fetch)
  max_retries <- 3L

  queue_request <- function(meta, pool) {
    request_key <- paste0(meta$origin_id, "_chunk", meta$chunk_index)
    h <- curl::new_handle()
    curl::handle_setopt(h, connecttimeout = 999999L, timeout = 0)

    curl::curl_fetch_multi(
      url = meta$url,
      handle = h,
      done = local({
        key <- request_key
        function(resp) {
          responses[[key]] <- resp
          state$completed <- state$completed + 1L
        }
      }),
      fail = local({
        key <- request_key
        function(msg) {
          responses[[key]] <- list(status_code = 0, error = msg)
          state$completed <- state$completed + 1L
        }
      }),
      pool = pool
    )
  }

  message(sprintf(
    "Fetching %d requests...", # <-- Remove reference to origin_ids
    total_requests
  ))

  max_queued <- n_concurrent * 4L

  progressr::with_progress({
    pb <- progressr::progressor(steps = total_requests)
    reported <- 0L
    queued <- 0L

    repeat {
      pending_in_queue <- queued - state$completed
      while (pending_in_queue < max_queued && queued < total_requests) {
        queued <- queued + 1L
        queue_request(to_fetch[[queued]], pool)
        pending_in_queue <- pending_in_queue + 1L
      }

      curl::multi_run(pool = pool, poll = TRUE, timeout = 1)

      if (state$completed > reported) {
        pb(amount = state$completed - reported)
        reported <- state$completed
      }

      if (state$completed >= total_requests) break
    }
  })

  # --- Retry failed requests ---
  for (attempt in seq_len(max_retries)) {
    failed_metas <- Filter(
      function(meta) {
        key <- paste0(meta$origin_id, "_chunk", meta$chunk_index)
        resp <- responses[[key]]
        is.null(resp) || resp$status_code != 200
      },
      to_fetch
    )

    if (length(failed_metas) == 0) {
      break
    }

    message(sprintf(
      "Retry %d/%d: %d failed requests...",
      attempt,
      max_retries,
      length(failed_metas)
    ))

    # Check if OSRM is actually responsive before retrying
    # Use a SHORT timeout here - separate from your main requests
    if (!check_osrm_responsive(routing_server, profile, timeout_sec = 5)) {
      message("OSRM not responding to health check (5s timeout)")

      # Only restart on first retry to avoid restart loops
      if (attempt == 1) {
        restart_osrm_container(profile)
      } else {
        warning("OSRM still unresponsive, skipping retry")
        break
      }
    } else {
      message("OSRM health check passed")
    }

    # Brief pause to let OSRM recover if it was under pressure
    Sys.sleep(2)

    # Reset state for retry batch
    state$completed <- 0L
    retry_pool <- curl::new_pool(
      total_con = n_concurrent,
      host_con = n_concurrent
    )

    for (meta in failed_metas) {
      queue_request(meta, retry_pool)
    }

    curl::multi_run(pool = retry_pool)
  }

  # --- Log remaining failures ---
  failed_keys <- Filter(
    function(k) responses[[k]]$status_code != 200,
    ls(responses)
  )
  if (length(failed_keys) > 0) {
    warning(sprintf(
      "Failed requests after %d retries: %d / %d",
      max_retries,
      length(failed_keys),
      total_requests
    ))
    for (key in head(failed_keys, 3)) {
      resp <- responses[[key]]
      cat(key, ": status=", resp$status_code %||% "NA", "\n")
      if (!is.null(resp$error)) cat("  error:", resp$error, "\n")
    }
  }
  # --- Parse responses and group by origin ---
  origin_chunks <- list()

  for (meta in to_fetch) {
    request_key <- paste0(meta$origin_id, "_chunk", meta$chunk_index)
    resp <- responses[[request_key]]

    if (is.null(resp) || resp$status_code != 200) {
      next
    }

    content <- tryCatch(
      RcppSimdJson::fparse(rawToChar(resp$content)),
      error = function(e) NULL
    )

    if (is.null(content) || content$code != "Ok") {
      next
    }

    dt <- data.table::data.table(
      id = c(meta$origin_id, meta$dest_ids),
      time = as.numeric(content$durations[1, ]),
      distance = as.numeric(content$distances[1, ])
    )

    oid <- meta$origin_id
    if (is.null(origin_chunks[[oid]])) {
      origin_chunks[[oid]] <- list(dt)
    } else {
      origin_chunks[[oid]] <- c(origin_chunks[[oid]], list(dt))
    }
  }

  # --- Merge chunks per origin ---
  # Ff memory becomes a concern at scale, let's write
  # completed batches to disk (e.g. fst/qs) and clear responses
  # between batches instead of accumulating everything in memory
  for (oid in names(origin_chunks)) {
    merged <- data.table::rbindlist(origin_chunks[[oid]])
    results[[oid]] <- unique(merged, by = "id")
  }

  results
}
