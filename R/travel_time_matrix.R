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
  if (!Sys.info()["sysname"] %in% c("Windows", "Darwin")) {
    stop("As of now, this function is only adapted for Windows and macOS.")
  }

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
    "Starting OSRM with %d threads, max-table-size %d...",
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

#' Create a travel time matrix (batched + async)
#'
#' @param centroids <`sf`> Points with `id` column
#' @param max_dist <`numeric`> Max distance in meters for destination filtering
#' @param routing_server <`character`> OSRM server URL (must end with /)
#' @param profile <`character`> OSRM routing profile (driving, foot, bicycle)
#' @param n_concurrent <`integer`> Simultaneous HTTP requests. Optimal value is
#'   roughly 3x the number of threads given to the OSRM container (e.g., 200 for
#'   a 64-thread server). Beyond ~3x, throughput plateaus as OSRM becomes
#'   CPU-bound. Too high may cause connection timeouts.
#' @param max_url_coords <`integer`> Max coordinates per OSRM request. Controls
#'   URL length and matrix computation size. Testing showed:
#'   - 200: 100% success, ~16k rows/sec on 28 threads (recommended default)
#'   - 500: 100% success, similar throughput (larger matrices offset fewer requests)
#'   - 100: 100% success but ~40% slower due to HTTP overhead
#'   Higher values reduce HTTP overhead but increase per-request OSRM computation
#'   time (matrix calc is O(n²)). Values above 500 may timeout on slower servers.
#' @param n_batches <`integer`> Number of spatial batches for neighbor indexing.
#' @param flush_every <`integer`> Number of origins to process per flush cycle.
#'   Controls peak memory: each cycle fetches, parses, writes to disk, then frees
#'   memory. Lower values = less RAM but more disk I/O overhead. Default 50000 is
#'   a good balance for most workloads.
#' @param output_dir <`character`|`NULL`> Directory for incremental output. If
#'   provided, results are flushed to disk in batches as `.qs` files and the
#'   function returns `invisible(output_dir)`. If `NULL` (default), all results
#'   are held in memory and returned as a named list of `data.table`s. For large
#'   jobs (>50k origins), always set `output_dir` to avoid OOM.
#'
#' @return Named list of data.tables, or invisible(output_dir) if writing to disk
#' @export
#'
#' @details
#' ## Performance tuning
#'
#' The bottleneck is typically the OSRM server, not R. Key findings from benchmarking:
#'
#' - **n_concurrent**: Throughput plateaus at ~3x OSRM threads. For a 28-thread
#'   OSRM container, 100 concurrent requests saturates the server. Going higher
#'   just queues requests without speed gain.
#'
#' - **max_url_coords**: Trade-off between HTTP overhead and matrix computation.
#'   200 coords/request is the sweet spot — small enough for fast OSRM response,
#'   large enough to minimize request overhead. Requests are automatically chunked
#'   if an origin has more neighbors than this limit.
#'
#' ## Memory management
#'
#' With `output_dir` set, memory usage is bounded by `flush_every` origins at a
#' time. Each flush cycle: fetch → parse → write `.qs` → free. This prevents the
#' OOM kills that occur when millions of origin-destination pairs accumulate in
#' the `responses` environment and result lists simultaneously.
tt_calculate <- function(
  centroids,
  max_dist = 120000,
  routing_server = "http://127.0.0.1:5001/",
  profile = "car",
  n_concurrent = parallel::detectCores() * 3,
  n_batches = 100,
  max_url_coords = 200L,
  flush_every = 50000L,
  output_dir = NULL
) {
  # --- Validation ---
  stopifnot(
    "id" %in% names(centroids),
    grepl("/$", routing_server)
  )

  write_to_disk <- !is.null(output_dir)

  if (write_to_disk) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Clean up any partially-written temp files from a prior crash
    stale_temps <- list.files(
      output_dir,
      pattern = "\\.qs\\.tmp$",
      full.names = TRUE
    )
    if (length(stale_temps) > 0) {
      file.remove(stale_temps)
      message(sprintf(
        "Cleaned up %d partial temp files from prior run.",
        length(stale_temps)
      ))
    }
  }

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

  # --- Pre-compute spatial neighbor index ONCE ---
  message("Building spatial neighbor index...")

  centroids_proj <- sf::st_transform(centroids, 3347)

  chunk_size <- nrow(centroids_proj) / n_batches
  chunks <- split(
    seq_len(nrow(centroids_proj)),
    ceiling(seq_len(nrow(centroids_proj)) / chunk_size)
  )

  # Method 1: Use mirai_map's built-in progress
  neighbor_sparse_raw <- mirai::mirai_map(
    chunks,
    function(idx) {
      sf::st_is_within_distance(
        centroids_proj[idx, ],
        centroids_proj,
        dist = max_dist,
        sparse = TRUE
      )
    },
    centroids_proj = centroids_proj,
    max_dist = max_dist
  )

  # This shows progress and blocks until complete
  neighbor_sparse_raw[.progress]

  neighbor_sparse <- unlist(neighbor_sparse_raw[], recursive = FALSE)
  Reduce(c, neighbor_sparse_raw[])

  # Lookup code
  all_ids <- centroids$id
  neighbor_lookup <- setNames(
    lapply(seq_along(neighbor_sparse), function(i) {
      dest_idx <- neighbor_sparse[[i]]
      dest_idx <- dest_idx[dest_idx != i]
      all_ids[dest_idx]
    }),
    all_ids
  )

  # Free the large sf objects now that we have the lookup
  rm(centroids_proj, neighbor_sparse_raw, neighbor_sparse)

  # --- Summary stats ---
  total_pairs <- sum(lengths(neighbor_lookup)) + length(all_ids)
  est_seconds <- total_pairs / 15000
  est_time <- if (est_seconds < 60) {
    sprintf("%.0f seconds", est_seconds)
  } else if (est_seconds < 3600) {
    sprintf("%.1f minutes", est_seconds / 60)
  } else {
    sprintf("%.1f hours", est_seconds / 3600)
  }

  message(sprintf(
    "Index built. Origins: %s | Median neighbors: %d | Max: %d | Total pairs: %s | Est. time: %s",
    format(length(all_ids), big.mark = ","),
    median(lengths(neighbor_lookup)),
    max(lengths(neighbor_lookup)),
    format(total_pairs, big.mark = ","),
    est_time
  ))

  # --- Main loop: batched fetch with periodic flush ---
  if (write_to_disk) {
    # Process in flush_every-sized chunks, write each to disk, free memory
    origin_batches <- split(
      all_ids,
      ceiling(seq_along(all_ids) / flush_every)
    )
    total_batches <- length(origin_batches)

    # Build a fingerprint so we can detect if parameters changed between runs.
    # Digest the origin IDs (order-sensitive) + key routing params. If any of
    # these change, the old batches are invalid and we must start fresh.
    run_fingerprint <- digest::digest(
      list(
        ids = all_ids,
        max_dist = max_dist,
        profile = profile,
        flush_every = flush_every
      ),
      algo = "xxhash64"
    )

    # --- Resume detection ---
    manifest_path <- file.path(output_dir, "_manifest.qs")
    completed_batches <- integer(0)

    if (file.exists(manifest_path)) {
      prev_manifest <- tryCatch(qs::qread(manifest_path), error = function(e) {
        NULL
      })

      if (
        !is.null(prev_manifest) &&
          "fingerprint" %in% names(attributes(prev_manifest)) &&
          attr(prev_manifest, "fingerprint") == run_fingerprint
      ) {
        # Fingerprint matches — safe to resume
        completed_batches <- prev_manifest$batch[prev_manifest$status == "done"]

        if (length(completed_batches) > 0) {
          message(sprintf(
            "Resuming: %d / %d batches already completed. Skipping.",
            length(completed_batches),
            total_batches
          ))
        }
      } else {
        # Fingerprint mismatch — parameters changed, start fresh
        warning(
          "Existing manifest fingerprint does not match current parameters. ",
          "Starting fresh (old batch files will be overwritten).",
          call. = FALSE
        )
      }
    }

    remaining_idx <- setdiff(seq_len(total_batches), completed_batches)

    if (length(remaining_idx) == 0) {
      message("All batches already completed. Nothing to do.")
      return(invisible(output_dir))
    }

    message(sprintf(
      "Processing %d origins in %d batches (%d remaining, flush_every = %d)...",
      length(all_ids),
      total_batches,
      length(remaining_idx),
      flush_every
    ))

    for (batch_idx in remaining_idx) {
      batch_ids <- origin_batches[[batch_idx]]

      batch_results <- fetch_batch_async(
        origin_ids = batch_ids,
        coords_dt = coords_dt,
        neighbor_lookup = neighbor_lookup,
        routing_server = routing_server,
        profile = profile,
        n_concurrent = n_concurrent,
        max_url_coords = max_url_coords
      )

      # Atomic write: temp file -> rename. A crash during qsave leaves only
      # the .tmp file, which gets cleaned up on the next run.
      final_path <- file.path(output_dir, sprintf("batch_%04d.qs", batch_idx))
      tmp_path <- paste0(final_path, ".tmp")
      qs::qsave(batch_results, tmp_path, preset = "fast")
      file.rename(tmp_path, final_path)

      # Update manifest incrementally — this is the checkpoint
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
      # Manifest itself uses atomic write
      manifest_tmp <- paste0(manifest_path, ".tmp")
      qs::qsave(manifest, manifest_tmp)
      file.rename(manifest_tmp, manifest_path)

      message(sprintf(
        "  Batch %d/%d written (%d origins) -> %s",
        batch_idx,
        total_batches,
        length(batch_ids),
        basename(final_path)
      ))

      rm(batch_results)
      gc()
    }

    message(sprintf(
      "Done. %d batches written to %s",
      total_batches,
      output_dir
    ))
    return(invisible(output_dir))
  } else {
    # In-memory mode: single pass, same as before
    # WARNING: this will OOM on large jobs. Consider setting output_dir.
    results <- fetch_batch_async(
      origin_ids = all_ids,
      coords_dt = coords_dt,
      neighbor_lookup = neighbor_lookup,
      routing_server = routing_server,
      profile = profile,
      n_concurrent = n_concurrent,
      max_url_coords = max_url_coords
    )
    return(results)
  }
}

#' Read back results written by tt_calculate
#'
#' @param output_dir <`character`> Directory containing batch .qs files
#' @param origins <`character`|`NULL`> Optional subset of origin IDs to load.
#'   If NULL, loads everything (watch your RAM).
#' @return Named list of data.tables
#' @export
tt_read_results <- function(output_dir, origins = NULL) {
  manifest <- qs::qread(file.path(output_dir, "_manifest.qs"))
  results <- list()

  for (f in manifest$file) {
    batch <- qs::qread(file.path(output_dir, f))
    if (!is.null(origins)) {
      batch <- batch[intersect(names(batch), origins)]
    }
    results <- c(results, batch)
  }

  # TODO: Consider returning a single rbindlist'd data.table with an origin_id
  # column instead of a named list, depending on downstream usage patterns.
  results
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
  origin_ids,
  coords_dt,
  neighbor_lookup,
  routing_server,
  profile,
  n_concurrent,
  max_url_coords
) {
  # --- Build request metadata (with chunking for URL length) ---
  message("Building request metadata...")

  chunk_size <- max_url_coords - 1L

  # Pre-extract vectors for fast lookup (outside lapply)
  ids_vec <- coords_dt$id
  x_vec <- coords_dt$x
  y_vec <- coords_dt$y
  names(x_vec) <- ids_vec
  names(y_vec) <- ids_vec

  progressr::with_progress({
    pb <- progressr::progressor(steps = length(origin_ids))

    request_meta <- lapply(origin_ids, function(oid) {
      dest_ids <- neighbor_lookup[[oid]]

      if (length(dest_ids) == 0) {
        pb()
        return(list(list(
          origin_id = oid,
          dest_ids = character(0),
          url = NULL,
          isolated = TRUE
        )))
      }

      # Fast named vector lookup
      origin_x <- x_vec[[oid]]
      origin_y <- y_vec[[oid]]

      dest_x <- x_vec[dest_ids]
      dest_y <- y_vec[dest_ids]

      # Drop NAs (equivalent to nomatch = NULL)
      valid <- !is.na(dest_x)
      dest_ids <- dest_ids[valid]
      dest_x <- dest_x[valid]
      dest_y <- dest_y[valid]

      n_dests <- length(dest_ids)
      n_chunks <- ceiling(n_dests / chunk_size)

      chunks <- lapply(seq_len(n_chunks), function(i) {
        start_idx <- (i - 1L) * chunk_size + 1L
        end_idx <- min(i * chunk_size, n_dests)

        idx <- start_idx:end_idx

        coord_str <- paste(
          c(
            sprintf("%.6f,%.6f", origin_x, origin_y),
            sprintf("%.6f,%.6f", dest_x[idx], dest_y[idx])
          ),
          collapse = ";"
        )

        list(
          origin_id = oid,
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

      pb()
      chunks
    })
  })

  all_requests <- unlist(request_meta, recursive = FALSE)
  rm(request_meta) # Free immediately

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
    "Fetching %d requests (%d origins)...",
    total_requests,
    length(origin_ids)
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
tt_calculate_all_modes <- function(
  dest_folder,
  DA_table,
  routing_server = "http://localhost:5001/",
  osm_pbf = "north-america/canada-220101.osm.pbf"
) {
  # Remove container function
  rem_container <- function(routing_server) {
    port <- gsub("http://localhost:|/", "", routing_server)

    # Locate the container ID of the Docker container using port of `routing_server`
    container_id <- system2(
      "docker",
      args = c("ps", "-q", "-f", paste0("publish=", port)),
      stdout = TRUE
    )

    # Remove the Docker container using the located container ID
    if (nzchar(container_id)) {
      remove_command <- paste0('docker rm --force ', container_id)
      shell(remove_command)
    } else {
      cat("No container found on port 5001\n")
    }
  }

  # Error checking
  if (!"id" %in% names(DA_table)) {
    stop("`ID` column must exist in `DA_table`")
  }

  # Create folder
  if (!file.exists(dest_folder)) {
    dir.create(dest_folder)
  }
  if (!grepl("/$", dest_folder)) {
    dest_folder <- paste0(dest_folder, "/")
  }

  # Calculate centroids -----------------------------------------------------

  # Get centroid
  DA_table <- suppressWarnings(sf::st_transform(DA_table, 3347))
  DA_table <- suppressWarnings(sf::st_centroid(DA_table))
  DA_table <- suppressWarnings(sf::st_transform(DA_table, 4326))

  # Foot mode ---------------------------------------------------------------

  # Build the docker image for car routing
  tt_local_osrm(dest_folder = dest_folder, mode = "foot", osm_pbf = osm_pbf)

  # Create the travel time matrix
  foot <- tt_calculate(
    centroids = DA_table,
    max_dist = 10000,
    routing_server = routing_server
  )
  rem_container(routing_server)

  # Bicycle mode ------------------------------------------------------------

  # Build the docker image for car routing
  tt_local_osrm(dest_folder = dest_folder, mode = "bicycle", osm_pbf = osm_pbf)
  # Create the travel time matrix
  bicycle <- tt_calculate(
    centroids = DA_table,
    max_dist = 30000,
    routing_server = routing_server
  )
  rem_container(routing_server)

  # Car mode ----------------------------------------------------------------

  # Build the docker image for car routing
  tt_local_osrm(dest_folder = dest_folder, mode = "car", osm_pbf = osm_pbf)
  # Create the travel time matrix
  car <- tt_calculate(
    centroids = DA_table,
    max_dist = 120000,
    routing_server = routing_server
  )
  rem_container(routing_server)

  # Return all --------------------------------------------------------------

  return(list(car = car, bicycle = bicycle, foot = foot))
}
