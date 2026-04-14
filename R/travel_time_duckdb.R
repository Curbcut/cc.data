# =============================================================================
# Travel-time matrix — DuckDB access layer
#
# Two access tracks built on the same S3-hosted Parquet layout:
#
#   LOCAL DUCKDB (Steps 1 → 3)
#     Best for: batch analytics, model runs, repeated queries on a stable machine.
#     Data flow: S3 .qs batches → shredded Parquet fragments on S3 →
#                merged/sorted Parquet on S3 → local DuckDB file
#     Query API: tt_connect() + tt_query()
#
#   S3 DIRECT / LAMBDA (Steps 1 → 2 → 3a → 3b/3c)
#     Best for: serverless APIs, AWS Lambda, any environment without persistent disk.
#     Data flow: same S3 Parquet, queried in-place via DuckDB httpfs
#     A tiny origin-range index (built once by tt_build_index()) tells DuckDB
#     which part files to open, so queries scan only 1–3 files instead of all N.
#     Query API: tt_connect_s3() + tt_query_s3()
#
# STORAGE LAYOUT
#   curbcut.traveltimes/
#     ttm_frags/mode={mode}/part-{NNNNN}/batch-{NNNN}.parquet   <- temporary fragments
#     ttm/mode={mode}/part-{NNNNN}.parquet                       <- final sorted parts
#     ttm_index/origin_range_index.qs                            <- footer-derived index (R)
#     ttm_index/origin_range_index.json                          <- footer-derived index (JS)
#
#   Each final part file is sorted by origin_db. DuckDB zone maps (min/max
#   stored per row group in the Parquet footer) turn origin range scans into
#   skip operations, so queries that filter by origin touch only the relevant
#   row groups — no full-file scans.
#
# OPTIONAL: S3 EXPRESS ONE ZONE (added query performance)
#   The hot data (ttm/mode=*/part-*.parquet) can be stored in an S3 Express
#   One Zone directory bucket instead of standard S3. Express One Zone is
#   co-located with the compute (Lambda / EC2) in the same Availability Zone,
#   reducing first-byte S3 latency from ~10–50 ms per request to ~1–5 ms.
#
#   Since DuckDB httpfs makes multiple range requests per file (footer read +
#   one request per needed row group), a typical query touching 2 part files
#   may issue 10–16 S3 requests. The difference between 30 ms and 3 ms per
#   request compounds: ~400 ms of S3 overhead becomes ~40 ms.
#
#   Cost delta (us-east-1 approximate):
#     Storage:  $0.16/GB-month vs $0.023/GB-month (~7x). On 150 GB of Parquet
#               that is roughly +$20/month.
#     Requests: $0.003/1,000 GETs vs $0.0004 — negligible at query volumes.
#     Transfer: free within the same AZ (Lambda → Express bucket, same AZ).
#
#   Setup steps:
#     1. Create a directory bucket in the same AZ as the compute:
#          curbcut.traveltimes--use1-az4--x-s3  (naming is mandatory)
#     2. Copy the final parts:
#          aws s3 cp s3://curbcut.traveltimes/ttm/ \
#                   s3://curbcut.traveltimes--use1-az4--x-s3/ttm/ --recursive
#     3. Add s3express:CreateSession to the IAM execution role.
#     4. Change two DuckDB settings on the connection (see tt_connect_s3):
#          SET s3_endpoint = 's3express-use1-az4.us-east-1.amazonaws.com';
#          SET s3_url_style = 'path';   -- unchanged
#
#   The index files (origin_range_index.qs / .json) are fetched once at
#   startup and do not need to move to Express — keep them on standard S3.
#
# ID ENCODING
#   origin_db and dest_db are 11-digit dissemination-block UIDs. R's default
#   numeric type (double, 53-bit mantissa) cannot represent them exactly and
#   produces scientific notation in SQL strings ("1e10" instead of
#   "10010165001"), silently returning zero rows. Always use
#   bit64::as.integer64() or as.character() before interpolating into SQL.
#   Both columns are stored as UBIGINT in Parquet/DuckDB.
#
# REQUIRED PACKAGES (not in DESCRIPTION Imports — install as needed)
#   duckdb, DBI          always required
#   arrow                tt_shred_to_s3(), tt_import_mode() (writing Parquet)
#   bit64                tt_query_s3() (safe INT64 handling)
#   aws.s3, aws.signature  build steps + tt_connect_s3() (S3 access from R)
#
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper — retry wrapper for aws.s3::put_object
# -----------------------------------------------------------------------------

#' Upload a file to S3 with automatic retry
#'
#' Wraps `aws.s3::put_object()` with exponential-ish backoff. Used internally
#' by the build pipeline to handle transient S3 upload failures.
#'
#' @param file <`character`> Local path of the file to upload.
#' @param object <`character`> Destination S3 key.
#' @param bucket <`character`> S3 bucket name.
#' @param retries <`integer`> Maximum number of attempts. Default 5.
#' @param wait <`integer`> Seconds to wait between attempts. Default 10.
#'
#' @return Invisibly returns the result of the successful `put_object()` call.
#' @keywords internal
put_object_retry <- function(file, object, bucket, retries = 5, wait = 10) {
  for (attempt in seq_len(retries)) {
    result <- tryCatch(
      aws.s3::put_object(
        file = file,
        object = object,
        bucket = bucket,
        multipart = TRUE
      ),
      error = function(e) e
    )
    if (!inherits(result, "error")) {
      return(invisible(result))
    }
    message(sprintf(
      "    Upload attempt %d failed: %s — retrying in %ds ...",
      attempt,
      conditionMessage(result),
      wait
    ))
    Sys.sleep(wait)
  }
  stop(sprintf("Upload failed after %d attempts: %s", retries, object))
}


# =============================================================================
# BUILD PIPELINE — run once per mode, safe to interrupt and resume
# =============================================================================

#' Build the origin manifest for a travel-time mode (Step 1a)
#'
#' Scans all `.qs` batch files for a given transport mode in the source S3
#' bucket, records which origin IDs live in each batch file, and assigns each
#' origin to a numbered output "part" so that each part holds at most
#' `rows_per_part` rows across all origins in it.
#'
#' The manifest is saved as `manifest_{mode}.qs` in `manifest_dir`. If the
#' file already exists the function loads and returns it without re-scanning —
#' delete the file manually to force a rebuild.
#'
#' @param mode <`character`> Transport mode. One of `"car"`, `"bicycle"`,
#'   `"foot"`.
#' @param source_bucket <`character`> S3 bucket containing the raw `.qs`
#'   batches. Default `"curbcut.routing"`.
#' @param source_prefix <`character`> Key prefix (year folder) inside
#'   `source_bucket`. Default `"2026"`.
#' @param rows_per_part <`numeric`> Target maximum number of
#'   origin→destination rows per output part file. Default `25e6`.
#' @param min_batch_size <`integer`> Minimum file size in bytes; smaller files
#'   are assumed empty and skipped. Default `1000`.
#' @param manifest_dir <`character`> Local directory where the manifest `.qs`
#'   file is written. Default `"calculated_ignore"`.
#'
#' @return Invisibly returns the manifest as a `data.table` with columns
#'   `origin_db`, `n_rows`, `s3_key`, `cum_rows`, `part_num`.
#'
#' @section Resume behaviour:
#'   If `manifest_{mode}.qs` exists in `manifest_dir`, the manifest is loaded
#'   from disk and returned immediately. This lets you safely kill and resume
#'   the build pipeline without re-scanning all batch files.
#'   To force a fresh scan, delete the file:
#'   ```r
#'   file.remove("calculated_ignore/manifest_car.qs")
#'   tt_build_manifest("car")
#'   ```
#'
#' @export
tt_build_manifest <- function(
  mode,
  source_bucket = "curbcut.routing",
  source_prefix = "2026",
  rows_per_part = 25e6,
  min_batch_size = 1000,
  manifest_dir = "calculated_ignore"
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  manifest_path <- file.path(manifest_dir, paste0("manifest_", mode, ".qs"))
  if (file.exists(manifest_path)) {
    message(sprintf(
      "[%s] Manifest already exists at %s, loading it.",
      mode,
      manifest_path
    ))
    return(invisible(qs::qread(manifest_path)))
  }

  src_prefix <- paste0(source_prefix, "/", mode, "/")
  objs <- aws.s3::get_bucket_df(bucket = source_bucket, prefix = src_prefix, max = 1000)
  objs <- objs[grepl("\\.qs$", objs$Key), ]
  objs$Size <- as.numeric(objs$Size)
  objs <- objs[objs$Size > min_batch_size, ]

  message(sprintf(
    "[%s] Found %d batch files (total %.1f GB)",
    mode,
    nrow(objs),
    sum(objs$Size) / 1e9
  ))

  manifest_list <- vector("list", nrow(objs))
  tmp_qs <- tempfile(fileext = ".qs")
  file.create(tmp_qs)
  on.exit(unlink(tmp_qs), add = TRUE)

  for (i in seq_len(nrow(objs))) {
    message(sprintf(
      "  Scanning [%d/%d]: %s (%.2f GB)",
      i,
      nrow(objs),
      basename(objs$Key[i]),
      objs$Size[i] / 1e9
    ))
    aws.s3::save_object(object = objs$Key[i], bucket = source_bucket, file = tmp_qs)
    batch <- qs::qread(tmp_qs)
    manifest_list[[i]] <- data.table::data.table(
      origin_db = names(batch),
      n_rows = vapply(batch, nrow, integer(1L)),
      s3_key = objs$Key[i]
    )
    rm(batch)
    gc()
  }

  manifest <- data.table::rbindlist(manifest_list)
  rm(manifest_list)
  gc()

  data.table::setorder(manifest, origin_db)
  manifest[, cum_rows := cumsum(as.numeric(n_rows))]
  manifest[, part_num := as.integer((cum_rows - 1) %/% rows_per_part)]

  n_parts <- manifest[, max(part_num)] + 1L
  message(sprintf(
    "  Total origins: %s | Total rows: %s | Parts: %d",
    format(nrow(manifest), big.mark = ","),
    format(sum(manifest$n_rows), big.mark = ","),
    n_parts
  ))

  qs::qsave(manifest, manifest_path)
  message(sprintf("  Manifest saved to %s", manifest_path))
  invisible(manifest)
}


#' Shred .qs batches into per-part Parquet fragments on S3 (Step 1b)
#'
#' Reads each raw `.qs` batch from the source bucket, flattens it into a
#' long-format `data.table`, assigns each row to the part number determined by
#' the manifest, and uploads one Parquet fragment per part to the destination
#' bucket. Each fragment is compressed with Zstandard level 3.
#'
#' Already-uploaded batches are detected via S3 key names and skipped, so the
#' function resumes cleanly after interruptions. A final consistency check
#' aborts with an error if any batch is still missing — preventing a premature
#' call to [tt_merge_parts()].
#'
#' @param mode <`character`> Transport mode. One of `"car"`, `"bicycle"`,
#'   `"foot"`.
#' @param source_bucket <`character`> S3 bucket containing the raw `.qs`
#'   batches. Default `"curbcut.routing"`.
#' @param dest_bucket <`character`> S3 bucket for output fragments and final
#'   parts. Default `"curbcut.traveltimes"`.
#' @param manifest_dir <`character`> Directory where `manifest_{mode}.qs` was
#'   written by [tt_build_manifest()]. Default `"calculated_ignore"`.
#'
#' @return Called for side effects. Stops with an error if shredding is
#'   incomplete at the end (safe guard against premature merging).
#'
#' @section Fragment layout:
#'   Fragments are uploaded to
#'   `ttm_frags/mode={mode}/part-{NNNNN}/{batch_label}.parquet`.
#'   `tt_merge_parts()` reads this prefix to collect all fragments for each
#'   part.
#'
#' @section ID encoding:
#'   `origin_db` and `dest_db` are cast to `INT64` via `bit64::as.integer64()`
#'   before writing. This maps to `UBIGINT` in Parquet and avoids the
#'   scientific-notation corruption that occurs when 11-digit IDs pass through
#'   R's `double` type.
#'
#' @export
tt_shred_to_s3 <- function(
  mode,
  source_bucket = "curbcut.routing",
  dest_bucket = "curbcut.traveltimes",
  manifest_dir = "calculated_ignore"
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  manifest_path <- file.path(manifest_dir, paste0("manifest_", mode, ".qs"))
  stopifnot(file.exists(manifest_path))
  manifest <- qs::qread(manifest_path)

  origin_to_part <- manifest$part_num
  names(origin_to_part) <- manifest$origin_db

  frags_prefix <- paste0("ttm_frags/mode=", mode, "/")

  done_batches <- tryCatch(
    {
      existing <- aws.s3::get_bucket_df(
        bucket = dest_bucket,
        prefix = frags_prefix,
        max = 100000
      )
      existing <- existing[grepl("\\.parquet$", existing$Key), ]
      unique(gsub(".*(batch-\\d+)\\.parquet", "\\1", existing$Key))
    },
    error = function(e) character(0)
  )

  objs <- unique(manifest$s3_key)
  message(sprintf(
    "[%s] %d batches total | %d already shredded | %d remaining",
    mode,
    length(objs),
    length(done_batches),
    length(objs) - length(done_batches)
  ))

  tmp_qs <- tempfile(fileext = ".qs")
  tmp_frag <- tempfile(fileext = ".parquet")
  file.create(tmp_qs)
  file.create(tmp_frag)
  on.exit(
    {
      unlink(tmp_qs)
      unlink(tmp_frag)
    },
    add = TRUE
  )

  for (i in seq_along(objs)) {
    s3_key <- objs[i]
    batch_label <- sprintf("batch-%04d", i)

    if (batch_label %in% done_batches) {
      message(sprintf(
        "  Skipping [%d/%d]: %s (already done)",
        i,
        length(objs),
        batch_label
      ))
      next
    }

    message(sprintf(
      "  Shredding [%d/%d]: %s",
      i,
      length(objs),
      basename(s3_key)
    ))

    aws.s3::save_object(object = s3_key, bucket = source_bucket, file = tmp_qs)
    batch <- qs::qread(tmp_qs)

    flat <- data.table::rbindlist(lapply(names(batch), function(orig) {
      dt <- batch[[orig]]
      data.table::data.table(
        origin_db = orig,
        dest_db = dt$id,
        time_s = dt$time,
        dist_m = dt$distance
      )
    }))
    rm(batch)
    gc()

    # Resolve part numbers before converting IDs.
    # as.integer() overflows for 11-digit DB UIDs; bit64::as.integer64()
    # maps to INT64 Parquet, read by DuckDB as UBIGINT.
    flat[, part_num := origin_to_part[origin_db]]

    # Guard: origins absent from manifest would produce NA part_num and be
    # silently dropped by data.table subsetting. Fail loudly instead.
    n_na <- sum(is.na(flat$part_num))
    if (n_na > 0) {
      stop(sprintf(
        "Batch %s: %d origins not in manifest — delete manifest_%s.qs and rebuild.",
        batch_label,
        n_na,
        mode
      ))
    }

    flat[, origin_db := bit64::as.integer64(origin_db)]
    flat[, dest_db := bit64::as.integer64(dest_db)]

    parts_in_batch <- unique(flat$part_num)

    for (p in parts_in_batch) {
      chunk <- flat[part_num == p]
      chunk[, part_num := NULL]
      arrow::write_parquet(
        chunk,
        tmp_frag,
        compression = "zstd",
        compression_level = 3L
      )
      frag_key <- paste0(
        frags_prefix,
        sprintf("part-%05d/", p),
        batch_label,
        ".parquet"
      )
      put_object_retry(file = tmp_frag, object = frag_key, bucket = dest_bucket)
    }

    rm(flat)
    gc()
    message(sprintf("    -> %d fragments uploaded", length(parts_in_batch)))
  }

  # Verify all batches are done before allowing merge to proceed
  done_now <- tryCatch(
    {
      existing <- aws.s3::get_bucket_df(
        bucket = dest_bucket,
        prefix = frags_prefix,
        max = 100000
      )
      existing <- existing[grepl("\\.parquet$", existing$Key), ]
      length(unique(gsub(".*(batch-\\d+)\\.parquet", "\\1", existing$Key)))
    },
    error = function(e) 0L
  )

  if (done_now < length(objs)) {
    stop(sprintf(
      "[%s] Only %d/%d batches confirmed on S3 — do NOT run tt_merge_parts yet.",
      mode,
      done_now,
      length(objs)
    ))
  }

  message(sprintf(
    "[%s] Shredding complete. All %d batches confirmed on S3.",
    mode,
    length(objs)
  ))
}


#' Merge per-part Parquet fragments into sorted final files (Step 1c)
#'
#' For each part, collects all fragment files uploaded by [tt_shred_to_s3()],
#' merges them inside DuckDB (no large R allocations), sorts by `origin_db`,
#' writes a single compressed Parquet to disk, uploads it, then deletes the
#' fragment files. Sorting by `origin_db` is the key step that enables DuckDB
#' zone-map skipping during queries.
#'
#' @param mode <`character`> Transport mode. One of `"car"`, `"bicycle"`,
#'   `"foot"`.
#' @param dest_bucket <`character`> S3 bucket for fragments and final parts.
#'   Default `"curbcut.traveltimes"`.
#' @param region <`character`> AWS region for `httpfs`. Default `"us-east-1"`.
#' @param memory_limit <`character`> DuckDB memory cap passed as a string
#'   (e.g. `"24GB"`). DuckDB will spill to disk if the merge exceeds this.
#'   Default `"24GB"`.
#'
#' @return Called for side effects.
#'
#' @section Resume and stale-merge detection:
#'   A part is skipped only if its final Parquet exists **and** zero fragments
#'   remain for it. If fragments still exist for an already-finalized part, the
#'   previous merge ran while [tt_shred_to_s3()] was still in progress — the
#'   stale final is deleted and the part is re-merged with the complete
#'   fragment set.
#'
#' @section Why sort by origin_db:
#'   DuckDB writes min/max statistics for `origin_db` into every row-group
#'   footer (zone maps). A `WHERE origin_db IN (...)` query with a narrow
#'   range skips entire row groups whose max < query_min or min > query_max,
#'   reducing IO dramatically for geographically localised queries.
#'
#' @export
tt_merge_parts <- function(
  mode,
  dest_bucket = "curbcut.traveltimes",
  region = "us-east-1",
  memory_limit = "24GB"
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  frags_prefix <- paste0("ttm_frags/mode=", mode, "/")
  dest_prefix <- paste0("ttm/mode=", mode, "/")

  all_frags <- aws.s3::get_bucket_df(
    bucket = dest_bucket,
    prefix = frags_prefix,
    max = 100000
  )
  all_frags <- all_frags[grepl("\\.parquet$", all_frags$Key), ]

  part_labels <- sort(unique(gsub(".*(part-\\d+)/.*", "\\1", all_frags$Key)))
  message(sprintf(
    "[%s] %d parts with remaining fragments to process",
    mode,
    length(part_labels)
  ))

  # Existing finals — used to detect stale merges, not to skip blindly
  existing_finals <- tryCatch(
    {
      ef <- aws.s3::get_bucket_df(
        bucket = dest_bucket,
        prefix = dest_prefix,
        max = 10000
      )
      ef <- ef[grepl("\\.parquet$", ef$Key), ]
      gsub("\\.parquet$", "", basename(ef$Key))
    },
    error = function(e) character(0)
  )

  tmp_final <- paste0("_tt_final_", mode, ".parquet")
  on.exit(unlink(tmp_final), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s';", memory_limit))
  DBI::dbExecute(con, "SET threads = 4;")

  creds <- aws.signature::locate_credentials()
  DBI::dbExecute(con, sprintf("SET s3_access_key_id     = '%s';", creds$key))
  DBI::dbExecute(con, sprintf("SET s3_secret_access_key = '%s';", creds$secret))
  if (!is.null(creds$session_token) && nchar(creds$session_token) > 0) {
    DBI::dbExecute(con, sprintf("SET s3_session_token = '%s';", creds$session_token))
  }

  for (part_label in part_labels) {
    part_frags <- all_frags[
      grepl(paste0("/", part_label, "/"), all_frags$Key, fixed = TRUE),
    ]

    if (nrow(part_frags) == 0) {
      message(sprintf("  [%s] No fragments found, skipping", part_label))
      next
    }

    if (part_label %in% existing_finals) {
      message(sprintf(
        "  [%s] Stale merge detected (%d orphaned fragments) — deleting final and redoing.",
        part_label,
        nrow(part_frags)
      ))
      aws.s3::delete_object(
        object = paste0(dest_prefix, part_label, ".parquet"),
        bucket = dest_bucket
      )
    } else {
      message(sprintf(
        "  [%s] Merging %d fragments via DuckDB ...",
        part_label,
        nrow(part_frags)
      ))
    }

    s3_paths <- paste0(
      "s3://",
      dest_bucket,
      "/",
      part_frags$Key,
      collapse = "', '"
    )

    DBI::dbExecute(
      con,
      sprintf(
        "COPY (
         SELECT
           origin_db::UBIGINT AS origin_db,
           dest_db::UBIGINT   AS dest_db,
           time_s,
           dist_m
         FROM read_parquet(['%s'])
         WHERE time_s IS NOT NULL
         ORDER BY origin_db, dest_db
       ) TO '%s'
       (FORMAT PARQUET, COMPRESSION 'zstd', ROW_GROUP_SIZE 1000000)",
        s3_paths,
        tmp_final
      )
    )

    message(sprintf(
      "    Written %.1f MB | Uploading ...",
      file.info(tmp_final)$size / 1e6
    ))

    final_key <- paste0(dest_prefix, part_label, ".parquet")
    put_object_retry(file = tmp_final, object = final_key, bucket = dest_bucket)

    for (k in part_frags$Key) {
      aws.s3::delete_object(object = k, bucket = dest_bucket)
    }
    message(sprintf("    Done: %s (fragments deleted)", part_label))
  }

  message(sprintf("[%s] All parts merged and uploaded.", mode))
}


# =============================================================================
# LOCAL DUCKDB TRACK — Steps 2 + 3
# =============================================================================

#' Import S3 Parquet into a local DuckDB file (Step 2)
#'
#' Pulls all final Parquet part files for a mode from S3 and inserts them into
#' a persistent local DuckDB table (`ttm_{mode}`), sorted by `origin_db`. This
#' is a one-time import per mode; subsequent queries hit the local `.duckdb`
#' file with no S3 latency.
#'
#' @param mode <`character`> Transport mode. One of `"car"`, `"bicycle"`,
#'   `"foot"`.
#' @param db_path <`character`> Path to the local DuckDB file. Created if it
#'   does not exist. Default `"calculated_ignore/traveltimes.duckdb"`.
#' @param source_bucket <`character`> S3 bucket containing the final Parquet
#'   parts. Default `"curbcut.traveltimes"`.
#' @param region <`character`> AWS region for `httpfs`. Default `"us-east-1"`.
#' @param memory_limit <`character`> DuckDB memory cap (e.g. `"90GB"`). The
#'   sort during import can be memory-intensive; set generously. Default
#'   `"90GB"`.
#' @param threads <`integer`> DuckDB thread count. Default `8L`.
#'
#' @return Invisibly returns the row count of the imported table.
#'
#' @section Idempotency:
#'   If the table already exists, the function prints the row count and returns
#'   without importing. Drop the table manually to reimport:
#'   ```r
#'   con <- tt_connect()
#'   DBI::dbExecute(con, "DROP TABLE ttm_bicycle")
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#'   tt_import_mode("bicycle")
#'   ```
#'
#' @section Disk and memory requirements:
#'   Peak disk usage is the final `.duckdb` file size plus DuckDB sort spill.
#'   Expect roughly 1.5× the raw Parquet size on disk during import. The final
#'   DuckDB file is typically similar in size to the source Parquet.
#'
#' @export
tt_import_mode <- function(
  mode,
  db_path = "calculated_ignore/traveltimes.duckdb",
  source_bucket = "curbcut.traveltimes",
  region = "us-east-1",
  memory_limit = "90GB",
  threads = 8L
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  con <- DBI::dbConnect(duckdb::duckdb(dbdir = db_path))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s';", memory_limit))
  DBI::dbExecute(con, sprintf("SET threads = %d;", as.integer(threads)))

  table_name <- paste0("ttm_", mode)
  s3_path <- sprintf("s3://%s/ttm/mode=%s/part-*.parquet", source_bucket, mode)

  tables <- DBI::dbGetQuery(con, "SHOW TABLES")$name
  if (table_name %in% tables) {
    n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n
    message(sprintf(
      "[%s] Already imported (%s rows). Drop table first to reimport.",
      mode,
      format(n, big.mark = ",")
    ))
    return(invisible(n))
  }

  message(sprintf("[%s] Importing from %s ...", mode, s3_path))
  t <- system.time(DBI::dbExecute(
    con,
    sprintf(
      "CREATE TABLE %s AS
     SELECT
       origin_db::UBIGINT AS origin_db,
       dest_db::UBIGINT   AS dest_db,
       time_s,
       dist_m
     FROM read_parquet('%s', hive_partitioning = false)
     WHERE time_s IS NOT NULL
     ORDER BY origin_db",
      table_name,
      s3_path
    )
  ))

  n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n
  message(sprintf(
    "[%s] Done in %.1f min | %s rows",
    mode,
    t["elapsed"] / 60,
    format(n, big.mark = ",")
  ))
  invisible(n)
}


#' Open a read-write connection to the local DuckDB travel-time database
#'
#' Returns a DBI connection to the persistent `.duckdb` file populated by
#' [tt_import_mode()]. Close with `DBI::dbDisconnect(con, shutdown = TRUE)`
#' when done.
#'
#' @param db_path <`character`> Path to the local DuckDB file.
#'   Default `"calculated_ignore/traveltimes.duckdb"`.
#'
#' @return A `DBIConnection` object.
#'
#' @seealso [tt_query()] for querying, [tt_import_mode()] for building the
#'   local database.
#'
#' @export
tt_connect <- function(db_path = "calculated_ignore/traveltimes.duckdb") {
  DBI::dbConnect(duckdb::duckdb(dbdir = db_path, read_only = FALSE))
}


#' Query travel times from the local DuckDB database
#'
#' Filters the `ttm_{mode}` table by one or more origin IDs and optionally
#' by destination IDs and/or a maximum travel time.
#'
#' @param con <`DBIConnection`> Connection returned by [tt_connect()].
#' @param origin_ids <`character`|`integer64`|`numeric`> Vector of origin
#'   dissemination-block UIDs to query.
#' @param dest_ids <`character`|`integer64`|`numeric`|`NULL`> Optional vector
#'   of destination UIDs to restrict results. `NULL` returns all reachable
#'   destinations.
#' @param mode <`character`> Transport mode table to query. One of `"car"`,
#'   `"bicycle"`, `"foot"`. Default `"bicycle"`.
#' @param max_time_s <`integer`|`NULL`> Optional upper bound on travel time in
#'   seconds. `NULL` imposes no limit.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`data`}{A `data.frame` with columns `origin_db`, `dest_db`,
#'       `time_s`, `dist_m`.}
#'     \item{`elapsed_sec`}{Query wall-clock time in seconds.}
#'   }
#'
#' @section ID encoding:
#'   IDs are passed through `as.character()` before interpolation into SQL to
#'   prevent R's `double` representation from producing scientific notation
#'   (`1e10`) for 11-digit UIDs, which silently returns zero rows.
#'
#' @section Performance note:
#'   The local DuckDB table is sorted by `origin_db`. DuckDB zone maps let
#'   narrow origin-range queries skip entire row groups, so queries for
#'   geographically clustered origins (same CSD or CMA) are fast even on
#'   large tables. For large destination filters, prefer the S3 track
#'   ([tt_query_s3()]) or filter post-hoc in R.
#'
#' @export
tt_query <- function(
  con,
  origin_ids,
  dest_ids = NULL,
  mode = "bicycle",
  max_time_s = NULL
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  table_name <- paste0("ttm_", mode)

  where <- sprintf(
    "WHERE origin_db IN (%s)",
    paste(as.character(origin_ids), collapse = ", ")
  )

  if (!is.null(dest_ids)) {
    where <- paste0(
      where,
      sprintf(
        " AND dest_db IN (%s)",
        paste(as.character(dest_ids), collapse = ", ")
      )
    )
  }

  if (!is.null(max_time_s)) {
    where <- paste0(where, sprintf(" AND time_s <= %d", as.integer(max_time_s)))
  }

  sql <- sprintf(
    "SELECT origin_db, dest_db, time_s, dist_m FROM %s %s",
    table_name,
    where
  )

  elapsed <- system.time(result <- DBI::dbGetQuery(con, sql))
  list(data = result, elapsed_sec = unname(elapsed["elapsed"]))
}


# =============================================================================
# S3 DIRECT / LAMBDA TRACK — Steps 3a → 3b → 3c
# =============================================================================

#' Build the origin-range index from Parquet footers (Step 3a)
#'
#' Reads only the footer metadata (row-group statistics) from every final
#' Parquet file across all three modes — no row data is scanned. Extracts the
#' minimum and maximum `origin_db` value per file and saves the result in two
#' formats to S3:
#'
#' - **`.qs`** — for R consumers ([tt_connect_s3()] / [tt_query_s3()])
#' - **`.json`** — for non-R consumers (Node.js / Express / any HTTP client)
#'
#' Run once after [tt_merge_parts()] completes, or whenever part files change.
#'
#' @param dest_bucket <`character`> S3 bucket for final parts and the index.
#'   Default `"curbcut.traveltimes"`.
#' @param index_key <`character`> S3 key for the `.qs` index consumed by R.
#'   Default `"ttm_index/origin_range_index.qs"`.
#' @param json_key <`character`> S3 key for the JSON index consumed by non-R
#'   callers (Node.js, Express, etc.).
#'   Default `"ttm_index/origin_range_index.json"`.
#' @param region <`character`> AWS region for `httpfs`. Default `"us-east-1"`.
#' @param memory_limit <`character`> DuckDB memory cap. `"4GB"` is sufficient
#'   since no row data is read. Default `"4GB"`.
#'
#' @return Invisibly returns the index as a `data.table` with columns
#'   `s3_path`, `min_origin_db`, `max_origin_db`, `mode`.
#'
#' @section How the index is used:
#'   Both files encode the same data: one row per final Parquet file with the
#'   minimum and maximum `origin_db` present in that file. A caller filters
#'   this index to find files whose `[min_origin_db, max_origin_db]` range
#'   overlaps the requested origins, then passes only those S3 paths to DuckDB.
#'   Because part files are non-overlapping and origins within each file are
#'   sorted, this prune is exact — no false positives.
#'
#' @section JSON format and ID encoding:
#'   The JSON file is a flat array of objects, one per part file:
#'   ```json
#'   [
#'     {
#'       "s3_path":       "s3://curbcut.traveltimes/ttm/mode=car/part-00000.parquet",
#'       "mode":          "car",
#'       "min_origin_db": "10010000001",
#'       "max_origin_db": "10019999999"
#'     },
#'     ...
#'   ]
#'   ```
#'   `min_origin_db` and `max_origin_db` are serialised as **strings**, not
#'   JSON numbers. JSON numbers are IEEE 754 doubles (53-bit mantissa), the
#'   same representation as R's `numeric` — 11-digit DB UIDs fit, but values
#'   approaching 2^53 (~9 trillion) would not. Using strings sidesteps the
#'   issue entirely and costs nothing at this scale.
#'
#' @section Node.js / Express usage:
#'   Load the index once at server startup, then use it to prune files before
#'   every query:
#'   ```js
#'   import { GetObjectCommand, S3Client } from '@aws-sdk/client-s3';
#'   import { Database } from '@duckdb/node-api';
#'
#'   // --- startup ---
#'   const s3     = new S3Client({ region: 'us-east-1' });
#'   const body   = await s3.send(new GetObjectCommand({
#'     Bucket: 'curbcut.traveltimes',
#'     Key:    'ttm_index/origin_range_index.json'
#'   }));
#'   const index  = JSON.parse(await body.Body.transformToString());
#'
#'   const db     = await Database.create(':memory:');
#'   const conn   = await db.connect();
#'   await conn.run("INSTALL httpfs; LOAD httpfs;");
#'   await conn.run("SET s3_region = 'us-east-1';");
#'   // credentials picked up from AWS_ACCESS_KEY_ID / IAM role automatically
#'
#'   // --- per-request query ---
#'   function queryTravelTimes(originIds, mode, maxTimeSec = null) {
#'     const originBigInts = originIds.map(BigInt);
#'     const minId = originBigInts.reduce((a, b) => a < b ? a : b);
#'     const maxId = originBigInts.reduce((a, b) => a > b ? a : b);
#'
#'     // Prune to relevant files using the index
#'     const files = index
#'       .filter(r => r.mode === mode
#'              && BigInt(r.max_origin_db) >= minId
#'              && BigInt(r.min_origin_db) <= maxId)
#'       .map(r => r.s3_path);
#'
#'     if (files.length === 0) return [];
#'
#'     const fileList  = files.map(f => `'${f}'`).join(', ');
#'     const originCsv = originIds.join(', ');
#'     const timePred  = maxTimeSec ? ` AND time_s <= ${maxTimeSec}` : '';
#'
#'     const sql = `
#'       SELECT origin_db::UBIGINT, dest_db::UBIGINT, time_s, dist_m
#'       FROM read_parquet([${fileList}], hive_partitioning = false)
#'       WHERE origin_db IN (${originCsv})${timePred}`;
#'
#'     return conn.all(sql);
#'   }
#'   ```
#'   Note: origin IDs must be passed as plain integers in the SQL `IN` clause
#'   (not quoted strings) so DuckDB matches them against the UBIGINT column
#'   without a cast. JavaScript's `BigInt` is used only for the index
#'   comparison in JS — not interpolated into the SQL string.
#'
#' @export
tt_build_index <- function(
  dest_bucket = "curbcut.traveltimes",
  index_key   = "ttm_index/origin_range_index.qs",
  json_key    = "ttm_index/origin_range_index.json",
  region      = "us-east-1",
  memory_limit = "4GB"
) {
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s';", memory_limit))

  creds <- aws.signature::locate_credentials()
  DBI::dbExecute(con, sprintf("SET s3_access_key_id     = '%s';", creds$key))
  DBI::dbExecute(con, sprintf("SET s3_secret_access_key = '%s';", creds$secret))
  if (!is.null(creds$session_token) && nchar(creds$session_token) > 0) {
    DBI::dbExecute(con, sprintf("SET s3_session_token = '%s';", creds$session_token))
  }

  index_list <- lapply(c("foot", "bicycle", "car"), function(mode) {
    s3_glob <- sprintf("s3://%s/ttm/mode=%s/part-*.parquet", dest_bucket, mode)
    message(sprintf("[%s] Reading footers ...", mode))

    # parquet_metadata() reads only file footers — zero row data scanned.
    # column_id = 0 is origin_db (first column written by tt_merge_parts).
    # stats_min/max_value are stored as VARCHAR by DuckDB; cast via UBIGINT.
    df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT
         file_name                              AS s3_path,
         MIN(stats_min_value::UBIGINT)          AS min_origin_db,
         MAX(stats_max_value::UBIGINT)          AS max_origin_db
       FROM parquet_metadata('%s')
       WHERE column_id = 0
       GROUP BY file_name
       ORDER BY min_origin_db",
        s3_glob
      )
    )
    df$mode <- mode
    df
  })

  index <- data.table::rbindlist(index_list)
  message(sprintf("Index built: %d file entries across 3 modes", nrow(index)))

  # --- .qs for R consumers ---
  tmp_qs <- tempfile(fileext = ".qs")
  on.exit(unlink(tmp_qs), add = TRUE)
  qs::qsave(index, tmp_qs)
  put_object_retry(file = tmp_qs, object = index_key, bucket = dest_bucket)
  message(sprintf("R index saved to   s3://%s/%s", dest_bucket, index_key))

  # --- JSON for non-R consumers (Node.js / Express / etc.) ---
  # ID columns are serialised as strings to avoid IEEE 754 precision loss.
  # JSON numbers are doubles (53-bit mantissa); string representation is exact.
  index_json <- as.data.frame(index)
  index_json$min_origin_db <- as.character(index_json$min_origin_db)
  index_json$max_origin_db <- as.character(index_json$max_origin_db)

  tmp_json <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_json), add = TRUE)
  jsonlite::write_json(index_json, tmp_json, auto_unbox = TRUE)
  put_object_retry(file = tmp_json, object = json_key, bucket = dest_bucket)
  message(sprintf("JSON index saved to s3://%s/%s", dest_bucket, json_key))

  invisible(index)
}


#' Open an S3-direct DuckDB connection with the origin-range index (Step 3b)
#'
#' Creates an in-memory DuckDB session configured to read Parquet files
#' directly from S3 via `httpfs`, downloads the tiny origin-range index built
#' by [tt_build_index()], and attaches it to the connection object so that
#' [tt_query_s3()] can prune which part files to open.
#'
#' @param dest_bucket <`character`> S3 bucket for final parts and the index.
#'   Default `"curbcut.traveltimes"`.
#' @param index_key <`character`> S3 key of the index saved by
#'   [tt_build_index()]. Default `"ttm_index/origin_range_index.qs"`.
#' @param region <`character`> AWS region. Default `"us-east-1"`.
#' @param memory_limit <`character`> DuckDB memory cap. `"8GB"` covers most
#'   query workloads; increase for very large result sets. Default `"8GB"`.
#' @param threads <`integer`> DuckDB thread count. For Lambda, set to the
#'   number of vCPUs allocated to the function (typically 2–6). Default `32L`.
#'
#' @return A `DBIConnection` object with two extra attributes:
#'   \describe{
#'     \item{`ttm_index`}{The origin-range index `data.table`.}
#'     \item{`ttm_bucket`}{The bucket name, forwarded to [tt_query_s3()].}
#'   }
#'
#' @section AWS credentials:
#'   Credentials are resolved via `aws.signature::locate_credentials()`, which
#'   checks (in order): explicit arguments, environment variables
#'   (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`), the
#'   shared credentials file, and the EC2/ECS/Lambda instance metadata
#'   endpoint. In Lambda this happens automatically from the function's IAM
#'   execution role — no key management needed.
#'
#' @section Lambda deployment:
#'   In a Lambda handler, create the connection **once per container** (outside
#'   the handler function) and reuse it across warm invocations. The index
#'   download adds ~100 ms on cold start and is free on warm calls:
#'
#'   ```r
#'   # Global scope — runs once per container lifecycle
#'   con <- tt_connect_s3(threads = 6L)   # 6 vCPUs on a 10 GB Lambda
#'
#'   # Handler — called on every invocation
#'   handler <- function(event, context) {
#'     result <- tt_query_s3(con, origin_ids = event$ids, mode = event$mode)
#'     result$data
#'   }
#'   ```
#'
#'   If your Lambda runtime does not support mutable global state, cache the
#'   index file in `/tmp` (persists across warm invocations on the same
#'   container instance):
#'
#'   ```r
#'   cached_index <- "/tmp/origin_range_index.qs"
#'   if (!file.exists(cached_index)) {
#'     aws.s3::save_object("ttm_index/origin_range_index.qs",
#'                         bucket = "curbcut.traveltimes",
#'                         file   = cached_index)
#'   }
#'   index <- qs::qread(cached_index)
#'   attr(con, "ttm_index")  <- index
#'   attr(con, "ttm_bucket") <- "curbcut.traveltimes"
#'   ```
#'
#' @section Sizing Lambda for travel-time queries:
#'   \itemize{
#'     \item **Memory**: 3–4 GB covers typical single-CSD origin sets.
#'       Increase to 10 GB for full-CMA queries (hundreds of origins × all
#'       destinations).
#'     \item **Threads**: match to vCPU count. AWS allocates 1 vCPU per 1,769
#'       MB of configured memory. A 3,538 MB function gets 2 vCPUs; a 7,076 MB
#'       function gets 4.
#'     \item **Timeout**: single-file queries finish in 2–5 s; multi-file
#'       queries in 10–30 s depending on part size and network throughput.
#'       Set Lambda timeout to at least 60 s.
#'     \item **Concurrency**: each invocation holds one in-memory DuckDB
#'       instance; connections are not shared across concurrent Lambda
#'       containers.
#'   }
#'
#' @section S3 Express One Zone (optional performance upgrade):
#'   For lower query latency, the final Parquet files (`ttm/mode=*/part-*.parquet`)
#'   can be stored in an S3 Express One Zone directory bucket co-located with
#'   the compute in the same Availability Zone. Express One Zone reduces
#'   first-byte S3 latency from ~10–50 ms to ~1–5 ms per request. Since DuckDB
#'   httpfs issues multiple range requests per file, this compounds to a
#'   meaningful end-to-end improvement (~400 ms → ~40 ms of pure S3 overhead
#'   for a typical 2-file query). Storage cost is ~7× higher than standard S3
#'   (~$0.16/GB-month vs $0.023) — on 150 GB of Parquet roughly +$20/month.
#'
#'   To use an Express bucket, change two DuckDB settings after connecting:
#'   ```r
#'   con <- tt_connect_s3()
#'   DBI::dbExecute(con, "SET s3_endpoint = 's3express-use1-az4.us-east-1.amazonaws.com';")
#'   # s3_url_style = 'path' is already set by tt_connect_s3 — no change needed
#'   ```
#'   The IAM role must also have `s3express:CreateSession` permission. The
#'   index files (`origin_range_index.qs` / `.json`) do not need to move to
#'   Express — they are fetched once at startup and are not on the query path.
#'   See the file-level header for full setup instructions.
#'
#' @seealso [tt_query_s3()], [tt_build_index()]
#' @export
tt_connect_s3 <- function(
  dest_bucket = "curbcut.traveltimes",
  index_key = "ttm_index/origin_range_index.qs",
  region = "us-east-1",
  memory_limit = "8GB",
  threads = 32L
) {
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:"))

  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s';", memory_limit))
  DBI::dbExecute(con, sprintf("SET threads = %d;", as.integer(threads)))

  creds <- aws.signature::locate_credentials()
  DBI::dbExecute(con, sprintf("SET s3_access_key_id     = '%s';", creds$key))
  DBI::dbExecute(con, sprintf("SET s3_secret_access_key = '%s';", creds$secret))
  if (!is.null(creds$session_token) && nchar(creds$session_token) > 0) {
    DBI::dbExecute(con, sprintf("SET s3_session_token = '%s';", creds$session_token))
  }

  # Pull the index and attach it to the connection object so tt_query_s3 can use it
  tmp <- tempfile(fileext = ".qs")
  on.exit(unlink(tmp), add = TRUE)
  aws.s3::save_object(object = index_key, bucket = dest_bucket, file = tmp)
  index <- qs::qread(tmp)

  attr(con, "ttm_index") <- index
  attr(con, "ttm_bucket") <- dest_bucket
  con
}


#' Query travel times directly from S3 Parquet using the origin-range index (Step 3c)
#'
#' Uses the index attached to `con` by [tt_connect_s3()] to identify which
#' part files overlap the requested origins, then issues a single DuckDB
#' `read_parquet([...])` query against only those files. Because part files are
#' non-overlapping and sorted by `origin_db`, the prune is exact: no false
#' positives, no full-table scans.
#'
#' @param con <`DBIConnection`> Connection returned by [tt_connect_s3()].
#' @param origin_ids <`character`|`integer64`|`numeric`> Vector of origin
#'   dissemination-block UIDs to query.
#' @param dest_ids <`character`|`integer64`|`numeric`|`NULL`> Optional
#'   destination UID filter. `NULL` returns all reachable destinations.
#' @param mode <`character`> Transport mode. One of `"car"`, `"bicycle"`,
#'   `"foot"`. Default `"bicycle"`.
#' @param max_time_s <`integer`|`NULL`> Optional upper bound on travel time in
#'   seconds.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`data`}{A `data.frame` with columns `origin_db`, `dest_db`,
#'       `time_s`, `dist_m`.}
#'     \item{`elapsed_sec`}{Query wall-clock time in seconds.}
#'   }
#'
#' @section Query performance — why it's fast:
#'   \enumerate{
#'     \item **File pruning via the index**: the index holds min/max
#'       `origin_db` per part file (derived from Parquet footer stats, not row
#'       data). `tt_query_s3()` filters to files whose range overlaps the
#'       query's `[min(origin_ids), max(origin_ids)]`. For geographically
#'       compact origin sets (same CSD) this is almost always 1 file.
#'     \item **Row-group skipping via zone maps**: within each opened file,
#'       DuckDB reads the footer's per-row-group min/max for `origin_db` and
#'       skips row groups whose range doesn't overlap the query. Only matching
#'       row groups are decompressed.
#'     \item **Predicate pushdown**: the `WHERE origin_db IN (...)` clause is
#'       pushed into `read_parquet()`, so DuckDB does not materialise the full
#'       file before filtering.
#'   }
#'   Together these reduce a multi-billion-row dataset to a sub-second scan
#'   for typical single-municipality queries.
#'
#' @section ID encoding:
#'   `origin_ids` (and `dest_ids` if provided) are cast to
#'   `bit64::integer64()` before min/max computation and to character before
#'   SQL interpolation, preventing scientific-notation corruption of 11-digit
#'   UIDs.
#'
#' @section Lambda usage example:
#'   ```r
#'   # Cold-start setup (once per container)
#'   con <- tt_connect_s3(memory_limit = "4GB", threads = 2L)
#'
#'   # Per-invocation query
#'   handler <- function(event, context) {
#'     res <- tt_query_s3(
#'       con,
#'       origin_ids = event$origin_ids,
#'       mode       = event$mode,
#'       max_time_s = 1800L   # 30-minute isochrone
#'     )
#'     jsonlite::toJSON(res$data)
#'   }
#'   ```
#'
#' @seealso [tt_connect_s3()], [tt_build_index()]
#' @export
tt_query_s3 <- function(
  con,
  origin_ids,
  dest_ids = NULL,
  mode = "bicycle",
  max_time_s = NULL
) {
  stopifnot(mode %in% c("car", "bicycle", "foot"))

  index <- attr(con, "ttm_index")
  bucket <- attr(con, "ttm_bucket")
  stopifnot(!is.null(index), !is.null(bucket))

  # Convert once — avoids scientific notation on 11-digit IDs
  origin_ids <- bit64::as.integer64(origin_ids)
  origin_min <- min(origin_ids)
  origin_max <- max(origin_ids)

  # Subset to only files whose origin range overlaps the requested origins
  m <- mode
  mode_index <- index[index$mode == m, ]
  relevant_files <- mode_index[
    mode_index$max_origin_db >= origin_min &
      mode_index$min_origin_db <= origin_max,
  ]

  if (nrow(relevant_files) == 0) {
    message("No files found for these origin IDs.")
    return(list(data = data.frame(), elapsed_sec = 0))
  }

  message(sprintf(
    "[%s] Scanning %d/%d files for %d origins",
    mode,
    nrow(relevant_files),
    nrow(mode_index),
    length(origin_ids)
  ))

  file_list <- paste(relevant_files$s3_path, collapse = "', '")

  origin_csv <- paste(as.character(origin_ids), collapse = ", ")

  where <- sprintf("WHERE origin_db IN (%s)", origin_csv)
  if (!is.null(dest_ids)) {
    dest_ids <- bit64::as.integer64(dest_ids)
    where <- paste0(
      where,
      sprintf(
        " AND dest_db IN (%s)",
        paste(as.character(dest_ids), collapse = ", ")
      )
    )
  }
  if (!is.null(max_time_s)) {
    where <- paste0(where, sprintf(" AND time_s <= %d", as.integer(max_time_s)))
  }

  sql <- sprintf(
    "SELECT origin_db::UBIGINT as origin_db, dest_db::UBIGINT as dest_db, time_s, dist_m
     FROM read_parquet(['%s'], hive_partitioning = false)
     %s",
    file_list,
    where
  )

  elapsed <- system.time(result <- DBI::dbGetQuery(con, sql))
  list(data = result, elapsed_sec = unname(elapsed["elapsed"]))
}
