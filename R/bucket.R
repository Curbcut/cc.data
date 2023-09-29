#' Write a folder to an AWS bucket
#'
#' Write a whole folder and its sub-folder to a pre-existing AWS bucket.
#' Environment variables (ID & KEY) to modify the buckets must be set beforehand, and
#' the user must have write access. As write requests are 10x more expensive than
#' read requests, each file is read from the bucket beforehand (if it exists) and if no
#' modification has been made to the file, it does not get written to the bucket.
#'
#' @param folder <`character`> The folder that should be entirely uploaded to
#' the bucket.
#' @param bucket <`character`> The name of the bucket in which the entire folder
#' should be uploaded.
#' @param prune <`character`|`logical`> Should local files not present in the
#' remote bucket be deleted? Defaults to "ask", which will prompt the user to
#' confirm in the case that files will be deleted, but can also be set to `TRUE`
#' or `FALSE` to respectively delete or not delete local files without a prompt.
#'
#' @return An error message or nothing if ran succesfully.
#' @export
bucket_write_folder <- function(folder, bucket, prune = "ask") {
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop(
      "Package \"aws.s3\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  # List of every files in the folder
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)

  # Get list of all existing files to not upload unmodified files
  all_objects <-
    aws.s3::get_bucket(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      bucket = bucket,
      max = Inf
    ) |>
    sapply(`[[`, "Key") |>
    unname()

  # If the hash exists in the bucket
  if ("hash.qs" %in% all_objects) {

    # Create a hash file of local files
    all_files <- list.files(folder, full.names = TRUE, recursive = TRUE)
    hash_file <- tibble::tibble(file = all_files)
    hash_file$hash_disk <- future.apply::future_sapply(hash_file$file, rlang::hash_file)

    # Grab the hash file in the bucket
    bucket_hash <- tempfile(fileext = "hash.qs")
    aws.s3::save_object(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      object = "hash.qs",
      bucket = bucket,
      file = bucket_hash
    ) |> suppressMessages()
    bucket_hash <- qs::qread(bucket_hash)
    names(bucket_hash)[2] <- "hash_bucket"

    # Are the files that are present in the bucket, and not locally? If so,
    # we ask if we want to delete them from the bucket.
    to_prune <- merge(bucket_hash, hash_file, all.x = TRUE)
    to_prune <- to_prune[is.na(to_prune$hash_disk), ]

    if (nrow(to_prune) > 0) {

      # Prompt with details about potential pruning
      if (prune == "ask") {
        prune_message <- to_prune$file[seq_len(min(nrow(to_prune), 15))]
        prune_message <- sub(folder, "", prune_message)
        prune_message <- sub("^/", "", prune_message)
        prune_message <- paste0("\u2219 ", prune_message, "\n")
        prune_message <- paste(prune_message, collapse = "")
        prune_message <- paste0(prettyNum(nrow(to_prune), big.mark = ","),
                                " files are present in the bucket but ",
                                "not in the local folder, including:\n",
                                prune_message,
                                "Should they be deleted from the bucket? (y/n)\n")
        cat(prune_message)
        prune_prompt <- readline()

        if (prune_prompt %in% c("y", "Y", "yes", "Yes", "YES")) {
          aws.s3::delete_object(object = gsub(sprintf("%s||%s/", folder, folder) , "", to_prune$file),
                                bucket = bucket)
          cat("Files succesfully removed. ")
        } else {
          cat("Files will not be deleted. ")
        }

      } else if (prune) {

        aws.s3::delete_object(object = gsub(sprintf("%s||%s/", folder, folder) , "", to_prune$file),
                              bucket = bucket)
        cat("Files present in the bucket but not locally were removed. ")

      } else if (!prune) {

        cat("Files present in the bucket but not locally were not removed. ")

      }

    }

    # Find objects which don't match between disk and bucket
    hash <- merge(hash_file, bucket_hash, all.x = TRUE)
    send_index <- mapply(identical, hash$hash_disk, hash$hash_bucket,
                         USE.NAMES = FALSE)
    send <- hash[!send_index, ]
    send$file <- gsub(paste0("^", folder, "/*"), "", send$file)
    hashed <- length(files) - nrow(send)
    to_send <- send$file

    # Report sending plan
    cat(prettyNum(hashed, big.mark = ","), " files unchanged. ",
        prettyNum(length(to_send), big.mark = ","),
        " files will be uploaded to the bucket\n", sep = "")

    sapply(to_send, \(file_path) {
      object_name <- gsub(paste0(".*", folder, "/"), "", file_path)
      aws.s3::put_object(
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
        file = file_path,
        object = object_name,
        bucket = bucket
      ) |> suppressMessages()
      return(invisible(NULL))
    })

  } else {

    # Report download plan
    cat("No hash file detected. ", prettyNum(length(files), big.mark = ","),
        " files will be uploaded to the bucket.\n", sep = "")

    sapply(files, \(file_path) {
      object_name <- gsub(paste0(".*", folder, "/"), "", file_path)
      aws.s3::put_object(
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
        file = file_path,
        object = object_name,
        bucket = bucket
      ) |> suppressMessages()
      return(invisible(NULL))
    })
  }

  # Create a hash file of all the local files
  all_files <- list.files(folder, full.names = TRUE, recursive = TRUE)
  hash_file <- tibble::tibble(file = all_files)
  hash_file$hash <- future.apply::future_sapply(hash_file$file, rlang::hash_file)

  hash_temp <- tempfile(fileext = ".qs")
  qs::qsave(hash_file, hash_temp)

  aws.s3::put_object(
    region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
    key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
    secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
    file = hash_temp,
    object = "hash.qs",
    bucket = bucket
  ) |> suppressMessages()

}


# -------------------------------------------------------------------------

#' Read a bucket from AWS
#'
#' Download a whole amazon bucket in a destination folder.
#'
#' @param destination_folder <`character`> The folder to which the bucket should
#' be downloaded.
#' @param bucket <`character`> The name of the bucket from which to download
#' the objects.
#' @param prune <`character`|`logical`> Should local files not present in the
#' remote bucket be deleted? Defaults to "ask", which will prompt the user to
#' confirm in the case that files will be deleted, but can also be set to `TRUE`
#' or `FALSE` to respectively delete or not delete local files without a prompt.
#' @param exclude <`character`> Files to not download from the bucket. Defaults
#' to NULL for none (download everything). Ignored if there is no file hash in
#' the source bucket.
#'
#' @return NULL if successful, and an error message if not.
#' @export

bucket_get_folder <- function(destination_folder, bucket, prune = "ask",
                              exclude = NULL) {

  # Argument check
  stopifnot(is.character(destination_folder))
  stopifnot(is.character(bucket))
  stopifnot(is.character(prune) || is.logical(prune))

  # Package check
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("\"aws.s3\" must be installed to use this function.", call. = FALSE)
  }

  # Credential check
  if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
    stop("You do not have Curbcut database user access.", call. = FALSE)
  }

  # Get list of all objects to download
  all_objects <-
    aws.s3::get_bucket(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      bucket = bucket,
      max = Inf
    ) |>
    sapply(`[[`, "Key") |>
    unname()

  # Exclude files
  to_download <- all_objects[!all_objects %in% exclude]
  excluded <- length(all_objects) - length(to_download)

  # If there is a hash file in the bucket, only download what is different from
  # the bucket content
  if ("hash.qs" %in% to_download) {

    # Grab the hash file in the bucket
    bucket_hash <- tempfile(fileext = "hash.qs")
    aws.s3::save_object(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      object = "hash.qs",
      bucket = bucket,
      file = bucket_hash
    ) |> suppressMessages()
    bucket_hash <- qs::qread(bucket_hash)
    names(bucket_hash)[2] <- "hash_bucket"

    # Create a hash file of existing files
    all_files <- list.files(destination_folder, recursive = TRUE)
    all_files <- file.path(destination_folder, all_files)
    hash_file <- tibble::tibble(file = all_files)
    hash_file$hash_disk <- future.apply::future_sapply(
      hash_file$file, rlang::hash_file)

    # If there are local files not present in the bucket, decide whether to
    # delete them
    to_prune <- merge(hash_file, bucket_hash, all.x = TRUE)
    to_prune <- to_prune[is.na(to_prune$hash_bucket),]

    if (nrow(to_prune) > 0) {

      # Prompt with details about potential pruning
      if (prune == "ask") {
        prune_message <- to_prune$file[seq_len(min(nrow(to_prune), 3))]
        prune_message <- sub(destination_folder, "", prune_message)
        prune_message <- sub("^/", "", prune_message)
        prune_message <- paste0("\u2219 ", prune_message, "\n")
        prune_message <- paste(prune_message, collapse = "")
        prune_message <- paste0(prettyNum(nrow(to_prune), big.mark = ","),
                                " files are present in the destination but ",
                                "not in the source bucket, including:\n",
                                prune_message,
                                "Should they be deleted? (y/n)\n")
        cat(prune_message)
        prune_prompt <- readline()

        if (prune_prompt %in% c("y", "Y", "yes", "Yes", "YES")) {
          removed <- file.remove(to_prune$file)
          cat(prettyNum(sum(removed), big.mark = ","), "files removed. ")
        } else {
          cat("Files will not be deleted. ")
        }

      } else if (prune) {

        removed <- file.remove(to_prune$file)
        cat(prettyNum(sum(removed), big.mark = ","),
            "files present in the destination but not in the source bucket",
            "were removed. ")

      } else if (!prune) {

        cat(prettyNum(nrow(to_prune), big.mark = ","),
            "files are present in the destination but not in the source bucket",
            "and were not removed. ")

      }

    }

    # Find objects which don't match between disk and bucket
    hash <- merge(bucket_hash, hash_file, all.x = TRUE)
    retrieve_index <- mapply(identical, hash$hash_bucket, hash$hash_disk,
                             USE.NAMES = FALSE)
    retrieve <- hash[!retrieve_index, ]
    retrieve$file <- gsub(paste0("^", destination_folder, "/*"), "",
                          retrieve$file)
    hashed <- length(to_download) - nrow(retrieve)
    to_download <- retrieve$file

    # Report download plan
    cat(prettyNum(excluded, big.mark = ","), " files excluded. ",
        prettyNum(hashed, big.mark = ","), " files unchanged. ",
        prettyNum(length(to_download), big.mark = ","),
        " files will be downloaded.\n", sep = "")

  } else {

    # Report download plan
    cat("No hash file detected. ", prettyNum(excluded, big.mark = ","),
        " files excluded. ", prettyNum(length(to_download), big.mark = ","),
        " files will be downloaded.\n", sep = "")

  }

  # Download the bucket and place it in the destination folder
  progressr::with_progress({
    pb <- progressr::progressor(length(to_download))
    out <- future.apply::future_sapply(to_download, \(object) {
      pb()
      aws.s3::save_object(
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
        object = object,
        bucket = bucket,
        file = file.path(destination_folder, object)
      ) |> suppressMessages()
    })
  })

  return(invisible(NULL))
}

# -------------------------------------------------------------------------

#' List bucket contents
#'
#' @param bucket <`character`> The name of the bucket from which to download
#' the object.
#'
#' @return Returns a df of objects in the bucket
#' @export

bucket_list_content <- function(bucket) {

  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop(
      "Package \"aws.s3\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  aws.s3::get_bucket_df(region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                        bucket = bucket)
}

#' Read single object from AWS bucket
#'
#' @param object <`character`> Name of the file with extension to retrieve.
#' @param objectext <`character`> Extension of the file to retrieve, e.g. `.csv`
#' or `.qs`.
#' @param bucket <`character`> The name of the bucket from which to download
#' the object.
#' @param method <`function`> A function used to read the object, e.g. if a `.csv`
#' then the method would be `utils::read.csv` or if a `.qs` would be `qs::qread`
#'
#' @return Returns the object read from the AWS bucket, and read using the
#' supplied `method`.
#' @export
bucket_read_object <- function(object, objectext, bucket, method) {

  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop(
      "Package \"aws.s3\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  tmp <- tempfile(fileext = objectext)
  aws.s3::save_object(region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                      object = object,
                      bucket = bucket,
                      file = tmp,
                      overwrite = TRUE)
  do.call(method, list(tmp))
}

#' Read a shapefile (.shp) from a zip file in an AWS S3 bucket
#'
#' This function downloads a zip file from an AWS S3 bucket, unzips it, and reads
#' the shapefile (.shp) contained in it. The function requires the \code{aws.s3}
#' package and the \code{sf} package.
#'
#' @param object <`character`> Name of the file with extension to retrieve.
#' @param bucket <`character`> The name of the bucket from which to download
#' the object.
#'
#' @return An \code{sf} object representing the shapefile.
#' @export
#'
#' @details This function assumes that the zip file in the bucket contains only
#' one shapefile. If there are multiple shapefiles, it will only read the first
#' one it finds.
bucket_read_object_zip_shp <- function(object, bucket) {

  # Get the file from the bucket
  file <- bucket_read_object(object = object,
                             bucket = bucket,
                             objectext = ".zip",
                             method = c)

  # Unzip, grab the shapefile name, and read it
  content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
  shp_file <- content[grepl("\\.shp", content)]
  utils::unzip(file, exdir = tempdir())
  sf::st_read(paste0(tempdir(), "\\", shp_file))

}
