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
#' @param exclude <`character`> Files to not upload to the bucket. Defaults to
#' NULL for none.
#'
#' @return An error message or nothing if ran succesfully.
#' @export
bucket_write_folder <- function(folder, bucket, exclude = NULL) {
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
  # Exclude files to exclude
  files <- files[!files %in% exclude]

  # Get list of all existing files to not upload unmodified files
  existing_files <- unname(sapply(
    aws.s3::get_bucket(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      bucket = bucket
    ), `[[`, "Key"
  ))

  # Iterate over all files to upload them to the bucket
  out <- sapply(files, \(file_path) {
    object_name <- gsub(paste0(".*", folder, "/"), "", file_path)

    # Read the file from the bucket and compare it to the one on the local machine
    upload <- if (object_name %in% existing_files) {
      existing_char <-
        aws.s3::get_object(
          region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
          key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
          secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
          object = object_name,
          bucket = bucket,
          as = "raw"
        )
      f <- file(file_path, "rb")
      new_char <- readBin(f, "raw",
                          n = file.info(file_path)$size,
                          endian = "big"
      )
      close.connection(f)

      !identical(existing_char, new_char)
    } else {
      TRUE
    }

    # If the file does not already exist or is not the same as the one on the
    # bucket, upload.
    if (upload) {
      aws.s3::put_object(
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
        file = file_path,
        object = object_name,
        bucket = bucket
      ) |> suppressMessages()
    }

    return(invisible(NULL))
  })

  # Create a hash file
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

#' Read a bucket from AWS
#'
#' Download a whole amazon bucket in a destination folder.
#'
#' @param destination_folder <`character`> The folder in which the bucket should
#' be downloaded.
#' @param bucket <`character`> The name of the bucket from which to download
#' the objects.
#' @param exclude <`character`> Files to not download from the bucket. Defaults to
#' NULL for none (download everything).
#'
#' @return An error message or nothing if ran succesfully.
#' @export
bucket_get_folder <- function(destination_folder, bucket, exclude = NULL) {
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop(
      "Package \"aws.s3\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (Sys.getenv("CURBCUT_BUCKET_ACCESS_ID") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  # Update destination folder to accept the `/` already present at the end
  if (!grepl("/$", destination_folder)) {
    destination_folder <- paste0(destination_folder, "/")
  }

  # Get list of all objects to download
  all_objects <- unname(sapply(
    aws.s3::get_bucket(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      bucket = bucket
    ), `[[`, "Key"
  ))
  all_objects <- all_objects[!all_objects %in% exclude]

  # If there is a hash file in the bucket, only download what is different from the
  # bucket content
  if ("hash.qs" %in% all_objects) {
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
    all_files <- list.files(destination_folder, full.names = TRUE, recursive = TRUE)
    hash_file <- tibble::tibble(file = all_files)
    hash_file$hash_disk <- future.apply::future_sapply(hash_file$file, rlang::hash_file)

    # Which object isn't the same as what is on disk
    hash <- merge(bucket_hash, hash_file, all.x = TRUE)
    retrieve_index <- sapply(seq_along(hash$file), \(x) {
      identical(hash$hash_bucket[x], hash$hash_disk[x])
    })
    retrieve <- hash[!retrieve_index, ]

    retrieve$file <- gsub(destination_folder, "", retrieve$file)
    all_objects <- retrieve$file
  }

  # Download the bucket and place it in the destination folder
  progressr::with_progress({
    pb <- progressr::progressor(length(all_objects))
    out <- future.apply::future_sapply(all_objects, \(object) {
      pb()
      aws.s3::save_object(
        region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
        key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
        secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
        object = object,
        bucket = bucket,
        file = paste0(destination_folder, object)
      ) |> suppressMessages()
    })
  })

  return(invisible(NULL))
}

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
