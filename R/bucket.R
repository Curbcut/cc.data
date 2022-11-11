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

      !all(existing_char == new_char)
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
        object = file_path,
        bucket = bucket
      ) |> suppressMessages()
    }

    return(invisible(NULL))
  })
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

  # Download the bucket and place it in the destination folder
  out <- sapply(all_objects, \(object) {
    print(object)
    aws.s3::save_object(
      region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
      object = object,
      bucket = bucket,
      file = paste(destination_folder, object)
    ) |> suppressMessages()
  })

  return(invisible(NULL))
}
