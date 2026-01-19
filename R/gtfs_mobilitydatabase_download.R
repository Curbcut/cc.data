# # REFRESH THE MOBILITY DATABASE TOKEN
# refresh_token <- "" # https://mobilitydatabase.org/

# # Define API endpoint and refresh token
# auth_url <- "https://api.mobilitydatabase.org/v1/tokens"

# # Make the POST request
# auth_response <- httr::POST(
#   auth_url,
#   httr::add_headers(`Content-Type` = "application/json"),
#   body = jsonlite::toJSON(
#     list(refresh_token = refresh_token),
#     auto_unbox = TRUE
#   ),
#   encode = "json"
# )

# # Check response and extract access token
# if (httr::status_code(auth_response) == 200) {
#   auth_content <- httr::content(
#     auth_response,
#     as = "parsed",
#     type = "application/json"
#   )
#   access_token <- auth_content$access_token
#   print(paste("Access Token:", access_token))
# } else {
#   stop("Failed to obtain access token: ", content(auth_response, as = "text"))
# }

# api_url <- "https://api.mobilitydatabase.org/v1/gtfs_feeds?country_code=CA"

# # Make the GET request using the access token
# response <- httr::GET(
#   api_url,
#   httr::add_headers(Authorization = paste("Bearer", access_token))
# )

# feeds <- httr::content(response)

# Create S3 key with mobilitydatabase prefix
create_mobilitydatabase_s3_key <- function(
  country,
  subdivision,
  municipality,
  provider,
  dataset_id
) {
  paste(
    "mobilitydatabase",
    country,
    clean_name(subdivision),
    clean_name(municipality),
    clean_name(provider),
    paste0(dataset_id, ".zip"),
    sep = "/"
  )
}

# Simplified main function
download_mobilitydatabase_gtfs <- function(
  country_code = "CA",
  access_token = Sys.getenv("MOBILITY_DATABASE_TOKEN"),
  bucket = "curbcut.gtfs"
) {
  # Get all feeds
  cat("Fetching feeds...\n")
  api_url <- sprintf(
    "https://api.mobilitydatabase.org/v1/gtfs_feeds?country_code=%s&data_type=gtfs",
    country_code
  )

  response <- httr::GET(
    api_url,
    httr::add_headers(Authorization = paste("Bearer", access_token))
  )

  feeds <- httr::content(response)
  cat(sprintf("Found %d feeds\n", length(feeds)))

  # Get versions for each feed
  cat("Fetching versions...\n")
  progressr::with_progress({
    p <- progressr::progressor(length(feeds))

    all_versions <- future.apply::future_lapply(
      feeds,
      function(feed) {
        p()

        # Get versions
        versions_url <- sprintf(
          "https://api.mobilitydatabase.org/v1/gtfs_feeds/%s/datasets",
          feed$id
        )
        versions_response <- httr::GET(
          versions_url,
          httr::add_headers(Authorization = paste("Bearer", access_token))
        )

        if (httr::status_code(versions_response) != 200) {
          return(NULL)
        }

        versions <- httr::content(versions_response)
        if (length(versions) == 0) {
          return(NULL)
        }

        # Extract location info
        location <- if (length(feed$locations) > 0) {
          feed$locations[[1]]
        } else {
          list()
        }

        # Build data frame
        data.frame(
          provider = feed$provider %||% NA_character_,
          country = location$country_code %||% NA_character_,
          subdivision = location$subdivision_name %||% NA_character_,
          municipality = location$municipality %||% NA_character_,
          dataset_id = vapply(versions, `[[`, character(1), "id"),
          downloaded_at = vapply(versions, `[[`, character(1), "downloaded_at"),
          hosted_url = vapply(versions, `[[`, character(1), "hosted_url"),
          stringsAsFactors = FALSE
        )
      },
      future.seed = TRUE
    )
  })

  all_versions <- do.call(rbind, Filter(Negate(is.null), all_versions))
  all_versions <- all_versions[all_versions$country == "CA", ]

  # Create S3 keys
  all_versions$s3_key <- mapply(
    create_mobilitydatabase_s3_key,
    all_versions$country,
    all_versions$subdivision,
    all_versions$municipality,
    all_versions$provider,
    all_versions$dataset_id,
    USE.NAMES = FALSE
  )

  cat(sprintf("Found %d versions\n", nrow(all_versions)))

  # Check what exists in S3
  cat("Checking S3...\n")
  existing <- tryCatch(
    {
      existing_objects <- aws.s3::get_bucket_df(
        bucket = bucket,
        prefix = "mobilitydatabase/",
        max = Inf
      )$Key
      all_versions$s3_key %in% existing_objects
    },
    error = function(e) rep(FALSE, nrow(all_versions))
  )

  to_download <- all_versions[!existing, , drop = FALSE]
  cat(sprintf(
    "%d already exist, %d to download\n",
    sum(existing),
    nrow(to_download)
  ))

  if (nrow(to_download) == 0) {
    return(invisible(all_versions))
  }

  # Download to S3
  cat("Downloading...\n")
  progressr::with_progress({
    p <- progressr::progressor(nrow(to_download))

    future.apply::future_mapply(
      function(s3_key, hosted_url) {
        tryCatch(
          {
            content_raw <- httr::content(httr::GET(hosted_url), "raw")

            aws.s3::put_object(
              file = rawConnection(content_raw),
              object = s3_key,
              bucket = bucket,
              region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
              key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
              secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY")
            )
            p()
            TRUE
          },
          error = function(e) {
            p()
            warning(sprintf("Failed %s: %s", s3_key, e$message))
            FALSE
          }
        )
      },
      to_download$s3_key,
      to_download$hosted_url,
      USE.NAMES = FALSE,
      future.seed = TRUE
    )
  })

  # Save metadata
  data.table::fwrite(
    all_versions,
    sprintf("calculated_ignore/mobilitydatabase_versions_%s.csv", country_code)
  )

  aws.s3::put_object(
    file = "calculated_ignore/mobilitydatabase_versions_CA.csv",
    object = "mobilitydatabase/CA/03_mobility_feed_versions_canada.csv",
    bucket = "curbcut.gtfs",
    region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
    key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
    secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
    multipart = length(all_versions) > 5 * 1024^3 # Use multipart for files > 5MB
  )

  return(all_versions)
}

# download_mobilitydatabase_gtfs()

# all_versions <- bucket_read_object(
#   "mobilitydatabase/CA/03_mobility_feed_versions_canada.csv",
#   "csv",
#   "curbcut.gtfs",
#   readr::read_csv
# )

# HERE I AM EXTRACTING ALL FEED METADATA SO I CAN THEN RUN FEED COVERAGE
# AND PROPAGATE BETWEEN TRANSITFEEDS AND MOBILITYDATABASE. IT'S GTFS FEEDS,
# JUST FROM DIFFERENT SOURCES. BUT THEY ARE THE SAME WITHIN! IT IS LIKELY
# THAT BOTH 'ALL_METADATA' CAN JUST BE MERGED!
