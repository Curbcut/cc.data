# Function to scrape providers from a page
scrape_providers_from_page <- function(url) {
  page <- rvest::read_html(url)

  # Extract provider info from the table
  rows <- rvest::html_nodes(page, "table tbody tr")

  providers <- data.frame(
    provider_name = character(length(rows)),
    provider_url = character(length(rows)),
    location = character(length(rows)),
    location_url = character(length(rows)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(rows)) {
    row <- rows[[i]]

    provider_name_node <- rvest::html_node(row, "td:nth-child(1) a")
    providers$provider_name[i] <- rvest::html_text(provider_name_node)
    providers$provider_url[i] <- rvest::html_attr(provider_name_node, "href")

    location_node <- rvest::html_node(row, "td:nth-child(2)")
    providers$location[i] <- rvest::html_text(location_node) |>
      stringr::str_trim()

    url_node <- rvest::html_node(row, "td:nth-child(3) a")
    providers$location_url[i] <- rvest::html_attr(url_node, "href")
  }

  return(providers)
}

# Scrape all pages of providers
scrape_all_providers <- function() {
  base_url <- "https://transitfeeds.com/l/32-canada?p="
  all_providers <- data.frame(
    provider_name = character(),
    provider_url = character(),
    location = character(),
    location_url = character(),
    stringsAsFactors = FALSE
  )

  # Based on your HTML, there are 3 pages
  for (page_num in 1:3) {
    url <- paste0(base_url, page_num)
    cat("Scraping page:", url, "\n")

    page_providers <- scrape_providers_from_page(url)
    all_providers <- rbind(all_providers, page_providers)
  }

  return(all_providers)
}

# Function to scrape feeds for a provider
scrape_feeds_for_provider <- function(provider_url, provider_name) {
  cat("Scraping feeds for:", provider_name, "\n")

  # Read the provider page
  page <- rvest::read_html(provider_url)

  # Extract all feed links (list-group items)
  items <- rvest::html_nodes(page, ".list-group-item")

  feeds <- data.frame(
    provider = character(length(items)),
    feed_name = character(length(items)),
    feed_url = character(length(items)),
    feed_type = character(length(items)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(items)) {
    item <- items[[i]]

    feed_text <- trimws(rvest::html_text(item))
    feed_url <- rvest::html_attr(item, "href")
    badge_node <- rvest::html_node(item, ".badge")

    if (!is.na(badge_node) && !is.null(badge_node)) {
      feed_type <- rvest::html_text(badge_node)
      # Remove the feed type from the feed name (it's included in the text)
      feed_name <- trimws(gsub(feed_type, "", feed_text))
    } else {
      feed_type <- NA
      feed_name <- feed_text
    }

    feeds$provider[i] <- provider_name
    feeds$feed_name[i] <- feed_name
    feeds$feed_url[i] <- feed_url
    feeds$feed_type[i] <- feed_type
  }

  # Remove rows with NA feed_url (might be empty elements)
  feeds <- feeds[!is.na(feeds$feed_url), ]

  if (nrow(feeds) > 0) {
    # Convert relative URLs to absolute URLs
    feeds$feed_url <- paste0("https://transitfeeds.com", feeds$feed_url)
  }

  return(feeds)
}

# Scrape feeds for all providers
scrape_all_feeds <- function(providers) {
  all_feeds <- data.frame(
    provider = character(),
    feed_name = character(),
    feed_url = character(),
    feed_type = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(providers)) {
    provider_url <- providers$provider_url[i]
    provider_name <- providers$provider_name[i]

    provider_feeds <- scrape_feeds_for_provider(provider_url, provider_name)
    all_feeds <- rbind(all_feeds, provider_feeds)
  }

  return(all_feeds)
}

# Function to extract breadcrumb information
extract_breadcrumb <- function(page_content) {
  tryCatch(
    {
      # Extract breadcrumb links and text
      breadcrumb_items <- rvest::html_nodes(page_content, ".breadcrumb li")

      breadcrumb <- list(
        country = rvest::html_text(breadcrumb_items[4]),
        province_state = rvest::html_text(breadcrumb_items[5]),
        city = rvest::html_text(breadcrumb_items[6])
      )
    },
    error = function(e) {
      warning(sprintf("Error extracting breadcrumb: %s", e$message))
    }
  )

  return(breadcrumb)
}


`%||%` <- function(x, y) if (is.na(x) || is.null(x) || x == "") y else x

# Optimized function to scrape versions for all feeds using parallel processing
scrape_all_versions <- function(feeds) {
  # Filter to just GTFS feeds (not GTFS-RealTime)
  gtfs_feeds <- feeds[feeds$feed_type == "GTFS", ]

  # Define function to process a single feed
  process_feed <- function(i) {
    feed_url <- gtfs_feeds$feed_url[i]
    feed_name <- gtfs_feeds$feed_name[i]
    provider_name <- gtfs_feeds$provider[i]

    p() # Progress indicator

    page_num <- 1
    has_more_pages <- TRUE
    previous_version_urls <- character()
    breadcrumb_info <- NULL

    # Store all versions for this feed
    feed_versions_list <- list()

    while (has_more_pages) {
      # Construct URL with page number
      url <- if (page_num > 1) paste0(feed_url, "?p=", page_num) else feed_url

      # Read the feed page
      page_content <- tryCatch(
        {
          rvest::read_html(url)
        },
        error = function(e) {
          warning(sprintf("Error reading page %s: %s", url, e$message))
          return(NULL)
        }
      )

      if (is.null(page_content)) {
        has_more_pages <- FALSE
        next
      }

      # Extract breadcrumb info only once per feed
      if (is.null(breadcrumb_info)) {
        breadcrumb_info <- extract_breadcrumb(page_content)
        cat(sprintf("Processing feed: %s\n", feed_name))
      }

      # Extract version info from the table
      rows <- tryCatch(
        {
          rvest::html_nodes(page_content, "table tbody tr")
        },
        error = function(e) {
          message(sprintf("Error extracting rows: %s", e$message))
          return(list())
        }
      )

      if (length(rows) == 0) {
        has_more_pages <- FALSE
        next
      }

      # Extract headers for column identification
      headers <- tryCatch(
        {
          rvest::html_nodes(page_content, "table th") |>
            rvest::html_text() |>
            trimws()
        },
        error = function(e) {
          warning(sprintf("Error extracting headers: %s", e$message))
          return(character())
        }
      )

      # Find column indices
      date_idx <- which(headers == "Date")[1]
      size_idx <- which(headers == "Size")[1]
      routes_idx <- which(headers == "Routes")[1]
      download_idx <- which(headers == "Status")[1]

      # Process each row
      page_data <- data.frame(
        provider = character(length(rows)),
        feed = character(length(rows)),
        country = character(length(rows)),
        province_state = character(length(rows)),
        city = character(length(rows)),
        date = character(length(rows)),
        version_url = character(length(rows)),
        size = character(length(rows)),
        routes = character(length(rows)),
        download_url = character(length(rows)),
        stringsAsFactors = FALSE
      )

      for (j in seq_along(rows)) {
        row <- rows[[j]]

        # Fill in constant data for this feed
        page_data$provider[j] <- provider_name
        page_data$feed[j] <- feed_name
        page_data$country[j] <- breadcrumb_info$country %||% NA_character_
        page_data$province_state[j] <- breadcrumb_info$province_state %||%
          NA_character_
        page_data$city[j] <- breadcrumb_info$city %||% NA_character_

        # Extract version data
        if (!is.na(date_idx)) {
          date_node <- rvest::html_node(
            row,
            sprintf("td:nth-child(%d) a", date_idx)
          )
          if (!is.null(date_node)) {
            page_data$date[j] <- trimws(rvest::html_text(date_node))
            page_data$version_url[j] <- rvest::html_attr(date_node, "href")
          }
        }

        if (!is.na(size_idx)) {
          size_node <- rvest::html_node(
            row,
            sprintf("td:nth-child(%d)", size_idx)
          )
          if (!is.null(size_node)) {
            page_data$size[j] <- trimws(rvest::html_text(size_node))
          }
        }

        if (!is.na(routes_idx)) {
          routes_node <- rvest::html_node(
            row,
            sprintf("td:nth-child(%d) a", routes_idx)
          )
          if (!is.null(routes_node)) {
            page_data$routes[j] <- trimws(rvest::html_text(routes_node))
          }
        }

        if (!is.na(download_idx)) {
          download_node <- rvest::html_node(
            row,
            sprintf("td:nth-child(%d) a.btn-primary", download_idx)
          )
          if (!is.null(download_node)) {
            page_data$download_url[j] <- rvest::html_attr(download_node, "href")
          }
        }
      }

      # Check for duplicate pages
      current_version_urls <- page_data$version_url[
        !is.na(page_data$version_url) & page_data$version_url != ""
      ]

      if (length(current_version_urls) > 0) {
        if (
          length(previous_version_urls) > 0 &&
            length(current_version_urls) == length(previous_version_urls) &&
            all(current_version_urls == previous_version_urls)
        ) {
          message("  Detected duplicate page data, stopping pagination")
          has_more_pages <- FALSE
          next
        }
        previous_version_urls <- current_version_urls
      } else {
        has_more_pages <- FALSE
        next
      }

      # Add to list
      feed_versions_list[[length(feed_versions_list) + 1]] <- page_data

      # Check for next page
      has_more_pages <- tryCatch(
        {
          pagination_links <- rvest::html_nodes(
            page_content,
            ".pagination li a"
          )
          if (length(pagination_links) == 0) {
            return(FALSE)
          }

          hrefs <- rvest::html_attr(pagination_links, "href")
          page_matches <- regmatches(
            hrefs,
            regexpr("\\?p=(\\d+)$", hrefs, perl = TRUE)
          )

          if (length(page_matches) > 0) {
            page_numbers <- as.numeric(gsub("\\?p=", "", page_matches))
            any(page_numbers > page_num, na.rm = TRUE)
          } else {
            FALSE
          }
        },
        error = function(e) FALSE
      )

      page_num <- page_num + 1
    }

    # Combine all pages for this feed
    if (length(feed_versions_list) > 0) {
      all_feed_versions <- do.call(rbind, feed_versions_list)

      # Convert relative URLs to absolute URLs
      base_url <- "https://transitfeeds.com"

      version_mask <- !is.na(all_feed_versions$version_url) &
        all_feed_versions$version_url != "" &
        !startsWith(all_feed_versions$version_url, "http")
      all_feed_versions$version_url[version_mask] <- paste0(
        base_url,
        all_feed_versions$version_url[version_mask]
      )

      download_mask <- !is.na(all_feed_versions$download_url) &
        all_feed_versions$download_url != "" &
        !startsWith(all_feed_versions$download_url, "http")
      all_feed_versions$download_url[download_mask] <- paste0(
        base_url,
        all_feed_versions$download_url[download_mask]
      )

      return(all_feed_versions)
    } else {
      # Return empty data frame with correct structure
      return(data.frame(
        provider = character(0),
        feed = character(0),
        country = character(0),
        province_state = character(0),
        city = character(0),
        date = character(0),
        version_url = character(0),
        size = character(0),
        routes = character(0),
        download_url = character(0),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Process all feeds in parallel
  progressr::with_progress({
    p <- progressr::progressor(nrow(gtfs_feeds))
    results <- future.apply::future_lapply(
      1:nrow(gtfs_feeds),
      process_feed,
      future.seed = TRUE
    )
  })

  # Combine all results
  all_versions <- do.call(rbind, results)

  return(all_versions)
}

clean_name <- function(x) {
  # Normalize accents (é → e, à → a, ö → o, etc.)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

  # Replace any non-alphanumeric characters with "_"
  x <- gsub("[^a-zA-Z0-9]", "_", x)

  # Collapse multiple underscores and trim edges
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  x
}

from_version_to_s3key <- function(versions) {
  with(
    versions,
    paste(
      country,
      clean_name(province_state),
      clean_name(city),
      clean_name(provider),
      clean_name(feed),
      paste0(date, ".zip"),
      sep = "/"
    )
  )
}


download_feeds_to_s3 <- function(
  versions,
  bucket,
  chunk_size = 100,
  retry_attempts = 3
) {
  versions$s3_key <- from_version_to_s3key(versions)

  # Batch check for existing files - major performance improvement
  existing_keys <- tryCatch(
    {
      existing_objects <- aws.s3::get_bucket_df(
        bucket = bucket,
        prefix = "",
        max = Inf
      )$Key
      versions$s3_key %in% existing_objects
    },
    error = function(e) {
      warning(
        "Could not check existing files in batch, falling back to individual checks"
      )
      rep(FALSE, nrow(versions))
    }
  )

  # Filter to only new files
  to_download <- versions[!existing_keys, , drop = FALSE]

  if (nrow(to_download) == 0) {
    cat("All files already exist in S3\n")
    return(invisible(NULL))
  }

  cat(
    "Found",
    sum(existing_keys),
    "existing files,",
    nrow(to_download),
    "to download\n"
  )

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(to_download))

    # Process in chunks to avoid memory issues with large datasets
    results <- list()
    n_chunks <- ceiling(nrow(to_download) / chunk_size)

    for (chunk_i in seq_len(n_chunks)) {
      start_idx <- (chunk_i - 1) * chunk_size + 1
      end_idx <- min(chunk_i * chunk_size, nrow(to_download))
      chunk_data <- to_download[start_idx:end_idx, , drop = FALSE]

      chunk_results <- furrr::future_pmap(
        list(
          s3_key = chunk_data$s3_key,
          download_url = chunk_data$download_url
        ),
        function(s3_key, download_url) {
          success <- FALSE
          last_error <- NULL

          for (attempt in seq_len(retry_attempts)) {
            tryCatch(
              {
                # More robust HTTP handling
                response <- httr::GET(
                  download_url,
                  httr::write_memory()
                )

                # Check HTTP status
                if (httr::status_code(response) != 200) {
                  stop(
                    "HTTP ",
                    httr::status_code(response),
                    ": ",
                    httr::http_status(response)$message
                  )
                }

                # Get content and validate
                content_raw <- httr::content(response, "raw")
                if (length(content_raw) == 0) {
                  stop("Empty response from server")
                }

                # Upload to S3 with better error handling
                aws.s3::put_object(
                  file = rawConnection(content_raw),
                  object = s3_key,
                  bucket = bucket,
                  region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                  key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                  secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                  multipart = length(content_raw) > 5 * 1024^3 # Use multipart for files > 5MB
                )

                success <- TRUE
                break # Success, exit retry loop
              },
              error = function(e) {
                last_error <- e$message
                if (attempt < retry_attempts) {
                  Sys.sleep(2^attempt) # Exponential backoff
                }
              }
            )
          }

          p() # Update progress

          list(
            s3_key = s3_key,
            success = success,
            error = if (!success) last_error else NULL
          )
        },
        .options = furrr_options(seed = TRUE)
      )

      results <- c(results, chunk_results)
    }
  })

  # Report results
  successful <- sapply(results, function(x) x$success)
  failed <- results[!successful]

  cat("\nDownload complete:\n")
  cat("- Successful:", sum(successful), "\n")
  cat("- Failed:", length(failed), "\n")

  if (length(failed) > 0) {
    cat("\nFailed downloads:\n")
    for (fail in failed) {
      cat("- ", fail$s3_key, ": ", fail$error, "\n")
    }

    # TODO: Consider writing failed downloads to a file for retry later
    warning(length(failed), " downloads failed")
  }
}

# # Setup parallel processing
# future::plan(future::multisession)

# # Get all providers
# all_providers <- scrape_all_providers()
# qs::qsave(
#   all_providers,
#   "calculated_ignore/transitfeeds_legacy/01_all_providers.qs"
# )
# aws.s3::put_object(
#   file = "calculated_ignore/transitfeeds_legacy/01_all_providers.qs",
#   object = "CA/01_all_providers.qs",
#   bucket = "curbcut.gtfs",
#   region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
#   key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
#   secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
#   multipart = length(content_raw) > 5 * 1024^3 # Use multipart for files > 5MB
# )

# # Convert relative URLs to absolute URLs
# all_providers$provider_url <- paste0(
#   "https://transitfeeds.com",
#   all_providers$provider_url
# )

# # Get all feeds
# all_feeds <- scrape_all_feeds(all_providers)
# qs::qsave(
#   all_feeds,
#   "calculated_ignore/transitfeeds_legacy/02_all_feeds.qs"
# )
# aws.s3::put_object(
#   file = "calculated_ignore/transitfeeds_legacy/02_all_feeds.qs",
#   object = "CA/02_all_feeds.qs",
#   bucket = "curbcut.gtfs",
#   region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
#   key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
#   secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
#   multipart = length(content_raw) > 5 * 1024^3 # Use multipart for files > 5MB
# )

# # Get all versions with breadcrumb information
# all_versions <- scrape_all_versions(all_feeds)

# # Parse dates
# all_versions$date <- lubridate::dmy(all_versions$date)

# # Convert to tibble
# all_versions <- tibble::as_tibble(all_versions)

# # Save results
# all_versions$s3_key <- from_version_to_s3key(all_versions)
# data.table::fwrite(
#   all_versions,
#   "calculated_ignore/transitfeeds_legacy/03_transit_feed_versions_canada.csv"
# )
# aws.s3::put_object(
#   file = "calculated_ignore/transitfeeds_legacy/03_transit_feed_versions_canada.csv",
#   object = "CA/03_transit_feed_versions_canada.csv",
#   bucket = "curbcut.gtfs",
#   region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
#   key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
#   secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
#   multipart = length(content_raw) > 5 * 1024^3 # Use multipart for files > 5MB
# )

# future::plan(future::sequential())
# future::plan(future::multisession(), workers = 12)
# download_feeds_to_s3(all_versions, "curbcut.gtfs")
