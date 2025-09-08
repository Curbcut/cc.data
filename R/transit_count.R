# # Load libraries
# library(httr)
# library(jsonlite)
# library(gtfstools)
# library(fs)
# library(sf)
# library(tidyverse)
# library(data.table)

# # Download Canada-wide DB-level census geometry
# dbs <- cc.data::bucket_read_object_zip_shp("DB_shp_carto.zip", "curbcut.rawdata")

# # Download GTFS Feeds -----------------------------------------------------

# # NOTE: API may timeout for Canada-wide bbox – fallback to province
# access_token <- "eyJhbGciOiJSUzI1NiIsImtpZCI6ImE4ZGY2MmQzYTBhNDRlM2RmY2RjYWZjNmRhMTM4Mzc3NDU5ZjliMDEiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoiTWF4aW1lIELDqWxhbmdlciBEZSBCbG9pcyIsInBpY3R1cmUiOiJodHRwczovL2F2YXRhcnMuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3UvNjQ0ODM5MzI_dj00IiwiaXNzIjoiaHR0cHM6Ly9zZWN1cmV0b2tlbi5nb29nbGUuY29tL21vYmlsaXR5LWZlZWRzLXByb2QiLCJhdWQiOiJtb2JpbGl0eS1mZWVkcy1wcm9kIiwiYXV0aF90aW1lIjoxNzM5NTQyMDk2LCJ1c2VyX2lkIjoiQWhHcWE3ZFhqeVZ0MTl0ZnFTb1pTODg5NVhKMyIsInN1YiI6IkFoR3FhN2RYanlWdDE5dGZxU29aUzg4OTVYSjMiLCJpYXQiOjE3NTI2MDc4MjUsImV4cCI6MTc1MjYxMTQyNSwiZW1haWwiOiJtYXhpbWUuYmVsYW5nZXJkZWJsb2lzQG1jZ2lsbC5jYSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJmaXJlYmFzZSI6eyJpZGVudGl0aWVzIjp7ImdpdGh1Yi5jb20iOlsiNjQ0ODM5MzIiXSwiZW1haWwiOlsibWF4aW1lLmJlbGFuZ2VyZGVibG9pc0BtY2dpbGwuY2EiXX0sInNpZ25faW5fcHJvdmlkZXIiOiJnaXRodWIuY29tIn19.J9qhDhQfQD9TfJIcWggY9EiIFoQQ4ufSI47ExJ2EHrQtpxZc2UwMi3zrsaLozmqtZBD1DQ2KS5BF1lkWHL3CSJPfJcs-nKmltgGBz9RE3XXRTmYo0jntEdeuExVSeZ9Mer5HGqP_txdNS_WvS-ptMzy020I8J-jtG6Q2njvhuyn-Q1gvQVzVeFo740oZn341h0vk8iO_KAiMF_rbPYfxazq_fZsEm5OEcaZvn1geZQ2odG0XM3_lPRF02tMHWBE1IkPE52FLQACfLbfQXILV8YFDik6O7hzckz2VGVCy1Ki3jPQ-NWqgmMIokXVY-DfN54o3IZGH-Bey8y593tyBEg"

# api_url <- "https://api.mobilitydatabase.org/v1/gtfs_feeds?country_code=CA"

# response <- httr::GET(
#   url = api_url,
#   httr::add_headers(
#     Authorization = paste("Bearer", access_token),
#     `User-Agent` = "R-script"  # optional but can prevent some 403s
#   ),
#   httr::config(followlocation = TRUE)
# )
# feeds <- content(response)

# # Save directory
# save_directory <- "gtfs_data"
# dir_create(save_directory)

# # Download GTFS feeds to disk
# download_gtfs_files <- function(feeds, save_directory) {
#   gtfs_paths <- list()
#   for (i in seq_along(feeds)) {
#     feed <- feeds[[i]]
#     if (!is.null(feed$latest_dataset$hosted_url)) {
#       file_path <- file.path(save_directory, paste0("gtfs_", i, ".zip"))
#       resp <- GET(feed$latest_dataset$hosted_url, write_disk(file_path, overwrite = TRUE))
#       if (status_code(resp) == 200) gtfs_paths[[length(gtfs_paths) + 1]] <- file_path
#     }
#   }
#   gtfs_paths
# }

# # Clean downloaded ZIPs
# clean_gtfs_files <- function(gtfs_paths) {
#   cleaned <- list()
#   for (path in gtfs_paths) {
#     tmp_dir <- tempfile()
#     unzip(path, exdir = tmp_dir)
#     file_delete(dir_ls(tmp_dir, recurse = TRUE, regexp = "__MACOSX|\\.pdf$"))
#     new_zip <- sub("\\.zip$", "_cleaned.zip", path)
#     zip::zipr(new_zip, dir_ls(tmp_dir, recurse = TRUE))
#     cleaned[[length(cleaned) + 1]] <- new_zip
#   }
#   cleaned
# }

# # Load GTFS
# load_gtfs_files <- function(paths) {
#   lapply(paths, function(p) tryCatch(read_gtfs(p), error = function(e) NULL)) |> compact()
# }

# # Merge all GTFS
# merge_all_gtfs <- function(gtfs_list) {
#   if (length(gtfs_list) > 1) merge_gtfs(gtfs_list, prefix = TRUE)
#   else if (length(gtfs_list) == 1) gtfs_list[[1]]
#   else stop("No GTFS files loaded.")
# }

# # Process GTFS
# # gtfs_paths <- download_gtfs_files(feeds, save_directory)
# # cleaned_paths <- clean_gtfs_files(gtfs_paths)
# cleaned_paths <- list.files(save_directory, full.name = TRUE)[grepl("cleaned", list.files(save_directory))]
# gtfs_list <- load_gtfs_files(cleaned_paths)
# merged_gtfs <- merge_all_gtfs(gtfs_list)
# merged_gtfs <- remove_duplicates(merged_gtfs)
# merged_gtfs$routes$route_type <- as.integer(merged_gtfs$routes$route_type)


# # Define route types
# route_types <- list(
#   tram = c(0),           # Tram/Light Rail
#   subway = c(1),         # Subway/Metro
#   rail = c(2),           # Rail/Commuter
#   bus = c(3, 700)        # Bus (local + regional)
# )

# # Filter GTFS by route type
# gtfs_by_type <- sapply(route_types, function(rt) {
#   filter_by_route_type(merged_gtfs, rt)
# }, simplify = FALSE)

# # Process for each mode ---------------------------------------------------
# out_gtfs <- lapply(gtfs_by_type, function(gtfs) {
#   # Convert stops to sf (unchanged)
#   stops_sf <- gtfs$stops |>
#     filter(!is.na(stop_lat), !is.na(stop_lon)) |>
#     st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> 
#     st_transform(crs = st_crs(dbs))

#   # Spatial join to DBs (unchanged)
#   intersected <- st_intersects(stops_sf, dbs)
#   stops_sf$DBUID <- sapply(intersected, function(x) {
#     if (length(x) == 0) NA_character_ else dbs$DBUID[x[1]]
#   })

#   # Count stops per DB (unchanged - this is independent of time)
#   access_stops <- tibble(DBUID = names(table(na.omit(stops_sf$DBUID))),
#                         stops = as.vector(unname(table(na.omit(stops_sf$DBUID)))))

#   # Define time periods to process
#   setDT(gtfs$calendar)
#   date <- as.Date("2025-06-11")
  
#   time_periods <- list(
#     daily = gtfs$calendar[start_date <= date & end_date >= date & wednesday == 1, service_id],
#     weekend = gtfs$calendar[start_date <= date & end_date >= date & saturday == 1, service_id]
#   )
  
#   # Process each time period
#   time_results <- lapply(names(time_periods), function(period_name) {
#     service_ids <- time_periods[[period_name]]
    
#     # Filter trips and stop_times for this period
#     trips_service <- gtfs$trips[gtfs$trips$service_id %in% service_ids, ]
#     stop_times <- gtfs$stop_times[gtfs$stop_times$trip_id %in% trips_service$trip_id, ]
#     setDT(stop_times)
    
#     # Join with stop DBUIDs
#     stop_geo <- stops_sf |> st_drop_geometry() |> as_tibble() |> select(stop_id, DBUID)
#     setDT(stop_geo)
#     stop_times <- merge(stop_times, stop_geo, by = "stop_id")
    
#     # For weekdays, also process peak hours
#     if (period_name == "daily") {
#       # Extract hour from arrival_time (handle >24h format)
#       stop_times[, hour := as.numeric(substr(arrival_time, 1, 2)) %% 24]
      
#       # Split into daily and peak
#       daily_times <- stop_times
#       peak_times <- stop_times[hour >= 7 & hour < 9]
      
#       time_splits <- list(daily = daily_times, peak = peak_times)
#     } else {
#       time_splits <- list(weekend = stop_times)
#     }
    
#     # Process each time split
#     lapply(names(time_splits), function(split_name) {
#       times_data <- time_splits[[split_name]]
      
#       # Join with route info
#       trips_dt <- as.data.table(trips_service)[, .(trip_id, route_id)]
#       trips <- merge(times_data, trips_dt, by = "trip_id")
      
#       # Count routes and trips per DB
#       routes <- trips[, .(routes = uniqueN(route_id)), by = DBUID]
#       vehicles <- trips[, .(trips = uniqueN(trip_id)), by = DBUID]
      
#       # Create suffix for column names
#       suffix <- if (period_name == "daily") split_name else period_name
      
#       # Rename columns with suffix
#       setnames(routes, "routes", paste0("routes_", suffix))
#       setnames(vehicles, "trips", paste0("trips_", suffix))
      
#       # Return merged results
#       merge(routes, vehicles, by = "DBUID", all = TRUE)
#     })
#   })
  
#   # Flatten and merge all time period results
#   all_time_results <- unlist(time_results, recursive = FALSE)
#   combined_time <- Reduce(function(x, y) merge(x, y, by = "DBUID", all = TRUE), all_time_results)
  
#   # Merge with stops data
#   final_result <- merge(access_stops, combined_time, by = "DBUID", all = TRUE)
#   replace_na(final_result, list(stops = 0))
# })

# # Add suffix and combine
# processed <- lapply(names(out_gtfs), function(mode) {
#   rename_with(out_gtfs[[mode]], ~ paste0(mode, "_", .), c("stops", "routes", "trips"))
# })

# out_combined <- reduce(processed, full_join, by = "DBUID")
# out_combined <- left_join(dbs["DBUID"], out_combined, by = "DBUID")
# out_combined <- out_combined |> 
#   mutate(across(where(is.numeric), ~replace_na(.x, 0)))
# out_combined <- tibble::as_tibble(out_combined)

# # Look only Montreal
# mtl <- cancensus::get_census("CA21", regions = list(CSD = 2466023), geo_format = "sf")
# mtl <- sf::st_transform(mtl, crs = sf::st_crs(dbs))
# ints <- sf::st_intersects(mtl, dbs)

# db_mtl <- out_combined[out_combined$DBUID %in% dbs[ints[[1]],]$DBUID, ]
# sf::st_as_sf(db_mtl)["bus_trips"] |> .mv()
