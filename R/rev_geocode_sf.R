#' Reverse geocode spatial features
#'
#' This geocoding works in two steps. The province database of addresses is
#' first downloaded from the National Open Database of Addresses, with URLs to
#' download the zip files available in \code{\link[cc.data]{buildings_osm_ms_keys}}.
#' Second, we reverse geocode using the OSM service. The reverse geocoding is done
#' using the centroid of every spatial feature.
#'
#' @param prov_folder <`character`> Where the sf data.frames with spatial features
#' for each province live (`.qs` files).
#' @param nb_batches <`numeric`> In how many batches should the sf that
#' are to be reverse geocoded through the local instance of Nominatim be divided?
#' Useful to have progress bar information.
#' @param nb_sf_process <`numeric`> Once the sf dataframe has been split up
#' in x number of batches, how many spatial features should be in each of the
#' apply iterations? By default, each data.frame going to each process will
#' be roughly 400 rows. Smaller dataframes help to not overload memory (every apply
#' iteration only has to hold a 400 sf data.frame before getting flushed and
#' restart with another data.frame). There must be a compromise between 1. not
#' too heavy on memory and 2. limit overhead of each iteration.
#'
#' @return An `sf` data.frame combining all the `.qs` files in `prov_folder` with
#' all spatial features reverse geocoded.
#' @export
rev_geocode_building <- function(prov_folder, nb_batches = 100, nb_sf_process = 400) {

  if (!grepl("/$", prov_folder)) prov_folder <- paste0(prov_folder, "/")

  # Download each province and match with National Open Database of  --------
  pb <- progressr::progressor(steps = nrow(cc.data::buildings_osm_ms_keys))
  provs <-
    future.apply::future_lapply(
      seq_len(nrow(cc.data::buildings_osm_ms_keys)), \(table_n) {

        # Download and load the addresses -----------------------------------------

        url <- cc.data::buildings_osm_ms_keys$addresses[table_n]
        if (!is.na(url)) {
          tmp <- tempfile(pattern = cc.data::buildings_osm_ms_keys$ms_code[table_n],
                          fileext = ".zip")
          utils::download.file(url, destfile = tmp)
          all_files <- utils::unzip(tmp, list = TRUE)
          csv_to_extract <- all_files$Name[all_files$Length == max(all_files$Length)]
          connection_to_csv <- unz(tmp, csv_to_extract)
          addresses_raw <- utils::read.csv(connection_to_csv)


          # Addresses as sf, and spatially filtered ---------------------------------

          addresses <- sf::st_as_sf(addresses_raw,
                                    coords = c("longitude", "latitude"),
                                    crs = 4326)
          addresses <- sf::st_transform(addresses, crs = 3347)


          # Get sfs centroid --------------------------------------------------------

          sf_df <- qs::qread(paste0(prov_folder, "/",
                                    cc.data::buildings_osm_ms_keys$ms_code[table_n],
                                    ".qs"))

          sf_df_centroids <- suppressWarnings(sf::st_centroid(sf_df))


          # Get closest address for each sf -----------------------------------------

          nearest <- suppressMessages(nngeo::st_nn(sf_df_centroids, addresses,
                                                   maxdist = 25))
          nearest[sapply(nearest, length) == 0] <- NA_integer_
          nearest <- unlist(nearest)
          sf_df_centroids$name <-
            paste(stringr::str_to_title(tolower(addresses$full_addr[nearest])),
                  addresses$csdname[nearest],
                  sep = ", "
            )
          sf_df_centroids$name[sapply(nearest, is.na)] <- NA_character_


          # Bind to raw sf df -------------------------------------------------------

          out <-
            merge(
              sf_df[, names(sf_df)[names(sf_df) != "name"]],
              sf::st_drop_geometry(sf_df_centroids[, c("ID", "name")], by = "ID")
            )
          out_reordered <-
            out[, c("ID", "name", names(out)[!names(out) %in% c("ID", "name")])]
          out_reordered <- tibble::as_tibble(out_reordered)
          out <- sf::st_as_sf(out_reordered)

        } else {


          # Download qs -------------------------------------------------------------

          out <- qs::qread(paste0(prov_folder, "/",
                                  cc.data::buildings_osm_ms_keys$ms_code[table_n],
                                  ".qs"))

        }


        # Return ------------------------------------------------------------------
        pb()
        return(out)

      }, future.seed = NULL)


  # Reverse geocode the rest using local Nominatim instance -----------------

  nat <- data.table::rbindlist(provs)
  nat <- tibble::as_tibble(nat)
  nat <- sf::st_as_sf(nat)

  # Keep only missing names
  nat_missing_centroids <- nat[is.na(nat$name), ]

  # Centroids
  nat_missing_centroids <- suppressWarnings(sf::st_centroid(nat_missing_centroids))
  nat_missing_centroids <- sf::st_transform(nat_missing_centroids, crs = 4326)

  # Split to spare the amount of data that is sent as batches
  nat_missing_centroids <- split(nat_missing_centroids, 1:nb_batches) |>
    suppressWarnings()

  # Subbatches
  nat_missing_centroids <-
    lapply(nat_missing_centroids, \(x) suppressWarnings(
      split(x, 1:ceiling(nrow(nat_missing_centroids[[1]])/nb_sf_process))))


  # Local Nominatim instance ------------------------------------------------

  # Free up some memory first
  rm(provs)
  # Save national geometries for later. Only keep centroids on memory
  tmp_nat <- tempfile(fileext = "qs")
  qs::qsave(nat, tmp_nat)
  rm(nat)

  pb <- progressr::progressor(steps = length(nat_missing_centroids))
  rev_geocoded <-
    lapply(nat_missing_centroids, \(batch) {

      # Parallelization in the batch
      out <- future.apply::future_lapply(batch, \(df) {
        df$name <- sapply(seq_len(nrow(df)), \(x) {
          rev_geocode_localhost(df$geometry[x])
        }, simplify = TRUE, USE.NAMES = FALSE)
        df
      }, future.seed = NULL)

      pb()

      return(out)
    })

  # Free memory
  rm(nat_missing_centroids)

  # Bind and drop centroid geometry
  rev_geocoded <- lapply(rev_geocoded, data.table::rbindlist)
  rev_geocoded <- data.table::rbindlist(rev_geocoded)
  rev_geocoded <- tibble::as_tibble(rev_geocoded)
  rev_geocoded <- rev_geocoded[, names(rev_geocoded) != "geometry"]


  # Get back the temp file and merge the names ------------------------------

  nat <- qs::qread(tmp_nat)

  new_names <- merge(rev_geocoded, nat[, c("ID", "geometry")], by = "ID",
                     all.x = TRUE, all.y = FALSE)
  new_names <- sf::st_as_sf(new_names)
  old_names <- nat[!nat$ID %in% new_names$ID, ]
  nat <- rbind(old_names, new_names)

  nat <- tibble::as_tibble(nat)
  nat <- sf::st_as_sf(nat)
  nat <- nat[order(nat$ID), ]
  nat$ID <- paste0("b", seq_along(nat$ID))


  # Return ------------------------------------------------------------------

  return(nat)

}
