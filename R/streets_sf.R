#' Build streets country-wide
#'
#' This function uses the \code{\link[cc.data]{rev_geocode_localhost}} function,
#' which means a local version of Nominatim must have been already set up and
#' needs to be running on port 8080. You can do so using
#' \code{\link[cc.data]{rev_geocode_create_local_nominatim}}.
#'
#' @param DA_processed_table <`sf data.frame`> A \code{DA} sf data.frame from
#' which the DA ID will be attached to every single building.
#' @param nb_batches <`numeric`> In how many batches should the sf that
#' are to be reverse geocoded through the local instance of Nominatim be divided?
#' To be gentle on memory.
#'
#' @return A street segment sf dataframe reverse geocoded.
#' @export
streets_sf <- function(DA_processed_table, nb_batches = 500) {

  # Download the most updated road network ----------------------------------

  road_network_links <-
    rvest::read_html(paste0("https://www12.statcan.gc.ca/census-recensement/",
                            "2021/geo/sip-pis/rnf-frr/index-eng.cfm")) |>
    rvest::html_elements(".list-group-item") |>
    (\(x) tibble::tibble(year = sub(" Archived", "", rvest::html_text(x)),
                         link = rvest::html_elements(x, "a") |>
                           rvest::html_attr("href")))()

  road_network_links <-
    road_network_links[order(road_network_links$year, decreasing = TRUE), ]
  year <- road_network_links[[1,1]]

  link <-
    paste0("http://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/",
           "files-fichiers/lrnf000r", sub("..", "", year), "a_e.zip")

  temp <- tempfile()
  temp2 <- tempfile()
  utils::download.file(link, temp)
  utils::unzip(zipfile = temp, exdir = temp2)
  streets <- sf::read_sf(temp2)


  # Construct segment name --------------------------------------------------

  # Split to spare the amount of data that is sent as batches
  street_batches <- suppressWarnings(split(streets, 1:nb_batches))

  # Construct
  pb <- progressr::progressor(steps = length(street_batches))
  streets <- future.apply::future_lapply(street_batches, \(bat) {

    # Ranges
    ranges <- sf::st_drop_geometry(bat)
    ranges <-
      ranges[
        , which(names(ranges) == "AFL_VAL"):which(names(ranges) == "ATR_VAL")]
    # Street name
    name <- bat$NAME
    # Street type
    type <- bat$TYPE
    # Direction
    direction <- bat$DIR

    bat$name <- sapply(seq_len(nrow(bat)), \(x) {

      r <- unlist(ranges[x, ])
      min_range <- suppressWarnings(min(r, na.rm = TRUE))
      max_range <- suppressWarnings(max(r, na.rm = TRUE))
      ranges2 <- if (all(!is.na(min_range), !is.na(max_range), !is.na(name[x])))
        paste(min_range, max_range, sep = "-") else ""

      name2 <- if (!is.na(name[x])) name[x] else ""

      type2 <- if (!is.na(name[x]) && !is.na(type[x]))
        stringr::str_to_sentence(type[x]) else ""

      direction2 <- if (!is.na(name[x]) && !is.na(direction[x]))
        direction[x] else ""

      out <- if (type2 %in% c("Rue", "Aut")) {
        paste(ranges2, type2, name2, direction2)
      } else if (type2 == "Hwy") {
        paste(type2, name2)
      } else {
        paste(ranges2, name2, type2, direction2)
      }

      out <- gsub("\\s{2}||\\s{3}", "", out)
      out <- gsub(" $", "", gsub("^ ", "", out))
      out <- if (out == "") NA else out

      if (is.na(out)) {
        centroids <- sf::st_centroid(bat$geometry[x])
        centroids <- sf::st_transform(centroids, 4326)
        out <- rev_geocode_localhost(centroids, street = TRUE)
      }

      if (grepl("^\\d*$", out)) out <- NA
      out

    }, USE.NAMES = FALSE)

    pb()
    bat[, c("OBJECTID", "name", "RANK", "geometry")]
  }, future.seed = NULL)

  streets <- data.table::rbindlist(streets)
  streets <- sf::st_as_sf(tibble::as_tibble(streets))

  # Add DA ID ---------------------------------------------------------------

  DA_table <- DA_processed_table[, "ID"]
  names(DA_table)[1] <- "DA_ID"

  streets_centroids <- suppressWarnings(sf::st_centroid(streets))
  da_joined <- sf::st_join(streets_centroids, DA_table)
  da_joined <- sf::st_drop_geometry(da_joined[, c("OBJECTID", "DA_ID")])
  streets <- merge(streets, da_joined, by = "OBJECTID")


  # Add DB ID for faster travel time matrice building -----------------------

  census_dataset <- gsub("^20", "CA",
                         cc.data::census_years[length(cc.data::census_years)])
  pr_codes <- cancensus::list_census_regions(census_dataset, quiet = TRUE)
  pr_codes <- pr_codes$region[pr_codes$level == "PR"]
  pr_codes <- lapply(pr_codes, \(x) list(PR = x))
  all_pr_vecs <- future.apply::future_lapply(pr_codes, \(reg) {
    cancensus::get_census(
      dataset = census_dataset,
      regions = reg,
      level = "DB",
      geo_format = "sf",
      quiet = TRUE
    )
  }, future.seed = NULL)
  DB <- lapply(all_pr_vecs, \(x) {
    names(x)[names(x) == "GeoUID"] <- "DB_ID"
    x <- x[, c("DB_ID", "geometry")]
    x
  })
  DB <- data.table::rbindlist(DB)
  DB <- sf::st_as_sf(tibble::as_tibble(DB))
  DB <- sf::st_transform(DB, 3347)

  db_joined <- sf::st_join(streets_centroids, DB)
  db_joined <- sf::st_drop_geometry(db_joined[, c("OBJECTID", "DB_ID")])
  streets <- merge(streets, db_joined, by = "OBJECTID")

  streets <- streets[order(streets$DA_ID), ]
  streets$ID <- paste0("s", seq_len(nrow(streets)))
  rownames(streets) <- NULL


  # Consolidate and clean output --------------------------------------------

  streets <- streets[, c("ID", "name", "DA_ID", "DB_ID", "OBJECTID", "RANK",
                         "geometry")]
  names(streets)[names(streets) == "OBJECTID"] <- "object_id"
  names(streets)[names(streets) == "RANK"] <- "rank"
  streets <- sf::st_cast(streets, "LINESTRING")
  streets <- sf::st_make_valid(streets)
  streets <- streets[!sf::st_is_empty(streets$geometry), ]
  streets <- sf::st_set_agr(streets, "constant")
  streets <- tibble::as_tibble(streets)
  streets <- sf::st_as_sf(streets)

  streets
}
