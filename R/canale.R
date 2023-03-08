build_canale <- function(DA_table, OSM_cache = TRUE) {

  # Workflow from CanALE User Guide
  # http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf

  # Intersection density measure --------------------------------------------

  # three_ways <- get_all_three_plus_ways(OSM_cache = OSM_cache)
  three_ways <- qs::qread("calculated_ignore/three_ways.qs")

  DA_centroids <- sf::st_centroid(DA_table)
  DA_buffer <- sf::st_buffer(DA_centroids, 1000)

  # How many 3+ way intersection per DA buffer
  intersects <- sf::st_intersects(DA_buffer, three_ways)
  DA_buffer$three_ways <- lengths(intersects)

  # Calculate the mean and standard deviation
  mean_data <- mean(DA_buffer$three_ways)
  sd_data <- sd(DA_buffer$three_ways)

  # Calculate the z-score for each data point
  DA_table$z_int_d <- (DA_buffer$three_ways - mean_data) / sd_data

  # Dwelling density measure ------------------------------------------------

  # Append number of dwellings to DA
  dwellings <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "DA",
    vectors = "v_CA21_4",
    geo_format = NA,
    quiet = TRUE
  )
  dwellings <- dwellings[c("GeoUID", "Dwellings")]
  names(dwellings) <- c("ID", "dwellings")
  dwellings <- merge(DA_table, dwellings, by = "ID")
  dwellings$area <- get_area(dwellings)
  dwellings <- sf::st_centroid(dwellings)

  # Calculate the dwelling density for each DA buffer
  intersects <- sf::st_intersects(DA_buffer, dwellings)
  dwelling_density <- mapply(\(ID, ind) {
    in_DA <- dwellings[ind, ]
    density <- sum(in_DA$dwellings) / sum(in_DA$area)
    tibble::tibble(ID = ID,
                   density = density)
  }, DA_buffer$ID, intersects, SIMPLIFY = FALSE)
  dwelling_density <- data.table::rbindlist(dwelling_density)
  dwelling_density <- tibble::as_tibble(dwelling_density)

  # Calculate the mean and standard deviation
  mean_data <- mean(dwelling_density$density, na.rm = TRUE)
  sd_data <- sd(dwelling_density$density, na.rm = TRUE)

  # Calculate the z-score for each data point
  DA_table$z_dwl_d <- (dwelling_density$density - mean_data) / sd_data
  DA_table$z_dwl_d[is.na(DA_table$z_dwl_d)] <- NA


  # Points of interest measure ----------------------------------------------

  # Load the points of interests
  poi <- read.csv("CANADA.txt")

  # Grab the major groups (from the SIC code)
  poi$sic_major_group <- stringr::str_extract(poi$SIC_1, "^\\d{2}")

  # Get a list of all the major group that we want to keep as amenities
  list_items <- rvest::read_html("https://www.osha.gov/data/sic-manual") |>
    rvest::html_elements("li")
  filtered_list_items <-
    list_items[stringr::str_detect(rvest::html_text(list_items), "Division")]

  divisions <- lapply(filtered_list_items, \(li) {
    # Extract the title of the <li>
    title <- rvest::html_text(rvest::html_element(li, "a"))
    # Extract the <ul> elements for the current <li>
    ul_elements <- rvest::html_elements(li, "a")
    ul_vec <- sapply(ul_elements, \(x) {
      stringr::str_extract(rvest::html_text(x), "(?<=Major Group )\\d{2}")
    })
    ul_vec <- ul_vec[!is.na(ul_vec)]
    # Return
    tibble::tibble(division = title,
                   major_groups = as.numeric(ul_vec))
  })
  divisions <- tibble::as_tibble(data.table::rbindlist(divisions))

  # Filter in only the amenities we are interested in
  amenities <- c("Division G: Retail Trade",
                 "Division H: Finance, Insurance, And Real Estate",
                 "Division I: Services")
  major_groups <- divisions$major_groups[divisions$division %in% amenities]
  # Manually add missing major groups
  major_groups <- c(major_groups, 41)
  poi <- poi[poi$sic_major_group %in% major_groups, ]

  # Make it as SF
  poi <- sf::st_as_sf(poi, coords = c("X", "Y"), crs = 4326)
  poi <- sf::st_transform(poi, crs = 3347)

  # Calculate the POI number for each DA buffer
  intersects <- sf::st_intersects(DA_buffer, poi)
  DA_buffer$poi <- lengths(intersects)

  # Calculate the mean and standard deviation
  mean_data <- mean(DA_buffer$poi, na.rm = TRUE)
  sd_data <- sd(DA_buffer$poi, na.rm = TRUE)

  # Calculate the z-score for each data point
  DA_table$z_poi <- (DA_buffer$poi - mean_data) / sd_data


  # Sum the three z index for the final score -------------------------------

  DA_table <- sf::st_drop_geometry(DA_table)
  DA_table[["canale_year"]] <-
    rowSums(DA_table[, c("z_dwl_d", "z_int_d", "z_poi")])


  # Return ------------------------------------------------------------------

  return(DA_table)
}

#' Get all three-plus way intersections in Canada
#'
#' This function returns all three-plus way intersections in Canada. It downloads
#' OpenStreetMap (OSM) data for Canada, retains footpaths, and excludes
#' limited-access highways and their entrances and exits. The OSM data is
#' transformed to the Stats Can CRS, and a grid is created to separate the work
#' charge to different cores. The function then calculates all the three-plus
#' way intersections in all the grids, and the streets that touch the lines of
#' the grid and potentially missed in the first step. Finally, it binds all the
#' three-plus way intersections and returns them.
#'
#' @param OSM_cache <`logical`> Indicates whether to use cached OSM data
#' or download it again. Default is TRUE.
#'
#' @return A data frame containing all the three-plus way intersections in
#' Canada.
#' @export
get_all_three_plus_ways <- function(year) {#, OSM_cache = TRUE) {
  # # Grab 2021 streets from OSM
  # osm_pbf <- "http://download.geofabrik.de/north-america/quebec-210101.osm.pbf"
  # streets <- osmextract::oe_read(osm_pbf, layer = "lines",
  #                              quiet = FALSE, force_download = !OSM_cache,
  #                               max_file_size = Inf)
  #
  # # Retain footbpaths and exclude limited-access highways and their entrances and
  # # exits.
  # fclasses <- c('trunk', 'primary', 'primary', 'secondary', 'tertiary',
  #               'unclassified', 'residential', 'pedestrian', 'living_street',
  #               'footway', 'steps', 'path')
  # streets <- streets[streets$highway %in% fclasses, ]
  # # Transform streets to the Stats Can CRS
  # streets <- sf::st_transform(streets, 3347)

  # Download the streets network --------------------------------------------

  url <- cc.data::road_network_url$url[cc.data::road_network_url$year == year]

  # download the zip file
  temp_zip_file <- tempfile()
  download.file(url, temp_zip_file)

  # extract contents to temporary folder
  temp_folder <- tempfile()

  # get the shapefile name
  listed_files <- unzip(temp_zip_file, list = TRUE)$Name
  shp <- if (year == 2001) {
    stringr::str_subset(listed_files, ".e00$")
  } else {
    listed_files <- unzip(temp_zip_file, list = TRUE)$Name
    stringr::str_subset(listed_files, ".shp$")
  }
  unzip(temp_zip_file, exdir = temp_folder)

  streets <- sf::st_read(paste0(temp_folder, "/", shp))
  streets <- sf::st_transform(streets, 3347)

  # Filter out highways -----------------------------------------------------

  if (year == 2001) {
    streets <- streets[streets$RANK1 == 0 & streets$RANK2 == 0, ]
  } else if (year == 2006) {
    streets <- streets[!streets$RANK %in% c("1", "2"), ]
  } else {
    streets <- streets[!streets$RANK %in% c("1", "2", "3"), ]
  }



  # Make a grid on all Canada to separate the work charge to different cores
  grid <- sf::st_make_grid(streets, n = c(500, 200))
  grid <- cbind(tibble::tibble(ID = seq_along(grid)), grid)
  grid <- sf::st_as_sf(tibble::as_tibble(grid))
  grid <- sf::st_filter(grid, streets)

  # Get all the three way intersections in all the grids
  progressr::with_progress({
    pb <- progressr::progressor(nrow(grid))
    three_ways <- future.apply::future_lapply(seq_len(nrow(grid)), \(x) {
      df <- sf:::`[.sf`(grid, x, )
      out <- nngeo::st_nn(df, streets, k = nrow(streets),
                          maxdist = 20000, progress = FALSE) |>
        suppressMessages()
      out <- sf:::`[.sf`(streets, out[[1]], )
      if (nrow(out) == 0) return(NULL)
      out <- sf::st_intersection(df, out)
      if (nrow(out) == 0) return(NULL)
      out <- sf::st_intersection(out)
      out <- out[sf::st_is(out, type = "POINT"), ]
      pb()
      unique(out["ID"])
    })
  })

  # Get all the three way intersections in streets that touch the lines of the
  # grid and that we potentially missed in the first step
  grid_lines <- sf::st_cast(grid, "MULTILINESTRING")
  streets_on_lines <- sf::st_filter(streets, grid_lines)
  streets_on_lines <- sf::st_intersection(streets_on_lines)
  streets_on_lines <-
    streets_on_lines[sf::st_is(streets_on_lines, type = "POINT"), ]
  streets_on_lines <- unique(streets_on_lines["osm_id"])
  names(streets_on_lines) <- c("ID", "geometry")

  # Bind all three+ way intersections
  three_ways <- rbind(Reduce(rbind, three_ways), streets_on_lines)
  three_ways <- three_ways["geometry"]
  three_ways <- unique(three_ways)
  three_ways$ID <- paste0("tw_", seq_along(three_ways$geometry))

  # Return
  return(three_ways)
}
