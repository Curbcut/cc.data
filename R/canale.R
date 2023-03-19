#' Build CanALE index for a given year
#'
#' This function builds the Canadian Active Living Environment (CanALE) index
#' for a given year based on three measures: intersection density, dwelling
#' density, and points of interest (POI) density.
#'
#' @param year <`numeric`> indicating the year of the streets network to use.
#' Options are census years starting from 2001.
#' @param DA_table <`sf data.frame`> An \code{sf} object representing the
#' dissemination areas (DAs) to calculate the CanALE index for.
#'
#' @details Workflow from CanALE User Guide http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf
#'
#' @return A data.frame object representing the CanALE index for the given year
#' and DAs.
#' @export
build_canale <- function(year, DA_table) {

  # Intersection density measure --------------------------------------------

  three_ways <- get_all_three_plus_ways(year = year)

  DA_buffer <- sf::st_buffer(DA_table, 1000)

  # How many 3+ way intersection per DA buffer
  intersects <- sf::st_intersects(DA_buffer, three_ways)
  DA_buffer$three_ways <- lengths(intersects)

  # Calculate the mean and standard deviation
  mean_data <- mean(DA_buffer$three_ways)
  sd_data <- stats::sd(DA_buffer$three_ways)

  # Calculate the z-score for each data point
  DA_table$z_int_d <- (DA_buffer$three_ways - mean_data) / sd_data


  # Dwelling density measure ------------------------------------------------

  dwell_vecs <- c(CA21 = "v_CA21_4", CA16 = "v_CA16_404", CA11 = "v_CA11F_2",
                  CA06 = "v_CA06_98", CA01 = "v_CA01_96")

  cancensus_dataset <- paste0("CA", gsub("^20", "", year))

  # Get dwellings information from the census
  dwellings <- tryCatch({cancensus::get_census(
    dataset = cancensus_dataset,
    regions = list(C = "01"),
    level = "DA",
    vectors = c(dwellings = unname(dwell_vecs[cancensus_dataset])),
    geo_format = "sf",
    quiet = TRUE
  )}, error = function(e) {
    regions <- cancensus::list_census_regions(cancensus_dataset)
    regions <- regions$region[regions$level == "PR"]
    regions <- lapply(regions, \(r) {
      cancensus::get_census(
        dataset = cancensus_dataset,
        regions = list(PR = r),
        level = "DA",
        vectors = c(dwellings = unname(dwell_vecs[cancensus_dataset])),
        geo_format = "sf",
        quiet = TRUE
      )
    })
    regions <- lapply(regions, `[`, c("GeoUID", "dwellings", "geometry"))
    return(Reduce(rbind, regions))
  })

  dwellings <- sf::st_transform(dwellings, 3347)
  dwellings <- sf::st_make_valid(dwellings)

  dwellings$previous_area <- get_area(dwellings)
  dwellings_cut <- sf::st_intersection(DA_buffer, dwellings)
  dwellings_cut$new_area <- get_area(dwellings_cut)
  dwellings_cut <- sf::st_drop_geometry(dwellings_cut)

  dwellings_cut$area_pct <- dwellings_cut$new_area / dwellings_cut$previous_area
  dwellings_cut$n_dwellings <- dwellings_cut$dwellings * dwellings_cut$area_pct

  # For each ID, get the density
  dwelling_density <- aggregate(cbind(n_dwellings, new_area) ~ ID,
                                data = dwellings_cut,
                                FUN = \(...) sum(..., na.rm = TRUE))
  dwelling_density$density <- dwelling_density$n_dwellings / dwelling_density$new_area

  # Calculate the mean and standard deviation
  mean_data <- mean(dwelling_density$density, na.rm = TRUE)
  sd_data <- stats::sd(dwelling_density$density, na.rm = TRUE)

  # Calculate the z-score for each data point
  dwelling_density$z_dwl_d <- (dwelling_density$density - mean_data) / sd_data
  dwelling_density$z_dwl_d[is.na(dwelling_density$z_dwl_d)] <- NA

  # Merge it to the DA_table
  DA_table <- merge(DA_table, dwelling_density[c("ID", "z_dwl_d")],
                    by = "ID", all.x = TRUE)


  # Points of interest measure ----------------------------------------------

  poi_year <- if (year == 2001) 2002 else year

  # Prepare to grab poi data from the bucket
  files <- bucket_list_content("curbcut.amenities")$Key
  txt_shp <- files[grepl("poi/.*(txt$|zip$)", files)]
  year_shps <- txt_shp[grepl(poi_year, txt_shp)]

  poi <- lapply(year_shps, \(f) {
    # If it is a txt file, read it from the bucket with the read.csv method
    if (grepl("\\.txt$", f)) {
      out <- bucket_read_object(object = f,
                                objectext = ".txt",
                                method = utils::read.csv,
                                bucket = "curbcut.amenities")
      return(out)
    }

    # If it's not a txt file, it's a shapefile. Read it using the following
    # function.
    bucket_read_object_zip_shp(object = f, bucket = "curbcut.amenities")
  })
  poi <- Reduce(rbind, poi)

  # Get the right column indicating the SIC codes
  sic_col <- if (year %in% c(2001)) "SIC.1" else "SIC_1"

  # Grab the major groups (from the SIC code)
  poi$sic_major_group <- stringr::str_extract(poi[[sic_col]], "^\\d{2}")

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
  sd_data <- stats::sd(DA_buffer$poi, na.rm = TRUE)

  # Calculate the z-score for each data point
  DA_table$z_poi <- (DA_buffer$poi - mean_data) / sd_data


  # Sum the three z index for the final score -------------------------------

  DA_table <- sf::st_drop_geometry(DA_table)
  DA_table[[paste0("canale_", year)]] <-
    rowSums(DA_table[, c("z_dwl_d", "z_int_d", "z_poi")])
  DA_table <- DA_table[c("ID", paste0("canale_", year))]

  # Return ------------------------------------------------------------------

  return(DA_table)
}

#' Get all three+ way intersections in Canadian streets for a given year
#'
#' This function downloads the streets network for a given year and returns all
#' the intersections with three or more streets.
#'
#' @param year <`numeric`> indicates the year of the streets network to use.
#' Options are census years starting 2001.
#'
#' @return An \code{sf} object representing the three+ way intersections in the
#' streets network.
#' @export
get_all_three_plus_ways <- function(year) {#, OSM_cache = TRUE) {
  # # Grab 2021 streets from OSM
  # osm_pbf <- "http://download.geofabrik.de/north-america/canada-210101.osm.pbf"
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

  # Download the streets network
  url <- cc.data::road_network_url$url[cc.data::road_network_url$year == year]
  temp_zip_file <- tempfile()
  utils::download.file(url, temp_zip_file)
  temp_folder <- tempfile()

  # get the shapefile name
  listed_files <- unzip(temp_zip_file, list = TRUE)$Name
  shp <- if (year == 2001) {
    stringr::str_subset(listed_files, ".e00$")
  } else {
    listed_files <- unzip(temp_zip_file, list = TRUE)$Name
    stringr::str_subset(listed_files, ".shp$")
  }
  utils::unzip(temp_zip_file, exdir = temp_folder)

  streets <- sf::st_read(paste0(temp_folder, "/", shp))
  streets <- sf::st_transform(streets, 3347)

  # Filter out highways
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

  # Shuffle the grid so that each core handles a similar amount of calculations
  grid <- grid[sample(grid), ]
  # Get all the three way intersections in all the grids
  grid_intersects <- sf::st_intersects(grid, streets)
  progressr::with_progress({
    pb <- progressr::progressor(length(grid_intersects))
    three_ways <- future.apply::future_lapply(seq_along(grid_intersects), \(x) {
      out <- sf::st_intersection(grid[x, ], streets[grid_intersects[[x]], ])
      if (nrow(out) == 0) return({
        pb()
        NULL})
      out <- sf::st_intersection(out)
      out <- out[sf::st_is(out, type = "POINT"), ]
      pb()
      tibble::tibble(unique(out["geometry"]))
    })
  })
  three_ways <- three_ways[!sapply(three_ways, is.null)]
  three_ways <- three_ways[sapply(three_ways, nrow) > 0]
  three_ways <- data.table::rbindlist(three_ways)

  # Get all the three way intersections in streets that touch the lines of the
  # grid and that we potentially missed in the first step
  grid_lines <- sf::st_cast(grid, "MULTILINESTRING")
  streets_on_lines <- sf::st_filter(streets, grid_lines)
  streets_on_lines <- sf::st_intersection(streets_on_lines)
  streets_on_lines <-
    streets_on_lines[sf::st_is(streets_on_lines, type = "POINT"), ]
  streets_on_lines <- unique(streets_on_lines["geometry"])
  names(streets_on_lines) <- c("geometry")

  # Bind all three+ way intersections
  three_ways <- rbind(three_ways, streets_on_lines)
  three_ways <- tibble::as_tibble(three_ways) |> sf::st_as_sf() |> unique()
  three_ways$ID <- paste0("tw_", seq_along(three_ways$geometry))

  # Return
  return(three_ways)
}
