build_canale <- function(DA_table, OSM_cache = TRUE) {

  # Workflow from CanALE User Guide
  # http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf

  # Intersection density measure --------------------------------------------

  three_ways <- get_all_three_plus_ways(OSM_cache = OSM_cache)

  # Create a buffer of 1km around DAs
  DA_list <- suppressWarnings(split(DA_table, 1:640))
  progressr::with_progress({
    pb <- progressr::progressor(length(DA_list))
    DA_buffer <- future.apply::future_lapply(DA_list, \(x) {
      out <- sf::st_buffer(x, 1000)
      pb()
      return(out)
    }, future.seed = NULL)
  })
  DA_buffer <- Reduce(rbind, DA_buffer)

  # How many 3+ way intersection per DA buffer
  progressr::with_progress({
    pb <- progressr::progressor(nrow(DA_buffer))
    DA_three_ways <- future.apply::future_lapply(seq_len(nrow(DA_buffer)), \(x) {
      df <- sf:::`[.sf`(DA_buffer, x, )
      three_ways <- sf::st_intersects(DA_buffer, three_ways, sparse = FALSE) |>
        sum()
      df$three_ways <- three_ways
      pb()
      return(out)
    }, future.seed = NULL)
  })
  DA_three_ways <- Reduce(rbind, DA_three_ways)


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

  DA_table <- merge(DA_table, dwellings, by = "ID")


  # Points of interest measure ----------------------------------------------



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
get_all_three_plus_ways <- function(streets, OSM_cache = TRUE) {
  # Grab 2021 streets from OSM
  #osm_pbf <- "http://download.geofabrik.de/north-america/canada-210101.osm.pbf"
  #streets <- osmextract::oe_read(osm_pbf, layer = "lines",
   #                              quiet = FALSE, force_download = !OSM_cache,
  #                               max_file_size = Inf)

  # Retain footbpaths and exclude limited-access highways and their entrances and
  # exits.
  fclasses <- c('trunk', 'primary', 'primary', 'secondary', 'tertiary',
                'unclassified', 'residential', 'pedestrian', 'living_street',
                'footway', 'steps', 'path')
  streets <- streets[streets$highway %in% fclasses, ]
  # Transform streets to the Stats Can CRS
  streets <- sf::st_transform(streets, 3347)

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
