#' Retrieve and Process Dissemination Block Geographical Information
#'
#' This function processes geographical information from Dissemination Blocks (DBs)
#' for a specified set of years. It excludes the year 1996 as there were no DBs in that
#' year, transforms, and intersects the data with given cartographic boundaries,
#' and appends population and dwelling data. It supports specific transformation
#' and intersection operations on spatial data.
#'
#' @param years <`numeric vector`> A vector of years to process. Default is set to
#' census_years from the cc.data package. The year 1996 is automatically excluded
#' as it does not contain DBs.
#' @param crs <`character`> Coordinate Reference System to be used for transforming
#' the spatial data.
#' @param DA_carto <`sf object`> An sf object representing cartographic boundaries
#' for intersection with the DB geographical information.
#'
#' @return A list of processed spatial data for each year, each element being an sf
#' object with MULTIPOLYGON geometry, and containing population and dwelling counts.
#' The data is transformed to the specified CRS and intersected with the DA_carto object.
#' @export
DB_get <- function(years = cc.data::census_years, crs, DA_carto) {

  # 1996 does not have DBs
  years <- years[years != 1996]

  years_l <- lapply(years, c)
  names(years_l) <- years

  years_l <- lapply(years_l, \(x) {
    file <- sprintf("DB_shp_%s.zip", x)
    if (x == 2021)  file <- "DB_shp_carto.zip"
    cc.data::bucket_read_object_zip_shp(file, "curbcut.rawdata")
  })

  # Transform and diff with cartographic
  years_l <- lapply(years_l, sf::st_transform, crs)
  years_l <- lapply(years_l, sf::st_make_valid)

  # Cut it at DA_carto
  years_l <- lapply(years_l, \(x) {
    int <- sf::st_intersects(x, DA_carto)
    only <- x[as.logical(lengths(int)), ]
    sf::st_intersection(only, DA_carto)
  })

  # Convert to MULTIPOLYGON
  years_l <- lapply(years_l, sf::st_cast, "MULTIPOLYGON")

  # Keep DA ID
  years_l <- lapply(years_l, \(x) {
    names(x)[names(x) == "DAUID"] <- "DA_ID"
    names(x)[1] <- "ID"
    x <- x[c(names(x)[1], if ("DA_ID" %in% names(x)) "DA_ID")]

    if (!"DA_ID" %in% names(x)) x$DA_ID <- stringr::str_extract(x$ID, "^\\d{8}")
    x
  })

  # Add population and dwellings count
  years_l <- lapply(years, \(y) {
    ds <- sprintf("CA%s", gsub(20, "", y))

    # Split into x number of waves so the retrieval is possible (and faster)
    DB_ID <- years_l[[as.character(y)]]$ID
    waves <- split(DB_ID, seq_len(ceiling(length(DB_ID) / 1000)))

    progressr::with_progress({
      pb <- progressr::progressor(steps = length(waves))
      dbs <- lapply(waves, \(x) {
        out <- cancensus::get_census(ds, region = list(DB = x), quiet = TRUE,
                                     use_cache = TRUE)
        pb()
        out
      })
    })

    # Bind the rows
    dbs <- Reduce(rbind, dbs)
    names(dbs)[names(dbs) == "GeoUID"] <- "ID"

    # Select good columns
    dbs <- dbs[, c("ID", "Population", "Dwellings")]

    # Rename columns
    names(dbs) <- c("ID", "population", "dwellings")

    # Merge population and households
    merge(years_l[[as.character(y)]], dbs, by = "ID")

  })

  names(years_l) <- years

  # Return
  return(years_l)
}
