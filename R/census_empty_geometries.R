#' Get empty geometries
#'
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[susdata]{census_years}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[susdata]{census_scales}}
#'
#' @return A named list of `sf data.frame` the same length as census_scales,
#' with census geometries, containing only ID, population and households.
#' @export
census_empty_geometries <- function(census_years, census_scales) {

  # Create vector of dataset code for cancensus
  census_dataset <- paste0("CA", sub("20", "", census_years))

  # Iterate over all scales to get the empty geometries
  census_empty_geometries <-
    sapply(census_scales, \(scale) {
      sapply(census_dataset, \(year) {
        out <- cancensus::get_census(dataset = year,
                                     regions = list(PR = "13"),
                                     level = scale, geo_format = "sf",
                                     quiet = TRUE)
        out <- out[, c("GeoUID", "Population", "Households", "geometry")]
        names(out) <- c("GeoUID", "population", "households", "geometry")

        out <- sf::st_transform(out, 3347)
        sf::st_as_sf(tibble::as_tibble(out))
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Rename with years
  lapply(census_empty_geometries, \(x) {
    names(x) <- census_years
    x
  })
}
