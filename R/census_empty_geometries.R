#' Get empty geometries
#'
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#'
#' @return A named list of `sf data.frame` the same length as census_scales,
#' with census geometries, containing only ID, population and households.
#' @export
census_empty_geometries <- function(census_scales = cc.data::census_scales,
                                    census_years = cc.data::census_years) {

  # Create vector of dataset code for cancensus
  census_dataset <- paste0("CA", sub("20", "", census_years))

  progressr::with_progress({
    # Iterate over all scales to get the empty geometries
    pb <- progressr::progressor(steps = length(census_scales) * length(census_years))
    census_empty_geometries <-
      future.apply::future_sapply(census_scales, \(scale) {
        future.apply::future_sapply(census_dataset, \(year) {

          # Retrieve
          # In the case it leads to an error (too large request), retrieve
          # by provinces.
          out <- tryCatch({cancensus::get_census(
            dataset = year,
            regions = list(C = "01"),
            level = scale,
            geo_format = "sf",
            quiet = TRUE,
            use_cache = TRUE
          )}, error = function(e) {
            regions <- cancensus::list_census_regions(year)
            regions <- regions$region[regions$level == "PR"]
            regions <- lapply(regions, \(r) {
              cancensus::get_census(
                dataset = year,
                regions = list(PR = r),
                level = scale,
                geo_format = "sf",
                quiet = TRUE,
                use_cache = TRUE
              )
            })
            regions <- lapply(regions, `[`, c("GeoUID", "geometry"))
            return(Reduce(rbind, regions))
          })

          out <- out[, c("GeoUID", "geometry")]
          names(out) <- c("ID", "geometry")
          out <- sf::st_transform(out, 3347)
          pb()
          sf::st_as_sf(tibble::as_tibble(out))
        }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
      }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  })

  # Rename with years
  lapply(census_empty_geometries, \(x) {
    names(x) <- census_years
    x
  })
}
