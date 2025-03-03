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
  latest_census <- census_dataset[length(census_dataset)]

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
            # We don't want interpolation for Canada and the Provinces. If boundaries change,
            # it's just methodology.
            dataset = if (scale %in% c("C", "PR")) latest_census else year,
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


          # Nunavut is a territory since 1999
          if (scale == "PR" & year == "CA1996") {
            # Subset the geometries
            geoms_to_merge <- out[out$ID %in% c(61, 62), ]
            # Combine the geometries into a single MULTIPOLYGON
            combined_geometry <- sf::st_union(geoms_to_merge)
            # Create a new feature with ID 61 (NW Territories)
            new_feature <- sf::st_sf(ID = 61, geometry = combined_geometry)
            # Add the new feature back to the rest of the data
            out <- out[!out$ID %in% c(61, 62), ]  # Remove the old features
            out <- rbind(out, new_feature)        # Add the combined feature
          }

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
