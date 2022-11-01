#' Interpolate all census years to the current year's geography
#'
#' @param data_raw <`list of sf data.frame`> The output of
#' \code{\link[susdata]{census_data_raw}}.
#' @param agg_type <`named list`> The output of
#' \code{\link[susdata]{census_agg_type}}.
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[susdata]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[susdata]{census_years}}
#'
#' @return A list of scales and years of census data with geometries interpolated
#' to the current year.
census_interpolate <- function(data_raw, agg_type, census_scales,
                               census_years) {
  sapply(census_scales, \(scale) {
    # Scale level globals
    max_year <- as.character(max(census_years))
    data_scale <- data_raw[[scale]]
    destination <- data_scale[[max_year]][, c("GeoUID", "geometry")]

    # Interpolate every year
    sapply(as.character(census_years), \(year) {

      # If max year, don't interpolate
      if (year == max_year) return(data_scale[[year]])

      # Interpolate other years
      origin <- data_scale[[year]]
      origin <- origin[names(origin) != "GeoUID"]
      origin$area <- get_area(origin)
      int <- suppressWarnings(sf::st_intersection(origin, destination))
      int <- int[sf::st_is(int, "POLYGON") | sf::st_is(int, "MULTIPOLYGON"), ]

      int$int_area <- get_area(int)
      int$area_prop <- int$int_area / int$area

      # Additive values, pre needed for average values
      add <- names(int)[names(int) %in% agg_type$additive]
      add <- c(add, names(int)[grepl("_parent$", names(int))])
      add_vars <- sapply(add, \(x) {
        v <- int[[x]] * int$area_prop
        out <- tibble::tibble(l = v)
        names(out) <- x
        out
        }, simplify = FALSE)
      add_vars <- Reduce(cbind, add_vars)

      add_vars <- tibble::tibble(
        cbind(int[!names(int) %in% names(add_vars)], add_vars))

      # Average values
      avg <- names(int)[names(int) %in% agg_type$average]
      avg_vars <-
        sapply(avg, \(x) {
          ids <- sapply(unique(int$GeoUID), \(id) {
            dat <- add_vars[add_vars$GeoUID == id, ]
            val <- weighted_mean(dat[[x]], dat[[paste0(x, "_parent")]])
            # Only keep output polygons with a majority non-NA inputs
            na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
            if (na_pct >= 0.5) val <- NA_real_

            out <- tibble::tibble(GeoUID = id,
                                  val = val)
            names(out)[2] <- x
            out
          }, simplify = FALSE)
          tibble::as_tibble(Reduce(rbind, ids))
        }, simplify = FALSE)

      avg_vars <- tibble::as_tibble(Reduce(merge, avg_vars))

      # Finalize additive values
      add_vars <- sapply(add, \(x) {
        ids <- sapply(unique(int$GeoUID), \(id) {

          dat <- add_vars[add_vars$GeoUID == id, ]
          val <- sum(dat[[x]], dat$area_prop, na.rm = TRUE)
          val <- round(val / 5) * 5
          # Only keep output polygons with a majority non-NA inputs
          na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
          if (na_pct >= 0.5) val <- NA_real_

          out <- tibble::tibble(GeoUID = id,
                                val = val)
          names(out)[2] <- x
          out
        }, simplify = FALSE)
        tibble::as_tibble(Reduce(rbind, ids))
      }, simplify = FALSE)

      add_vars <- tibble::as_tibble(Reduce(merge, add_vars))

      # Merge additive and average variables
      tibble::as_tibble(merge(merge(add_vars, avg_vars, by = "GeoUID"),
                              destination[c("GeoUID", "geometry")])) |>
        sf::st_as_sf()

    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
}
