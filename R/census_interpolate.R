#' Interpolate all census years to the current year's geography
#'
#' @param data_raw <`list of sf data.frame`> The output of
#' \code{\link[cc.data]{census_data_raw}}.
#' @param agg_type <`named list`> The output of
#' \code{\link[cc.data]{census_agg_type}}.
#' @param census_vectors <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A list of scales and years of census data with geometries interpolated
#' to the current year.
census_interpolate <- function(data_raw,
                               census_vectors = cc.data::census_vectors,
                               census_scales = cc.data::census_scales,
                               census_years = cc.data::census_years,
                               agg_type = census_agg_type(
                                 census_vectors = census_vectors,
                                 census_scales = census_scales,
                                 census_years = census_years
                               )) {
  pb <- progressr::progressor(steps = length(census_scales) * length(census_years))

  future.apply::future_sapply(census_scales, \(scale) {
    future.apply::future_sapply(as.character(census_years), \(year) {

      # Scale level globals
      max_year <- as.character(max(census_years))
      data_scale <- data_raw[[scale]]
      all_dest <- data_scale[[max_year]][, c("ID", "geometry")] |>
        sf::st_make_valid()

      # If max year, don't interpolate
      if (year == max_year) {
        return(data_scale[[year]])
      }

      # IDs that needs interpolation
      y_df <- data_scale[[year]]
      needing_inter <- sapply(all_dest$ID, \(id) {
        if (!id %in% y_df$ID) {
          return(FALSE)
        }
        dest_id <- all_dest$geometry[all_dest$ID == id]
        or_id <- y_df$geometry[y_df$ID == id]

        get_area(sf::st_intersection(dest_id, or_id)) / get_area(dest_id) > 0.95
      })
      needing_inter <- names(needing_inter)[which(needing_inter == F)]

      # Update destination to only include the ones that need interpolation
      destination <- all_dest[all_dest$ID %in% needing_inter, ]

      # Interpolate other years
      origin <- sf::st_make_valid(data_raw[["DA"]][[year]])
      origin <- origin[names(origin) != "ID"]
      origin$area <- get_area(origin)
      int <- suppressWarnings(sf::st_intersection(origin, destination))
      int <- int[sf::st_is(int, "POLYGON") | sf::st_is(int, "MULTIPOLYGON"), ]

      int$int_area <- get_area(int)
      int <- sf::st_drop_geometry(int)
      int$area_prop <- int$int_area / int$area

      # Additive values, pre needed for average values
      add <- names(int)[names(int) %in% agg_type$additive]
      add_vars <- sapply(add, \(x) {
        v <- int[[x]] * int$area_prop
        out <- tibble::tibble(l = v)
        names(out) <- x
        out
      }, simplify = FALSE)
      add_vars <- Reduce(cbind, add_vars)

      add_vars <- tibble::tibble(
        cbind(int[!names(int) %in% names(add_vars)], add_vars)
      )

      # Average values
      avg <- names(int)[names(int) %in% agg_type$average]
      avg_vars <-
        sapply(avg, \(x) {
          ids <- sapply(unique(int$ID), \(id) {
            dat <- add_vars[add_vars$ID == id, ]
            parent_string <-
              cc.data::census_vectors_table$parent_vec[
                cc.data::census_vectors_table$var_code == x]
            val <- weighted_mean(dat[[x]], dat[[parent_string]])
            # Only keep output polygons with a majority non-NA inputs
            na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
            if (na_pct >= 0.5) val <- NA_real_

            out <- tibble::tibble(
              ID = id,
              val = val
            )
            names(out)[2] <- x
            out
          }, simplify = FALSE)
          tibble::as_tibble(Reduce(rbind, ids))
        }, simplify = FALSE)

      avg_vars <- tibble::as_tibble(Reduce(merge, avg_vars))

      # Finalize additive values
      add_vars <- sapply(add, \(x) {
        ids <- sapply(unique(int$ID), \(id) {
          dat <- add_vars[add_vars$ID == id, ]
          val <- sum(dat[[x]], dat$area_prop, na.rm = TRUE)
          val <- round(val / 5) * 5
          # Only keep output polygons with a majority non-NA inputs
          na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
          if (na_pct >= 0.5) val <- NA_real_

          out <- tibble::tibble(
            ID = id,
            val = val
          )
          names(out)[2] <- x
          out
        }, simplify = FALSE)
        tibble::as_tibble(Reduce(rbind, ids))
      }, simplify = FALSE)

      add_vars <- tibble::as_tibble(Reduce(merge, add_vars))

      # Merge additive and average variables
      out <- if (nrow(add_vars) == 0) {
        avg_vars
      } else if (nrow(avg_vars) == 0) {
        add_vars
      } else {
        merge(add_vars, avg_vars, by = "ID")
      }
      out <- tibble::as_tibble(out)

      # Switch NaN or Inf to NA
      out[out == "NaN"] <- NA
      out[out == "Inf"] <- NA

      # Bind the correct year's data with the interpolated data
      out <- rbind(
        sf::st_drop_geometry(y_df[!y_df$ID %in% out$ID, ]),
        out
      )

      # Return as sf
      pb()
      tibble::as_tibble(merge(out, all_dest[c("ID", "geometry")])) |>
        sf::st_as_sf()
    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
}
