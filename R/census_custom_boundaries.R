#' Interpolate and process data for non-census boundaries
#'
#' @param destination <`named list`> A named list of sf data.frame. The name is
#' the name of a scale which can be found in the census, e.g. `CSD`, `CT`, `DA`.
#' Each contains an sf data.frame of non-census boundaries, for which data must
#' be interpolated. e.g. boroughs in Montreal.
#' @param data_raw_DA <`named list`> A named list of sf data.frame where the names
#' are census years. It must be the lower possible scale of the raw data (with parents),
#' and so the output of \code{\link[cc.data]{census_data_raw}} \code{[["DA"]]}
#' @param census_vectors <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_vectors}}, or a subset of the character vector to
#' only filter the wanted variables.
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#' @param agg_type <`named list`> The output of
#' \code{\link[cc.data]{census_agg_type}}
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return The same list as what is fed to the `destination` argument,
#' with all processed census data appended.
#' @export
census_custom_boundaries <-
  function(destination, data_raw_DA,
           census_vectors = cc.data::census_vectors,
           census_scales = cc.data::census_scales,
           census_years = cc.data::census_years,
           agg_type = census_agg_type(
             census_vectors_table =
               cc.data::census_vectors_table[
                 cc.data::census_vectors_table$var_code %in% census_vectors,
               ],
             census_scales = census_scales,
             census_years = census_years
           ),
           crs) {

    # Subset the census vectors if necessary
    census_vecs <-
      cc.data::census_vectors_table[cc.data::census_vectors_table$var_code %in%
        census_vectors, ]

    # Prepare the progress bar
    pb <- progressr::progressor(steps = length(destination) * length(census_years))

    # Interpolate
    interpolated <- future.apply::future_sapply(destination, \(dest) {
      # In case no interpolation needed
      if (is.null(dest)) {
        return(dest)
      }
      future.apply::future_sapply(as.character(census_years), \(year) {

        # Transform to census projection
        dest <- sf::st_transform(dest, crs)

        # Interpolate other years
        origin <- data_raw_DA[[year]]
        origin <- sf::st_transform(origin, crs)
        origin <- origin[names(origin) != "ID"]
        origin$area <- get_area(origin)
        int <- suppressWarnings(sf::st_intersection(origin, dest))
        int <- int[sf::st_is(int, "POLYGON") | sf::st_is(int, "MULTIPOLYGON"), ]

        int$int_area <- get_area(int)
        int <- sf::st_drop_geometry(int)
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
          cbind(int[!names(int) %in% names(add_vars)], add_vars)
        )

        # Average values
        avg <- names(int)[names(int) %in% agg_type$average]
        avg_vars <-
          sapply(avg, \(x) {
            ids <- sapply(unique(int$ID), \(id) {
              dat <- add_vars[add_vars$ID == id, ]
              val <- weighted_mean(dat[[x]], dat[[paste0(x, "_parent")]])
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
        out <- tibble::as_tibble(merge(add_vars, avg_vars, by = "ID"))

        # Switch NaN or Inf to NA
        out[out == "NaN"] <- NA
        out[out == "Inf"] <- NA

        # Return as sf
        pb()
        tibble::as_tibble(merge(dest["ID"], out)) |>
          sf::st_as_sf()
      }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)

    normalized <- census_normalize(interpolated,
      census_scales = names(interpolated),
      census_vectors_table = census_vecs
    )
    parent_dropped <- census_drop_parents(normalized,
      census_vectors_table = census_vecs
    )
    census_reduce_years(parent_dropped,
      census_scales = names(interpolated),
      census_vectors_table = census_vecs
    )
  }
