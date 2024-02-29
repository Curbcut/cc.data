#' Interpolate and process data for non-census boundaries
#'
#' @param destination <`named list`> A named list of sf data.frame. The name is
#' the name of a scale which can be found in the census, e.g. `CSD`, `CT`, `DA`.
#' Each contains an sf data.frame of non-census boundaries, for which data must
#' be interpolated. e.g. boroughs in Montreal.
#' @param DA_IDs <`named list`> <`character vector`> All the current census'
#' DA IDs present in the region. Only those will be extracted from the database
#' to do interpolation.
#' @param DB_table <`named list`> List of DB tables for all census years.
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
  function(destination, DA_IDs, DB_table,
           census_vectors = cc.data::census_vectors,
           census_scales = cc.data::census_scales,
           census_years = cc.data::census_years,
           agg_type = census_agg_type(
             census_vectors = census_vectors,
             census_scales = census_scales,
             census_years = census_years
           ),
           crs) {

    # Format the DB scale
    DB_table <- lapply(DB_table, \(x) {
      names(x)[names(x) == "ID"] <- "DB_ID"
      x
    })

    # Subset the census vectors if necessary
    census_vectors_table <- census_get_vectors_table(census_vectors)

    # Grab DA tables for all years
    # Get origin's DA data from the database
    origins <-
      lapply(census_years, \(year) {
        ids_retrieve_from_db <-
          cc.data::census_DA_years_dict[[paste0("ID_", year)]][
            cc.data::census_DA_years_dict[[1]] %in% DA_IDs
          ]
        ids_retrieve_from_db <- unique(unlist(ids_retrieve_from_db))

        # Open a DB connection and get the necessary data
        origin <- db_read_data(
          table = paste0("raw_DA_", year),
          columns = census_vectors_table$var_code,
          IDs = ids_retrieve_from_db, keep_geometry = TRUE
        )
        sf::st_transform(origin, crs)
      })
    names(origins) <- as.character(census_years)

    progressr::with_progress({

      # Prepare the progress bar
      # Interpolate
      pb <- progressr::progressor(steps = length(destination) * length(census_years))

      interpolated <- sapply(destination, \(dest) {
        # In case no interpolation needed
        if (is.null(dest)) {
          return(dest)
        }
        sapply(as.character(census_years), \(year) {

          # Transform to census projection
          dest <- sf::st_transform(dest, crs)

          # Get origin's DA data from the database
          origin <- origins[[year]]

          # If there are DBs for that year, interpolate to them and use them
          # for following interpolation.
          if (year %in% names(DB_table)) {
            # Grab the DB_table of this year
            DBs <- DB_table[[year]]

            # Calculate the sum of population for each ID
            population_sums <- stats::aggregate(population ~ DA_ID, data = DBs, sum)
            names(population_sums)[1] <- "DA_ID"

            # Merge the sum back with the original data frame
            merged_data <- merge(DBs, population_sums, by = "DA_ID", suffixes = c("", "_sum"))

            # Calculate population percentage
            merged_data$pop_pct <- with(merged_data, population / population_sum)

            # Merge DB info with origin data
            merged_data <- merge(merged_data, sf::st_drop_geometry(origin), by.x = "DA_ID", by.y = "ID")

            # Looping through each column to process
            for (col in census_vectors_table$var_code) {
              # Check if the column exists in the data frame
              if (col %in% names(merged_data)) {
                if (col %in% agg_type$additive) {
                  # Calculate the new column values
                  merged_data[[col]] <- with(merged_data, merged_data[[col]] * pop_pct)
                }
              } else {
                warning(paste("Column", col, "not found in the data frame."))
              }
            }

            # Select the required columns
            merged_data <- tibble::as_tibble(merged_data)
            merged_data <- sf::st_as_sf(merged_data)
            origin <- merged_data[names(merged_data) %in% census_vectors_table$var_code]
          }

          # Interpolate other years
          origin <- origin[names(origin) != "ID"]
          origin$area <- get_area(origin)
          int <- suppressWarnings(sf::st_intersection(origin, dest))
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
          # Create a function for weighted mean calculation
          weighted_mean_vectorized <- function(x, weight, group) {
            tapply(x * weight, group, sum, na.rm = TRUE) / tapply(weight, group, sum, na.rm = TRUE)
          }

          avg_vars <- lapply(avg, function(x) {
            parent_string <- cc.data::census_vectors_table$parent_vec[cc.data::census_vectors_table$var_code == x]

            # Calculate weighted mean for each group
            val <- weighted_mean_vectorized(add_vars[[x]], add_vars[[parent_string]], add_vars$ID)

            # Calculate NA percentage for each group
            na_pct <- tapply(is.na(add_vars[[x]]) * add_vars$int_area, add_vars$ID, sum) / tapply(add_vars$int_area, add_vars$ID, sum)

            # Replace values with NA where NA percentage is >= 0.5
            val[na_pct >= 0.5] <- NA_real_

            # Combine results into a tibble
            tibble::tibble(ID = names(val), !!x := as.numeric(val))
          })

          # Combine all tibbles into one
          avg_vars <- Reduce(merge, avg_vars)

          # Finalize additive values
          add_vars <- lapply(add, function(x) {
            # Calculate sum for each group
            val <- tapply(add_vars[[x]], add_vars$ID, sum, na.rm = TRUE)

            # Calculate NA percentage for each group
            na_pct <- tapply(is.na(add_vars[[x]]) * add_vars$int_area, add_vars$ID, sum) / tapply(add_vars$int_area, add_vars$ID, sum)

            # Replace values with NA where NA percentage is >= 0.5
            val[na_pct >= 0.5] <- NA_real_

            # Combine results into a tibble
            tibble::tibble(ID = names(val), !!x := as.numeric(val))
          })

          # Combine all tibbles into one
          add_vars <- Reduce(merge, add_vars)

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

          # Return as non-projected sf
          pb()
          tibble::as_tibble(merge(dest["ID"], out, all = TRUE)) |>
            sf::st_as_sf()
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    })

    normalized <- census_normalize(
      interpolated = interpolated,
      census_scales = names(interpolated),
      census_vectors = census_vectors,
      census_years = census_years
    )
    census_reduce_years(normalized,
                        census_scales = names(interpolated),
                        census_vectors = census_vectors,
                        census_years = census_years
    )
  }
