#' Get census variables units type
#'
#' @param census_vectors <`data.frame`> Should be equal to
#' \code{\link[susdata]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[susdata]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[susdata]{census_years}}
#'
#' @return A dataframe of two columns. The first column is the var_code, and the
#' second is the unit.
#' @export
census_unit_type <- function(census_vectors = susdata::census_vectors,
                             census_scales = susdata::census_scales,
                             census_years = susdata::census_years) {

  units_year <-
    # Skip 2001. Some vectors have `number` units instead of `currency`,
    # e.g. `v_CA01_1674`
    sapply(as.character(census_years[census_years != 2001]), \(year) {
      # Current census dataset
      census_dataset <- paste0("CA", sub("20", "", year))

      vecs <- cancensus::list_census_vectors(census_dataset)
      vecs <-
        vecs[vecs$vector %in% unlist(census_vectors[[paste0("vec_", year)]]), ]

      var_codes <-
        census_vectors$var_code[!is.na(census_vectors[[paste0("vec_", year)]])]

      units <- sapply(var_codes, \(x) {
        codes <-
          census_vectors[census_vectors$var_code == x, paste0("vec_", year)] |>
          unlist()
        out <- unique(vecs$units[vecs$vector %in% codes])

        # Check for error
        if (length(out) != 1)
          stop(paste0("`", x, "` does not have a unique aggregation in `", year,
                      "`."))

        tibble::tibble(var_code = x,
                       units = gsub(" .*", "", out))

      }, simplify = FALSE)

      units <- Reduce(rbind, units)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Combine all years
  units <- unique(Reduce(rbind, units_year))

  # Check for error
  if (length(unique(units$var_code)) != nrow(units))
    stop(paste0("One or more var_code have multiple `units` values in ",
                "cancensus::list_census_vectors() between the years."))

  # Out
  units

}
