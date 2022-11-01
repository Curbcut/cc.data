#' Get census variables aggregation type
#'
#' Get the aggregation type which will be used for interpolation.
#'
#' @param census_vectors <`data.frame`> Should be equal to
#' \code{\link[susdata]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[susdata]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[susdata]{census_years}}
#'
#' @return A named list of length two. The first is a vector of character of
#' additive variables, and the other is average.
#' @export
census_agg_type <- function(census_vectors, census_scales, census_years) {

  agg_year <-
    # Skip 2001. Some vectors are labelled as averaged but aggregated as
    # additive, e.g. `v_CA01_1667`
    sapply(as.character(census_years[census_years != 2001]), \(year) {
      # Current census dataset
      census_dataset <- paste0("CA", sub("20", "", year))

      vecs <- cancensus::list_census_vectors(census_dataset)
      vecs <-
        vecs[vecs$vector %in% unlist(census_vectors[[paste0("vec_", year)]]), ]

      var_codes <-
        census_vectors$var_code[!is.na(census_vectors[[paste0("vec_", year)]])]

      agg <- sapply(var_codes, \(x) {
        codes <-
          census_vectors[census_vectors$var_code == x, paste0("vec_", year)] |>
          unlist()
        out <- unique(vecs$aggregation[vecs$vector %in% codes])

        # Check for error
        if (length(out) != 1)
          stop(paste0("`", x, "` does not have a unique aggregation in `", year,
                      "`."))

        tibble::tibble(var_code = x,
                       agg = gsub(" .*", "", out))

      }, simplify = FALSE)

      agg <- Reduce(rbind, agg)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Combine all years
  agg <- unique(Reduce(rbind, agg_year))

  # Check for error
  if (length(unique(agg$var_code)) != nrow(agg))
    stop(paste0("One or more var_code have multiple `aggregation` values in ",
                "cancensus::list_census_vectors() between the years."))

  # Out
  list(additive = agg$var_code[agg$agg == "Additive"],
       average = agg$var_code[agg$agg != "Additive"])

}
