#' Get census variables aggregation type
#'
#' Get the aggregation type which will be used for interpolation.
#'
#' @param census_vectors <`data.frame`> Should be equal to
#' \code{\link[cc.data]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A named list of length two. The first is a vector of character of
#' additive variables, and the other is average.
#' @export
census_agg_type <- function(census_vectors = cc.data::census_vectors,
                            census_scales = cc.data::census_scales,
                            census_years = cc.data::census_years) {

  census_vectors_table <- census_get_vectors_table(census_vectors)

  agg_year <-
    # Skip 2001. Some vectors are labelled as averaged but aggregated as
    # additive, e.g. `v_CA01_1667`
    sapply(as.character(census_years[census_years != 2001]), \(year) {
      # Current census dataset
      census_dataset <- paste0("CA", sub("20", "", year))

      vecs <- cancensus::list_census_vectors(census_dataset)
      vecs <-
        vecs[vecs$vector %in% unlist(census_vectors_table[[paste0("vec_", year)]]), ]

      var_codes <-
        census_vectors_table$var_code[!is.na(census_vectors_table[[paste0("vec_", year)]])]

      agg <- sapply(var_codes, \(x) {
        codes <-
          census_vectors_table[census_vectors_table$var_code == x, paste0("vec_", year)] |>
          unlist()
        out <- unique(vecs$aggregation[vecs$vector %in% codes])

        # Check for error
        if (length(out) != 1) {
          stop(paste0(
            "`", x, "` does not have a unique aggregation in `", year,
            "`."
          ))
        }

        tibble::tibble(
          var_code = x,
          agg = gsub(" .*", "", out)
        )
      }, simplify = FALSE)

      agg <- Reduce(rbind, agg)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Combine all years
  agg <- unique(Reduce(rbind, agg_year))

  # Override for census bugs
  agg <-
    agg[!{agg$var_code == "housing_stress_renter" & agg$agg == "Additive"} &
          !{agg$var_code == "housing_stress_owner" & agg$agg == "Additive"} &
            !{agg$var_code == "housing_shelcost" & agg$agg == "Additive"}, ]

  # Check for error
  multiple <- table(agg$var_code)[table(agg$var_code) > 1]
  if (length(multiple) > 0) {
    stop(paste(names(multiple), "has multiple `aggregation` values in ",
               "cancensus::list_census_vectors() between the years."))
  }

  # Out
  list(
    additive = agg$var_code[agg$agg == "Additive"],
    average = agg$var_code[agg$agg != "Additive"]
  )
}
