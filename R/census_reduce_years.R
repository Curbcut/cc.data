#' Reduce all census years to one dataframe
#'
#' @param parent_dropped <`list of sf data.frame`> The output of
#' \code{\link[cc.data]{census_drop_parents}}.
#' @param census_vectors <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A list of scales with years reduced, and the years appended to all
#' variables column name
#' @export
census_reduce_years <- function(parent_dropped,
                                census_vectors = cc.data::census_vectors,
                                census_scales = cc.data::census_scales,
                                census_years = cc.data::census_years) {

  # Subset the census vectors table including necessary parent variables
  parent_vecs <-
    cc.data::census_vectors_table$parent_vec[
      cc.data::census_vectors_table$var_code %in% census_vectors
    ]
  census_vectors <- unique(c(census_vectors, parent_vecs))
  census_vectors_table <- cc.data::census_vectors_table[
    cc.data::census_vectors_table$var_code %in% census_vectors,
  ]

  sapply(census_scales, \(scale) {
    years <- sapply(as.character(census_years), \(year) {
      data <- parent_dropped[[scale]][[year]]
      var_codes <- names(data)[names(data) %in% census_vectors_table$var_code]
      names(data)[names(data) %in% var_codes] <- paste0(var_codes, "_", year)

      data
    }, simplify = FALSE, USE.NAMES = TRUE)

    # Drop geometry except the current year
    years <-
      c(
        lapply(years[1:(length(years) - 1)], sf::st_drop_geometry),
        years[length(years)]
      )

    merge_keep_all <- function(...) merge(..., all.x = TRUE)
    tibble::as_tibble(Reduce(merge_keep_all, rev(years))) |>
      sf::st_as_sf()
  }, simplify = FALSE, USE.NAMES = TRUE)
}
