#' Drop parent variables
#'
#' @param normalized <`list of sf data.frame`> The output of
#' \code{\link[susdata]{census_normalize}}.
#' @param census_vectors <`character vector`> Should be equal to
#' \code{\link[susdata]{census_vectors}}
#'
#' @return A list of scales and years of census data with the columns
#' corresponding to ID, census_vectors$var_code, and geometry.
#' @export
census_drop_parents <- function(normalized,
                               census_vectors = susdata::census_vectors) {
  sapply(normalized, \(x) {
    sapply(x , \(y) {
      var_codes <- names(y)[names(y) %in% census_vectors$var_code]

      y[, c("ID", var_codes, "geometry")]
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
}
