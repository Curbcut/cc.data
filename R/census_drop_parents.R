#' Drop parent variables
#'
#' @param normalized <`list of sf data.frame`> The output of
#' \code{\link[cc.data]{census_normalize}}.
#' @param census_vectors_table <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_vectors_table}}
#'
#' @return A list of scales and years of census data with the columns
#' corresponding to ID, census_vectors_table$var_code, and geometry.
#' @export
census_drop_parents <- function(normalized,
                                census_vectors_table = cc.data::census_vectors_table) {
  sapply(normalized, \(x) {
    sapply(x, \(y) {
      var_codes <- names(y)[names(y) %in% census_vectors_table$var_code]

      y[, c("ID", var_codes, "geometry")]
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
}
