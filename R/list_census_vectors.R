#' List available census vectors
#'
#' @return Returns a data frame detailing the available Census vectors
#' (i.e. variables).
#' @export
list_census_vectors <- function() {
  cc.data::census_vectors_table
}
