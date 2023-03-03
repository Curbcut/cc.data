#' Create the dictionary linking the most recent IDs to past DA IDs
#'
#' @param DA_data_raw <`list of sf data.frame`> The DA output of
#' \code{\link[cc.data]{census_data_raw}}. The DA output of the raw data retrieval.
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A tibble with the number of columns as there are available years in the
#' census. The anchor is the most recent census' DA IDs. Every other column is
#' the other year's IDs with which the most recent DAs intersect.
#' @export
census_build_DA_years_dictionary <- function(DA_data_raw,
                                             census_years = cc.data::census_years) {

  # Get the most recent year to anchor the dictionary
  most_recent_census <- as.character(census_years[length(census_years)])

  # Get all years, minus the most recent one
  years <- as.character(census_years)[-length(as.character(census_years))]

  # Iterate over all years to create diuctionary tibbles
  dict_tibbles <- future.apply::future_sapply(years, \(x) {
    recent <- DA_data_raw[[most_recent_census]]["ID"]
    ite <- DA_data_raw[[x]]["ID"]

    intersected <- sf::st_intersects(recent, ite, sparse = T)

    ite <- sf::st_drop_geometry(ite)
    ite <- sapply(intersected, \(x) ite$ID[x], USE.NAMES = FALSE)

    out <- tibble::tibble(ID_2021 = recent$ID)
    out[[paste0("ID_", x)]] <- ite

    out
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Merge the outputs
  Reduce(\(x, y) base::merge(x, y, by = "ID_2021", all.x = TRUE), dict_tibbles) |>
    tibble::as_tibble()
}
