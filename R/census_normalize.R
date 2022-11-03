#' Normalize percentage variables
#'
#' @param interpolated <`list of sf data.frame`> The output of
#' \code{\link[susdata]{census_interpolate}}.
#' @param census_vectors <`data.frame`> Should be equal to
#' \code{\link[susdata]{census_vectors}}
#' @param unit_type <`data.frame`> The output of
#' \code{\link[susdata]{census_unit_type}}.
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[susdata]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[susdata]{census_years}}
#'
#' @return A list of scales and years of census data with variables desired as
#' percentage normalized.
#' @export
census_normalize <- function(interpolated,
                             census_vectors = susdata::census_vectors,
                             census_scales = susdata::census_scales,
                             census_years = susdata::census_years,
                             unit_type =
                               census_unit_type(census_vectors = census_vectors,
                                                census_scales = census_scales,
                                                census_years = census_years)) {

  vars_pct <- census_vectors$var_code[census_vectors$type == "pct"]
  units <- unit_type[unit_type$var_code %in% vars_pct, ]

  sapply(census_scales, \(scale) {
    sapply(as.character(census_years), \(year) {

      data <- interpolated[[scale]][[year]]
      data_no_geo <- sf::st_drop_geometry(data)

      # If classed as percentage in the census
      pcts <-
        names(data)[names(data) %in% units$var_code[units$units == "Percentage"]]

      pcts <-
        sapply(pcts, \(x) {
          tb <- data_no_geo["ID"]
          tb[[x]] <- pmin(1, data_no_geo[[x]]/100)
          tb
        }, simplify = FALSE, USE.NAMES = TRUE)
      pcts <- Reduce(merge, pcts)

      # If classed as number in the census
      numb <-
        names(data)[names(data) %in% units$var_code[units$units == "Number"]]

      numb <-
        sapply(numb, \(x) {
          tb <- data_no_geo["ID"]
          tb[[x]] <- pmin(1, data_no_geo[[x]]/data_no_geo[[paste0(x, "_parent")]])
          tb
        }, simplify = FALSE, USE.NAMES = TRUE)
      numb <- Reduce(merge, numb)

      # Combine
      if (!is.null(pcts) && !is.null(numb)) pcts_numb <- merge(pcts, numb)
      if (is.null(pcts) && !is.null(numb)) pcts_numb <- numb
      if (!is.null(pcts) && is.null(numb)) pcts_numb <- pcts
      pcts_numb <- tibble::as_tibble(pcts_numb)

      # Switch NaN or Inf to NA
      pcts_numb[pcts_numb == "NaN"] <- NA
      pcts_numb[pcts_numb == "Inf"] <- NA

      # Return
      tibble::as_tibble(merge(data[
        , !names(data) %in% names(pcts_numb)[names(pcts_numb) != "ID"]],
        pcts_numb, by = "ID")) |>
        sf::st_as_sf()

    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)

}
