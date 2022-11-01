#' Compute geometric measurements and drop units
#'
#' @param geometry <`sf`, `sfc` or `sfg`> A column of geometries.
#' @param ... passed on to \code{\link[sf]{st_area}}
#'
#' @return A numeric vector of shape areas.
#' @export
get_area <- function(geometry, ...) {
  x <- sf::st_area(geometry)
  as.vector(x)
}

#' Helper for interpolation
#'
#' Needed as stats::weighted.mean does not handle NA weights
#'
#' @param x The value
#' @param w The weight
#' @param ... Other options to pass to stats::weighted.mean()
#' @param na.rm Remove NAs, defaults to TRUE
#'
#' @return Same output as weighted.mean, but with NA handled for weights.
#' @export
weighted_mean <- function(x, w, ..., na.rm = TRUE) {
  if (na.rm) {
    x_1 <- x[!is.na(x) & !is.na(w)]
    w <- w[!is.na(x) & !is.na(w)]
    x <- x_1
  }
  stats::weighted.mean(x, w, ..., na.rm = TRUE)
}
