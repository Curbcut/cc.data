#' Null-or-NA coalescing operator
#'
#' Returns `b` when `a` is `NULL` or a length-one `NA`, otherwise `a`. The one
#' package-wide `%||%`: cc.data supports R >= 3.5, before base R gained its own.
#' Same semantics as the copy the CMHC workers ship to mirai daemons, where the
#' namespace is not reachable. Define no other top-level `%||%` in this package:
#' R sources files alphabetically, so a later definition silently replaces this
#' one everywhere (see `%or_blank%` for scraped fields that can also be `""`).
#'
#' @param a Value to test.
#' @param b Fallback.
#' @return `a` unless it is `NULL` or a scalar `NA`; otherwise `b`.
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
