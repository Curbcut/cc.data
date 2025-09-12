#' Normalize census variables to proportions in `[0,1]`
#'
#' @param interpolated list [[scale]][[year]] sf — output of census_interpolate()
#' @param census_vectors <character> cc.data::census_vectors (used only if unit_type = NULL)
#' @param census_scales  <character> cc.data::census_scales
#' @param census_years   <numeric>   cc.data::census_years
#' @param unit_type <data.frame|NULL> if NULL, built from variables actually present
#' @return list [[scale]][[year]] sf with normalized variables
#' @export
census_normalize <- function(interpolated,
  census_vectors = cc.data::census_vectors,
  census_scales  = cc.data::census_scales,
  census_years   = cc.data::census_years,
  unit_type = NULL) {

# Detect variables actually present in requested scales (across all years)
present_vars <- unique(unlist(
lapply(interpolated[names(interpolated) %in% census_scales], function(scale_list) {
unlist(lapply(scale_list, function(df) setdiff(names(df), c("ID","geometry"))),
use.names = FALSE)
})
))
present_vars <- sort(unique(present_vars))

# Restrict vectors table to present variables
census_vectors_table <- census_get_vectors_table(present_vars)

# Build unit_type only for present variables (if not supplied)
if (is.null(unit_type)) {
unit_type <- census_unit_type(
census_vectors = present_vars,
census_scales  = census_scales,
census_years   = census_years
)
}
unit_type <- unit_type[unit_type$var_code %in% present_vars, , drop = FALSE]

# Variables that should be normalized as proportions
vars_pct <- census_vectors_table$var_code[census_vectors_table$type == "pct"]
units    <- unit_type[unit_type$var_code %in% vars_pct, , drop = FALSE]

sapply(census_scales, function(scale) {
sapply(as.character(census_years), function(year) {

data <- interpolated[[scale]][[year]]
if (!inherits(data, "sf")) data <- sf::st_as_sf(data, sf_column_name = "geometry")
data_no_geo <- unique(sf::st_drop_geometry(data))

# Percentage → proportion in `[0,1]`
pct_vars <- intersect(names(data), units$var_code[units$units == "Percentage"])
if (length(pct_vars) > 0) {
pcts_list <- lapply(pct_vars, function(x) {
tb <- data_no_geo["ID"]
tb[[x]] <- pmin(1, data_no_geo[[x]] / 100)
tb
})
pcts <- Reduce(function(a, b) merge(a, b, by = "ID"), pcts_list)
} else {
pcts <- NULL
}

# Number (with parent) → proportion in `[0,1]`
numb_vars <- intersect(names(data), units$var_code[units$units == "Number"])
if (length(numb_vars) > 0) {
numb_list <- lapply(numb_vars, function(x) {
tb <- data_no_geo["ID"]
parent_string <- census_vectors_table$parent_vec[
census_vectors_table$var_code == x
]
if (length(parent_string) == 0L || !parent_string %in% names(data_no_geo)) {
# No valid parent denominator: return NA (conservative)
tb[[x]] <- NA_real_
} else {
denom <- data_no_geo[[parent_string]]
ratio <- data_no_geo[[x]] / denom
tb[[x]] <- pmin(1, ratio)
}
tb
})
numb <- Reduce(function(a, b) merge(a, b, by = "ID"), numb_list)
} else {
numb <- NULL
}

# Combine normalized outputs; if nothing to normalize, return input as-is
if (!is.null(pcts) && !is.null(numb)) {
pcts_numb <- merge(pcts, numb, by = "ID")
} else if (is.null(pcts) && !is.null(numb)) {
pcts_numb <- numb
} else if (!is.null(pcts) && is.null(numb)) {
pcts_numb <- pcts
} else {
return(data)  # nothing to normalize
}

pcts_numb <- tibble::as_tibble(pcts_numb)

# Sanitize numeric outputs: NaN / Inf → NA
num_cols <- names(pcts_numb)[sapply(pcts_numb, is.numeric)]
if (length(num_cols)) {
pcts_numb[num_cols] <- lapply(pcts_numb[num_cols], function(col) {
col[is.nan(col)]      <- NA_real_
col[is.infinite(col)] <- NA_real_
col
})
}

# Merge back into original data (preserve other columns and geometry)
keep_cols <- !names(data) %in% names(pcts_numb)[names(pcts_numb) != "ID"]
sf::st_as_sf(tibble::as_tibble(merge(
data[, keep_cols, drop = FALSE],
pcts_numb, by = "ID"
)))
}, simplify = FALSE, USE.NAMES = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)
}
