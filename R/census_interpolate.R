#' Interpolate all census years to the current year's geography
#'
#' @param data_raw <list of sf data.frame> The output of cc.data::census_data_raw.
#' @param census_vectors <character vector> Should be equal to cc.data::census_vectors
#' @param census_scales  <character vector> Should be equal to cc.data::census_scales
#' @param census_years   <numeric vector>   Should be equal to cc.data::census_years
#' @param agg_type <named list|NULL> If NULL, built from variables actually present in data_raw.
#' @return list [[scale]][[year]] sf with geometries of max year and values interpolated.
#' @export
census_interpolate <- function(data_raw,
  census_vectors = cc.data::census_vectors,
  census_scales  = cc.data::census_scales,
  census_years   = cc.data::census_years,
  agg_type = NULL) {

# Variables present only in the requested scales
present_vars <- unique(unlist(
lapply(data_raw[names(data_raw) %in% census_scales], function(scale_list) {
unlist(lapply(scale_list, function(df) setdiff(names(df), c("ID","geometry"))),
use.names = FALSE)
})
))
present_vars <- sort(unique(present_vars))

# Build/filter agg_type after detecting actually present variables
if (is.null(agg_type)) {
agg_type <- census_agg_type(
census_vectors = present_vars,
census_scales  = census_scales,
census_years   = census_years
)
}
agg_type$additive <- intersect(as.character(agg_type$additive), present_vars)
agg_type$average  <- intersect(as.character(agg_type$average),  present_vars)

pb <- progressr::progressor(steps = length(census_scales) * length(census_years))

future.apply::future_sapply(census_scales, function(scale) {
future.apply::future_sapply(as.character(census_years), function(year) {

max_year   <- as.character(max(census_years))
data_scale <- data_raw[[scale]]

# Destination (max_year geometry) — force sf
all_dest <- data_scale[[max_year]]
if (!inherits(all_dest, "sf")) {
all_dest <- sf::st_as_sf(all_dest, sf_column_name = "geometry")
}
all_dest <- sf::st_make_valid(all_dest)[, c("ID","geometry")]

# Latest year: return as is (force sf if needed)
if (year == max_year) {
pb()
res_now <- data_scale[[year]]
if (!inherits(res_now, "sf")) {
res_now <- sf::st_as_sf(res_now, sf_column_name = "geometry")
}
return(res_now)
}

# Origin data for the current year (force sf)
y_df <- data_scale[[year]]
if (!inherits(y_df, "sf")) {
y_df <- sf::st_as_sf(y_df, sf_column_name = "geometry")
}

# IDs requiring interpolation (missing ID or geometry significantly different)
needing_inter <- sapply(all_dest$ID, function(id) {
if (!id %in% y_df$ID) return(FALSE)
dest_id <- all_dest$geometry[all_dest$ID == id]
or_id   <- y_df$geometry[y_df$ID == id]
cc.data::get_area(sf::st_intersection(dest_id, or_id)) /
cc.data::get_area(dest_id) > 0.95
})
needing_inter <- names(needing_inter)[which(needing_inter == FALSE)]
destination <- all_dest[all_dest$ID %in% needing_inter, ]

# Origin layer for intersection (prefer DA if available, else same scale) — force sf
if ("DA" %in% names(data_raw)) {
origin <- data_raw[["DA"]][[year]]
if (!inherits(origin, "sf")) {
origin <- sf::st_as_sf(origin, sf_column_name = "geometry")
}
} else {
origin <- y_df
}
origin <- sf::st_make_valid(origin)
origin <- origin[names(origin) != "ID"]
origin$area <- cc.data::get_area(origin)

# Intersection
int <- suppressWarnings(sf::st_intersection(origin, destination))
int <- int[sf::st_is(int, "POLYGON") | sf::st_is(int, "MULTIPOLYGON"), ]

int$int_area <- cc.data::get_area(int)
int <- sf::st_drop_geometry(int)
if (!nrow(int)) {
out <- sf::st_drop_geometry(y_df)
res <- tibble::as_tibble(merge(out, all_dest[c("ID","geometry")])) |> sf::st_as_sf()
pb(); return(res)
}
int$area_prop <- int$int_area / int$area

# Variables to process actually present in the intersection
avg <- intersect(names(int), agg_type$average)
add <- intersect(names(int), agg_type$additive)

# Additive variables: area-based apportionment
if (length(add)) {
add_vars_list <- lapply(add, function(x) {
v <- int[[x]] * int$area_prop
out <- tibble::tibble(l = v); names(out) <- x; out
})
add_vars <- Reduce(cbind, add_vars_list)
add_vars <- tibble::tibble(cbind(int[!names(int) %in% names(add_vars)], add_vars))
} else {
add_vars <- tibble::as_tibble(int)
}

# Average variables: weighted by parent_vec + NA rule (≥50% area NA)
if (length(avg)) {
avg_vars <- lapply(avg, function(x) {
ids <- lapply(unique(int$ID), function(id) {
dat <- add_vars[add_vars$ID == id, ]
parent_string <- cc.data::census_vectors_table$parent_vec[
cc.data::census_vectors_table$var_code == x]
val <- if (length(parent_string) == 0L || !parent_string %in% names(dat)) {
NA_real_
} else {
cc.data::weighted_mean(dat[[x]], dat[[parent_string]])
}
na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
if (is.nan(na_pct) || na_pct >= 0.5) val <- NA_real_
out <- tibble::tibble(ID = id, val = val); names(out)[2] <- x; out
})
tibble::as_tibble(Reduce(rbind, ids))
})
avg_vars <- tibble::as_tibble(Reduce(function(a,b) merge(a,b,by="ID"), avg_vars))
} else {
avg_vars <- tibble::tibble()
}

# Finalize additive variables (sum, round ×5, NA rule ≥50%)
if (length(add)) {
add_final <- lapply(add, function(x) {
ids <- lapply(unique(int$ID), function(id) {
dat <- add_vars[add_vars$ID == id, ]
val <- sum(dat[[x]], na.rm = TRUE)
val <- round(val / 5) * 5
na_pct <- sum(is.na(dat[[x]]) * dat$int_area) / sum(dat$int_area)
if (is.nan(na_pct) || na_pct >= 0.5) val <- NA_real_
out <- tibble::tibble(ID = id, val = val); names(out)[2] <- x; out
})
tibble::as_tibble(Reduce(rbind, ids))
})
add_final <- tibble::as_tibble(Reduce(function(a,b) merge(a,b,by="ID"), add_final))
} else {
add_final <- tibble::tibble()
}

# Merge additive and average outputs
out <- if (nrow(add_final) == 0 && nrow(avg_vars) == 0) {
tibble::tibble(ID = unique(int$ID))
} else if (nrow(add_final) == 0) {
avg_vars
} else if (nrow(avg_vars) == 0) {
add_final
} else {
merge(add_final, avg_vars, by = "ID")
}
out <- tibble::as_tibble(out)

# Clean NaN/Inf -> NA on numeric columns
if (nrow(out)) {
num_cols <- names(out)[sapply(out, is.numeric)]
out[num_cols] <- lapply(out[num_cols], function(col) {
col[is.nan(col)] <- NA_real_
col[is.infinite(col)] <- NA_real_
col
})
}

# Reintegrate non-interpolated units + attach max-year geometry
non_interp <- sf::st_drop_geometry(y_df[is.na(match(y_df$ID, out$ID)), ])
out <- rbind(non_interp, out)

pb()
tibble::as_tibble(merge(out, all_dest[c("ID","geometry")])) |> sf::st_as_sf()
}, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
}, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
}
