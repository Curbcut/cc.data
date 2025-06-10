## Generate Correspondence Tables for CSD(s) Between Two Census Periods (e.g., 1996 and 2021)

correspondence_single_year <- function(
  csd_ref,
  csd_cmp,
  reference_year,
  year_cmp
) {
# Ensure geometries are valid and area columns exist
make_and_check_valid <- function(sf_obj) {
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf_obj[sf::st_is_valid(sf_obj) & !sf::st_is_empty(sf_obj), ]
  # Buffer 0 can fix some edge-cases (self-touches, ring direction, slivers)
  sf_obj$geometry <- sf::st_buffer(sf_obj$geometry, 0)
  sf_obj <- sf_obj[sf::st_is_valid(sf_obj) & !sf::st_is_empty(sf_obj), ]
  sf_obj
}
csd_ref <- make_and_check_valid(csd_ref)
csd_cmp <- make_and_check_valid(csd_cmp)

csd_ref$area_1 <- cc.data::get_area(csd_ref)
csd_cmp$area_2 <- cc.data::get_area(csd_cmp)

make_and_check_valid <- function(sf_obj) {
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf_obj[sf::st_is_valid(sf_obj) & !sf::st_is_empty(sf_obj), ]
  # Buffer 0 can fix some edge-cases (self-touches, ring direction, slivers)
  sf_obj$geometry <- sf::st_buffer(sf_obj$geometry, 0)
  sf_obj <- sf_obj[sf::st_is_valid(sf_obj) & !sf::st_is_empty(sf_obj), ]
  sf_obj
}
csd_ref <- make_and_check_valid(csd_ref)
csd_cmp <- make_and_check_valid(csd_cmp)

ints <- sf::st_intersects(csd_ref, csd_cmp)
pairs <- data.frame(
  ref_idx = rep(seq_along(ints), lengths(ints)),
  cmp_idx = unlist(ints)
)
if (nrow(pairs) == 0) return(NULL)

# Prepare vectors for parallelization
ref_idx <- pairs$ref_idx
cmp_idx <- pairs$cmp_idx

# Setup for progress reporting
result <- progressr::with_progress({
  p <- progressr::progressor(along = ref_idx)
  future.apply::future_mapply(
    function(i, j) {
      ref_geom <- csd_ref[i, ]
      cmp_geom <- csd_cmp[j, ]
      p()
      out <- tryCatch(
        {
          inter <- sf::st_intersection(
            sf::st_geometry(ref_geom),
            sf::st_geometry(cmp_geom)
          )
          
          # Ne garder que les POLYGON et MULTIPOLYGON
          inter <- inter[
            sf::st_geometry_type(inter) %in% c("POLYGON", "MULTIPOLYGON")
          ]
          
          if (length(inter) == 0) return(NULL)
          
          # Corriger les géométries invalides si nécessaire
          inter <- sf::st_make_valid(inter)
          
          # Union de toutes les géométries pour une seule entité, puis cast en MULTIPOLYGON
          inter <- sf::st_union(inter)
          inter <- sf::st_cast(inter, "MULTIPOLYGON")
          
          # Calcul des aires
          intersected_area <- get_area(inter)
          area_1 <- ref_geom$area_1
          area_2 <- cmp_geom$area_2
          
          # Proportions d'intersection
          prop_1 <- intersected_area / area_1
          prop_2 <- intersected_area / area_2
          
          # Retour conditionnel
          if (prop_1 >= 0.9 && prop_2 >= 0.9) {
            tibble::tibble(
              geouid = ref_geom$GeoUID,
              geouid_old = cmp_geom$GeoUID,
              status = ifelse(
                ref_geom$GeoUID == cmp_geom$GeoUID,
                "stable",
                "changed"
              ),
              ref_year = reference_year,
              cmp_year = year_cmp
            )
          } else {
            NULL
          }
        },
        error = function(e) {
          # Optionally log error, e.g. print(e$message)
          NULL
        }
      )
      out
    },
    ref_idx,
    cmp_idx,
    future.seed = TRUE,
    SIMPLIFY = FALSE
  )
})

# Bind the non-null results
out <- do.call(rbind, result)
if (is.null(out) || nrow(out) == 0) return(NULL)
year_ref_num <- sub("^CA", "", reference_year)
year_cmp_num <- sub("^CA", "", year_cmp)
names(out)[names(out) == "geouid"] <- paste0("geouid_", year_ref_num)
names(out)[names(out) == "geouid_old"] <- paste0("geouid_", year_cmp_num)
out
}

cmhc_csd_correspondences <- function(census_years, reference_year) {
csd_ref <- cancensus::get_census(reference_year, regions = list(C = "01"), geo_format = "sf", level = "CSD")
csd_ref <- csd_ref[, c("GeoUID", "geometry")]
names(csd_ref)[names(csd_ref) == "GeoUID"] <- "GeoUID" # enforce

results <- list()
for (year_cmp in census_years) {
  csd_cmp <- cancensus::get_census(year_cmp, regions = list(C = "01"), geo_format = "sf", level = "CSD")
  csd_cmp <- csd_cmp[, c("GeoUID", "geometry")]
  out <- correspondence_single_year(csd_ref, csd_cmp, reference_year, year_cmp)
  if (!is.null(out)) {
    results[[paste0("correspondence_", sub("^CA", "", reference_year), "_", sub("^CA", "", year_cmp))]] <- out
  }
}
results
}

census_csd_correspondences_list <- cmhc_csd_correspondences(
  census_years = c("CA1996", "CA01", "CA06", "CA11", "CA16"),
  reference_year = "CA21"
)

usethis::use_data(census_csd_correspondences_list, overwrite = TRUE)
