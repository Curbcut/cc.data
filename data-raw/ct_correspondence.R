#' @title Generate Census Tract Correspondence Table Between Two periods
#' @description Computes the correspondence between census tracts from two census years at the national level.
#' @param year_1 Character. The target census year (e.g., "CA21").
#' @param year_2 Character. The initial census year for comparison (e.g., "CA1996").
#' @return A tibble containing census tract correspondences with stability indicators.
#' @export
generate_correspondence_table <- function(year_1, year_2) {
  
  # Retrieve all census tracts (CT) for the given years at the national level
  ct_1 <- cancensus::get_census(year_1, regions = list(C = "01"), geo_format = "sf", level = "CT") |>
    dplyr::rename(geouid = GeoUID)
  
  ct_2 <- cancensus::get_census(year_2, regions = list(C = "01"), geo_format = "sf", level = "CT") |>
    dplyr::rename(geouid = GeoUID)
  
  # Compute the area of census tracts
  ct_1 <- ct_1 |> dplyr::mutate(area_1 = units::drop_units(sf::st_area(ct_1)))
  ct_2 <- ct_2 |> dplyr::mutate(area_2 = units::drop_units(sf::st_area(ct_2)))
  
  # Rename `geouid` in ct_2 to avoid conflicts
  names(ct_2)[names(ct_2) == "geouid"] <- "geouid_old"
  
  # Compute spatial intersections between census tracts from both years
  inter <- sf::st_intersection(ct_1, ct_2[c("geouid_old", "area_2")])
  
  if (nrow(inter) == 0) {
    return(NULL)
  }
  
  # Compute new areas after intersection
  inter <- inter |> 
    dplyr::mutate(
      intersected_area = units::drop_units(sf::st_area(inter)),
      proportion_area_1 = intersected_area / area_1,
      proportion_area_2 = intersected_area / area_2
    )
  
  # Filter correspondences where overlap is at least 90%
  correspondence_table <- inter |> 
    dplyr::filter(proportion_area_1 >= 0.9 & proportion_area_2 >= 0.9) |> 
    dplyr::select(geouid, geouid_old) |> 
    dplyr::distinct() |> 
    dplyr::mutate(status = ifelse(geouid == geouid_old, "stable", "changed"))
  
  # Rename `geouid_old` to indicate the census year
  year_short <- substr(year_2, nchar(year_2) - 1, nchar(year_2))
  correspondence_table <- correspondence_table |>
    dplyr::rename(!!paste0("geouid_", year_short) := geouid_old) |>
    dplyr::rename(geouid_21 = geouid)
  
  return(correspondence_table)
}

#' @title Generate Correspondence Tables for All Census Tracts Nationally
#' @description Iterates through multiple census years to create correspondence tables for all census tracts (CTs) at the national level.
#' @return A named list of tibbles, each representing correspondences for a specific census year.
#' @export
generate_national_correspondences <- function() {
  # Define historical census years to compare with CA21
  census_years <- c("CA1996", "CA01", "CA06", "CA11", "CA16")
  
  # Generate correspondence tables for all CTs nationally
  ct_correspondence_list <- setNames(
    lapply(
      census_years,
      function(y) generate_correspondence_table("CA21", y)
    ),
    paste0("correspondence_2021_", gsub("CA", "", census_years))
  )
  
  return(ct_correspondence_list)
}

# Generate the final correspondence list
ct_correspondence_list <- generate_national_correspondences()

usethis::use_data(ct_correspondence_list, overwrite = TRUE)
