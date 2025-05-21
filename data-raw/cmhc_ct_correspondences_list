## Generate Correspondence Tables for CT(s) Between Two Census Periods (e.g., 1996 and 2021)

census_ct_correspondences <- function(census_years = c("CA1996", "CA01", "CA06", "CA11", "CA16"),
                                    cma_codes = NULL,
                                    reference_year = "CA21") {
  
  if (is.null(cma_codes)) {
    cma_codes <- cancensus::list_census_regions(reference_year) |>
      dplyr::filter(level == "CMA") |>
      dplyr::pull(region)
  }
    ct_21_list <- list()
  for (cma in cma_codes) {
    ct_21 <- tryCatch({
      cancensus::get_census(reference_year, regions = list(CMA = cma), geo_format = "sf", level = "CT") |>
        dplyr::rename(geouid = GeoUID)
    }, error = function(e) {
      message("Skipped CA21 for CMA ", cma, ": ", e$message)
      return(NULL)
    })
    ct_21_list[[cma]] <- ct_21
  }
  
  generate_correspondence_table <- function(ct_21, year_cmp, cma_code) {
    message("→ Comparing CA21 to ", year_cmp, " for CMA ", cma_code)
    
    if (is.null(ct_21)) return(NULL)
    
    ct_cmp <- tryCatch({
      cancensus::get_census(year_cmp, regions = list(CMA = cma_code), geo_format = "sf", level = "CT") |>
        dplyr::rename(geouid = GeoUID)
    }, error = function(e) {
      message("Skipped CMA ", cma_code, " for ", year_cmp, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(ct_cmp)) return(NULL)
    
    ct_21 <- ct_21 |> dplyr::mutate(area_1 = units::drop_units(sf::st_area(ct_21)))
    ct_cmp <- ct_cmp |> dplyr::mutate(area_2 = units::drop_units(sf::st_area(ct_cmp)))
    
    names(ct_cmp)[names(ct_cmp) == "geouid"] <- "geouid_old"
    
    inter <- sf::st_intersection(ct_21, ct_cmp[c("geouid_old", "area_2")])
    if (nrow(inter) == 0) return(NULL)
    
    inter <- inter |>
      dplyr::mutate(
        intersected_area = units::drop_units(sf::st_area(inter)),
        proportion_area_1 = intersected_area / area_1,
        proportion_area_2 = intersected_area / area_2
      )
    
    correspondence_table <- inter |>
      dplyr::filter(proportion_area_1 >= 0.9 & proportion_area_2 >= 0.9) |>
      dplyr::select(geouid, geouid_old) |>
      dplyr::distinct() |>
      dplyr::mutate(status = ifelse(geouid == geouid_old, "stable", "changed"))
    
    year_ref_num <- substr(gsub("CA", "", reference_year), nchar(gsub("CA", "", reference_year)) - 1, nchar(gsub("CA", "", reference_year)))
    year_cmp_num <- substr(gsub("CA", "", year_cmp), nchar(gsub("CA", "", year_cmp)) - 1, nchar(gsub("CA", "", year_cmp)))
    
    correspondence_table <- correspondence_table |>
      dplyr::rename(
        !!paste0("geouid_", year_ref_num) := geouid,
        !!paste0("geouid_", year_cmp_num) := geouid_old
      ) |>
      dplyr::mutate(cma_code = cma_code) |>
      dplyr::select(
        paste0("geouid_", year_ref_num),
        paste0("geouid_", year_cmp_num),
        "status",
        "cma_code"
      )
    
    return(correspondence_table)
  }
  
  result <- list()
  for (year in census_years) {
    temp_list <- list()
    for (cma_code in cma_codes) {
      ct_21 <- ct_21_list[[cma_code]]
      table <- generate_correspondence_table(ct_21, year, cma_code)
      if (!is.null(table)) {
        temp_list[[cma_code]] <- table
      }
    }
    
    if (length(temp_list) > 0) {
      combined_table <- dplyr::bind_rows(temp_list)
      
      year_ref_label <- ifelse(nchar(gsub("CA", "", reference_year)) == 2,
                               paste0("20", gsub("CA", "", reference_year)),
                               gsub("CA", "", reference_year))
      year_cmp_label <- ifelse(nchar(gsub("CA", "", year)) == 2,
                               paste0("20", gsub("CA", "", year)),
                               gsub("CA", "", year))
      
      name <- paste0("correspondence_", year_ref_label, "_", year_cmp_label)
      result[[name]] <- combined_table
    }
  }
  
  return(result)
}

census_ct_correspondences_list <- census_ct_correspondences()

usethis::use_data(census_ct_correspondences_list, overwrite = TRUE)

