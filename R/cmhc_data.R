#' @title Split Ottawa-Gatineau CMA by Province
#' @description This function retrieves census metropolitan areas (CMAs) and provinces for a given census year,
#' identifies Ottawa-Gatineau (B), and splits it into two separate CMAs: one for Ontario and one for Quebec.
#' The function assigns appropriate names and GeoUIDs to the split regions.
#'
#' @param census_year Character. The census year (e.g., "CA21").
#' @return A spatial dataframe (sf object) containing CMAs with Ottawa-Gatineau split by province.
#' @export

census_year <- "CA21"

cma_list <- cancensus::list_census_regions(census_year) |>
  dplyr::filter(level == "CMA") |>
  dplyr::select(region, name)

province_list <- cancensus::list_census_regions(census_year) |>
  dplyr::filter(level == "PR") |>
  dplyr::select(region, name)

# Retrieve census data for CMAs and provinces
cma_all <- cancensus::get_census(dataset = census_year, 
                                 regions = list(CMA = cma_list$region), 
                                 geo_format = "sf", 
                                 level = "CMA")

provinces_sf <- cancensus::get_census(dataset = census_year, 
                                      regions = list(PR = province_list$region), 
                                      geo_format = "sf", 
                                      level = "PR")

# Identify Ottawa-Gatineau CMA and corresponding provinces
ottawa_cma <- dplyr::filter(cma_all, name == "Ottawa - Gatineau (B)")
ottawa_provinces <- dplyr::filter(provinces_sf, name %in% c("Ontario (Ont.)", "Quebec (Que.)"))

# Split Ottawa-Gatineau into Ontario and Quebec components if both exist
if (nrow(ottawa_cma) > 0 & nrow(ottawa_provinces) > 0) {
  ottawa_split <- sf::st_intersection(ottawa_cma, ottawa_provinces) |>
    dplyr::mutate(
      name = dplyr::case_when(
        name.1 == "Ontario (Ont.)" ~ "Ottawa - Ontario (B)",
        name.1 == "Quebec (Que.)" ~ "Ottawa - Québec (B)"
      ),
      GeoUID = dplyr::case_when( # Assign appropriate GeoUIDs
        name.1 == "Ontario (Ont.)" ~ "35505",
        name.1 == "Quebec (Que.)" ~ "24505"
      )
    ) |>
    dplyr::select(GeoUID, name, geometry) 
  
  # Remove the original Ottawa-Gatineau entry and add the split regions
  cma_all <- dplyr::filter(cma_all, name != "Ottawa - Gatineau (B)") |>
    dplyr::bind_rows(ottawa_split)
}

# Rename and select final columns
cma_all <- cma_all |>
  dplyr::rename(geouid = GeoUID) |> 
  dplyr::select(geouid, name, geometry) |> 
  dplyr::arrange(name)


