#' Retrieve Urban Containment Boundaries (UCB) for British Columbia Regions
#'
#' This function downloads the Urban Containment Boundaries (UCB) for various
#' regions in the province of British Columbia.
#'
#' @return A list containing UCB data for the Capital Region District (`crd`),
#' Metro Vancouver (`metro_van`), and Nanaimo (`nanaimo`).
#' @export
BC_UCB <- function(crs = 32610) {

  # THERE ARE 10 ADOPTED REGIONAL GROWTH STRATEGY IN THE PROVINCE
  # https://www2.gov.bc.ca/gov/content/governments/local-governments/planning-land-use/local-government-planning/regional-growth-strategies/status-of-regional-growth-strategies


  # For the Capital Region District, download the UCB directly from their
  # ArcGIS REST Services Directory
  crd <- arcgis_rest_services_ret(paste0("https://mapservices.crd.bc.ca/arcgis",
                                         "/rest/services/Root/RegionalGrowthSt",
                                         "rategy/MapServer/1"))
  crd <- sf::st_transform(crd, crs)

  # Metro Vancouver
  metro_van <- bucket_read_object_zip_shp("Metro_2050_Urban_Containment_Boundary.zip",
                                          "curbcut.bc.zoning")
  metro_van <- sf::st_transform(metro_van, crs)

  # Nanaimo
  nanaimo <- bucket_read_object_zip_shp("RGS_ContainmentBoundaries_97.zip",
                                        "curbcut.bc.zoning")
  nanaimo <- sf::st_transform(nanaimo, crs)


  merged <- Reduce(rbind, list(crd["geometry"],
                     metro_van["geometry"],
                     nanaimo["geometry"]))
  merged <- sf::st_cast(merged, "MULTIPOLYGON")

  return(merged)
}
