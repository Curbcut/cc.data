#' Build Defavorisation Index for Given Years
#'
#' This function builds the defavorisation dataset for a given set of years.
#' It processes shapefiles containing material and social deprivation indices
#' for Quebec and interpolates the indices to match a new dissemination area (DA) structure.
#'
#' @param years <`character vector`> A vector of years to retrieve the defavorisation data for.
#' Defaults to 2021, 2016, and 2011.
#'
#' @details The defavorisation dataset contains material and social deprivation
#' indices (QuintMatRS and QuintSocRS). The function interpolates previous years'
#' data to match the most recent DA structure and processes missing values.
#' The data used is sourced from the official Quebec deprivation indices provided by
#' Données Québec.
#'
#' @return A `tibble` with the defavorisation indices for each year.
#' Columns include the DA identifier (`ID`), the material deprivation index for
#' each year (`defav_material_YEAR`), and the social deprivation index for each
#' year (`defav_social_YEAR`).
#' @export
build_defavorisation <- function(years = c("2021", "2016", "2011")) {

  # Load the defavorisation shapefiles
  defav <- lapply(years, \(x) {
    cc.data::bucket_read_object_zip_shp(
      object = sprintf("defavorisation_qc_%s.zip", x),
      bucket = "curbcut.rawdata")
  })

  # Rename IDs and fix NAs
  defav <- lapply(defav, \(x) {
    names(x)[names(x) == "ADIDU"] <- "ID"
    x$QuintMatRS <- ifelse(x$QuintMatRS == 0, NA, x$QuintMatRS)
    x$QuintSocRS <- ifelse(x$QuintSocRS == 0, NA, x$QuintSocRS)
    x
  })

  # Interpolate previous years to new DAs
  defav[2:length(defav)] <-
    lapply(defav[2:length(defav)], \(x) {
      pop_col <- grep("^ADPOP", names(x), value = TRUE)
      cc.buildr::interpolate_from_variable(to = defav[[1]][1], from = x,
                                           average_vars = c("QuintMatRS", "QuintSocRS"),
                                           weight_by = pop_col, crs = 32618)
    })

  # Select the dfs
  defav <- lapply(defav, `[`, c("ID", "QuintMatRS", "QuintSocRS"))
  defav <- lapply(defav, sf::st_drop_geometry)

  # Keep it as a quantile.
  defav <- lapply(defav, \(x) {
    x$QuintMatRS <- round(x$QuintMatRS)
    x$QuintSocRS <- round(x$QuintSocRS)
    x
  })

  # Iterate over the elements of the list and rename columns
  names(defav) <- years
  defav <- lapply(names(defav), function(year) {
    df <- defav[[year]] # Extract the dataframe for each year
    colnames(df) <- c("ID", paste0("defav_material_", year), paste0("defav_social_", year))
    df # Return the updated dataframe
  })

  out <- tibble::as_tibble(Reduce(\(x, y) merge(x, y, by = "ID"), defav))

  return(out)
}
