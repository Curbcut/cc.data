#' Process raw postal codes from the AWS bucket
#'
#' @param DA_processed_table <`sf data.frame`> A \code{DA} sf data.frame from
#' which the DA ID will be attached to every single building.
#'
#' @return An `sf` data.frame of 3 columns: postal code, dissemination area ID
#' and the centroid point geometry of every postal code in the country.
#' @export
postal_codes_process <- function(DA_processed_table) {

  # Read postal codes from bucket -------------------------------------------

  pc <- bucket_read_object(object = "postal_codes202103.csv",
                           bucket = "curbcut.rawdata",
                           objectext = ".csv",
                           method = utils::read.csv) |>
    tibble::as_tibble()


  # Manipulate --------------------------------------------------------------

  pc <- sf::st_as_sf(pc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  pc <- pc[c("POSTAL_CODE", "geometry")]
  names(pc)[1] <- "postal_code"

  pc$postal_code <- tolower(pc$postal_code)
  pc$postal_code <- gsub("\\s", "", pc$postal_code)

  pc <- sf::st_transform(pc, 3347)


  # Bind DA ID --------------------------------------------------------------

  pc <- sf::st_join(pc, DA_processed_table["ID"])
  names(pc) <- c("postal_code", "geometry", "DA_ID")
  pc <- pc[c("postal_code", "DA_ID", "geometry")]


  # Return ------------------------------------------------------------------

  return(pc)

}
