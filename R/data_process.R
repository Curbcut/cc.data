#' Process raw Canbics data from the AWS bucket
#'
#' @return Returns the Canbics dataset processed country-wide.
#' @export
canbics_process <- function() {

  # Read CANBICS from bucket ------------------------------------------------

  canbics_data <- bucket_read_object(object = "canbics_2021.csv",
                                     bucket = "curbcut.rawdata",
                                     objectext = ".csv",
                                     method = utils::read.csv) |>
    tibble::as_tibble()


  # Manipulate --------------------------------------------------------------

  canbics_data <- canbics_data[, c("nhbic21_01", "nhbic21_09")]
  names(canbics_data) <- c("DA_ID", "canbics_2021")
  class(canbics_data$DA_ID) <- "character"

  canbics_data <- unique(canbics_data)
  canbics_data <- canbics_data[canbics_data$canbics_2021 != -9999.00, ]


  # Return ------------------------------------------------------------------

  return(canbics_data)

}


#' Process raw NO2 data from the AWS bucket
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame
#'
#' @return Returns the NO2 dataset processed country-wide.
#' @export
no2_process <- function(DA_table) {

  # Read NO2 from bucket ----------------------------------------------------

  no2_data <- bucket_read_object(object = "no2_2016.csv",
                                 bucket = "curbcut.rawdata",
                                 objectext = ".csv",
                                 method = utils::read.csv) |>
    tibble::as_tibble()

  pc <- bucket_read_object(object = "postal_codes202103.csv",
                           bucket = "curbcut.rawdata",
                           objectext = ".csv",
                           method = utils::read.csv) |>
    tibble::as_tibble()
  pc <- sf::st_as_sf(pc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  pc$POSTAL_CODE <- gsub(" ", "", pc$POSTAL_CODE)
  pc <- pc[c("POSTAL_CODE", "geometry")]
  names(pc)[1] <- "postalcode16"

  no2_data <- sf::st_as_sf(tibble::as_tibble(merge(no2_data, pc)))

  # Manipulate --------------------------------------------------------------

  no2_data <- no2_data["no2lur16_02"]
  names(no2_data)[1] <- "NO2"
  no2_data <- no2_data[no2_data$NO2 != -9999, ]

  no2_data <- sf::st_transform(no2_data, 3347)
  no2_data <- sf::st_join(no2_data, DA_table)
  no2_data <- sf::st_drop_geometry(no2_data)

  no2_data <- stats::aggregate(NO2 ~ ID, data = no2_data, mean, na.rm = TRUE)
  no2_data <- tibble::as_tibble(no2_data)
  names(no2_data)[1] <- "DA_ID"

  # Return ------------------------------------------------------------------

  return(no2_data)

}


#' Process raw NDVI data from the AWS bucket
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame
#'
#' @return Returns the NDVI dataset processed country-wide.
#' @export
ndvi_process <- function(DA_table) {

  # Read NDVI from bucket ----------------------------------------------------

  ndvi_data <- bucket_read_object(object = "ndvi_2019.csv",
                                 bucket = "curbcut.rawdata",
                                 objectext = ".csv",
                                 method = utils::read.csv) |>
    tibble::as_tibble()

  pc <- bucket_read_object(object = "postal_codes202103.csv",
                           bucket = "curbcut.rawdata",
                           objectext = ".csv",
                           method = utils::read.csv) |>
    tibble::as_tibble()
  pc <- sf::st_as_sf(pc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  pc$POSTAL_CODE <- gsub(" ", "", pc$POSTAL_CODE)
  pc <- pc[c("POSTAL_CODE", "geometry")]
  names(pc)[1] <- "postalcode19"

  ndvi_data <- sf::st_as_sf(tibble::as_tibble(merge(ndvi_data, pc)))

  # Manipulate --------------------------------------------------------------

  ndvi_data <- ndvi_data["grlan19_01"]
  names(ndvi_data)[1] <- "NDVI"
  ndvi_data <- ndvi_data[ndvi_data$NDVI != -9999, ]

  ndvi_data <- sf::st_transform(ndvi_data, 3347)
  ndvi_data <- sf::st_join(ndvi_data, DA_table)
  ndvi_data <- sf::st_drop_geometry(ndvi_data)

  ndvi_data <- stats::aggregate(NDVI ~ ID, data = ndvi_data, mean, na.rm = TRUE)
  ndvi_data <- tibble::as_tibble(ndvi_data)
  names(ndvi_data)[1] <- "DA_ID"

  # Return ------------------------------------------------------------------

  return(ndvi_data)

}
