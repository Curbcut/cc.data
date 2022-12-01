#' Process raw Canbics data from the AWS bucket
#'
#' @return Returns the Canbics dataset processed country-wide.
#' @export
canbics_process <- function() {

  # Read postal codes from bucket -------------------------------------------

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
