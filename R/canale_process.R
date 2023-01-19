#' Process raw Can-ALE data from the AWS bucket
#'
#' @return Returns the Can-ALE dataset processed country-wide.
#' @export
canale_process <- function() {

  # Read CANALE from bucket -------------------------------------------------

  canale_data <- bucket_read_object(object = "canale_2016.csv",
                                    bucket = "curbcut.rawdata",
                                    objectext = ".csv",
                                    method = utils::read.csv) |>
    tibble::as_tibble()


  # Manipulate --------------------------------------------------------------

  canale_data <- canale_data[, c("DAUID", "ale_index")]
  names(canale_data) <- c("DA_ID", "canale_2016")
  class(canale_data$DA_ID) <- "character"


  # Return ------------------------------------------------------------------

  return(canale_data)

}
