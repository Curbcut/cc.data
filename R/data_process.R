#' Read and Process Spatial Data
#'
#' This function reads and processes spatial data related to postal codes and specific
#' data values, mostly data coming from CANUE. It performs a series of transformations
#' and aggregations, resulting in a tibble with the average values grouped by DA ID.
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame
#' @param bucket <`character`> The bucket from which to grab the data. Defaults
#' to `"curbcut.rawdata"`.
#' @param bucket_object <`character`> The object in the bucket to grab and read
#' data values from. e.g. `"no2_2016.csv"`.
#' @param objectext <`character`> Extension of the bucket_object. Defaults to `".csv"`.
#' @param col_val <`character`> The name of the column which holds the data in the
#' bucket_object.
#' @param value_name <`character`> The name of the column of the output value.
#' e.g. `"NO2"`.
#'
#'
#' @return A tibble containing the processed data, including the aggregated values.
#'
#' @export
read_and_process_data_canue <- function(DA_table, bucket = "curbcut.rawdata",
                                         bucket_object, objectext = ".csv", col_val,
                                         value_name) {
  # Read Postal Code
  pc_data <- bucket_read_object(object = "postal_codes202103.csv",
                                bucket = "curbcut.rawdata",
                                objectext = ".csv",
                                method = utils::read.csv) |>
    tibble::as_tibble()

  pc_data <- sf::st_as_sf(pc_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  pc_data$POSTAL_CODE <- gsub(" ", "", pc_data$POSTAL_CODE)
  pc_data <- pc_data[c("POSTAL_CODE", "geometry")]
  names(pc_data)[1] <- "postalcode"

  # Read data from bucket
  from_bucket <- bucket_read_object(object = bucket_object,
                                    bucket = bucket,
                                    objectext = objectext,
                                    method = utils::read.csv)
  from_bucket <- tibble::as_tibble(from_bucket)
  names(from_bucket)[grepl("postalcode", names(from_bucket))] <- "postalcode"
  data_values <- merge(from_bucket, pc_data)
  data_values <- tibble::as_tibble(data_values)
  data_values <- sf::st_as_sf(data_values)


  # Manipulate
  data_values <- data_values[col_val]
  names(data_values)[1] <- value_name
  data_values <- data_values[data_values[[value_name]] != -9999, ]

  data_values <- sf::st_transform(data_values, 3347)
  data_values <- sf::st_join(data_values, DA_table)
  data_values <- sf::st_drop_geometry(data_values)

  data_values <- stats::aggregate(as.formula(paste(value_name, "~ ID")),
                                  data = data_values,
                                  mean, na.rm = TRUE)
  data_values <- tibble::as_tibble(data_values)
  names(data_values)[1] <- "DA_ID"

  # Return
  return(data_values)
}

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

  read_and_process_data_canue(DA_table = DA_table,
                        bucket_object = "no2_2016.csv",
                        col_val = "no2lur16_02",
                        value_name = "NO2")

}

#' Process Land Surface Temperature Data Across Years
#'
#' This function processes land surface temperature data across specified years by reading
#' the data using the \code{read_and_process_data_canue} function. It performs
#' a series of transformations and merges the data across different years
#' resulting in a tibble with the land surface values grouped by DA ID.
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame that contains
#' the Dissemination Area (DA) information for spatial joins.
#'
#' @return A tibble containing the processed land surface data, including the merged
#' values across the specified years.
#' @export
lst_process <- function(DA_table) {

  progressr::with_progress({
    pb <- progressr::progressor(length(15:21))

    # Grab all the years from the bucket
    out <- future.apply::future_lapply(15:21, \(x) {
      out <- read_and_process_data_canue(
        DA_table = DA_table,
        bucket_object = sprintf("land_surface_temp/wtlst_ava_%s.csv", x),
        col_val = sprintf("wtlst%s_01", x),
        value_name = sprintf("lst_20%s", x))
      pb()
      return(out)
    })
  })

  # Merge on the same tibble
  merged <- Reduce(\(x, y) merge(x, y, by = "DA_ID", all = TRUE), out)
  merged <- tibble::as_tibble(merged)

  return(merged)

}

#' Process Modis - Greeness Data Across Years
#'
#' This function processes Modis - Greeness data across specified years by reading
#' the data using the \code{read_and_process_data_canue} function. It performs
#' a series of transformations and merges the data across different years
#' resulting in a tibble with the land surface values grouped by DA ID.
#'
#' @param DA_table <`sf data.frame`> A \code{DA} sf data.frame that contains
#' the Dissemination Area (DA) information for spatial joins.
#'
#' @return A tibble containing the processed land surface data, including the merged
#' values across the specified years.
#' @export
ndvi_process <- function(DA_table) {

  progressr::with_progress({
    pb <- progressr::progressor(length(0:22))

    # Grab all the years from the bucket
    out <- future.apply::future_lapply(sprintf("%02d", 0:22), \(x) {
      out <- read_and_process_data_canue(
        DA_table = DA_table,
        bucket_object = sprintf("ndvi_modis/grmod_amx_%s.csv", x),
        col_val = sprintf("grmod%s_06", x),
        value_name = sprintf("ndvi_20%s", x))
      pb()
      return(out)
    })
  })

  # Merge on the same tibble
  merged <- Reduce(\(x, y) merge(x, y, by = "DA_ID", all = TRUE), out)
  merged <- tibble::as_tibble(merged)

  return(merged)

}
