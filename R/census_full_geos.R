#' Append Geospatial Data to Processed Census Data
#'
#' This function retrieves the geospatial data for each scale in the
#' processed census data from a specified bucket, merges this data
#' with the existing census data, and returns the combined data set.
#'
#' @param processed_census <`named list`> A list of data frames, each containing
#' census data at a different scale.
#'
#' @return A list of spatial data frames, each containing the
#' merged census and geospatial data for a particular scale.
#' @export
census_full_geos <- function(processed_census) {

  # For how many scales do we need to switch the geometries
  scales <- seq_along(processed_census)

  # Iterate over all of them
  geos_appended <- future.apply::future_lapply(scales, \(n) {
    # Get the scale name
    scale_name <- names(processed_census)[n]

    # From the bucket, grab the geometries of the scale
    new_geos <- bucket_read_object_zip_shp(object = sprintf("%s_shp.zip", scale_name),
                                           bucket = "curbcut.rawdata")
    new_geos <- new_geos[1]
    ID <- names(new_geos)[1]

    # Get the current df and merge the new geometries
    df <- processed_census[[n]]
    df <- sf::st_drop_geometry(df)
    df <- merge(df, new_geos, by.x = "ID", by.y = ID) |>
      tibble::as_tibble() |>
      sf::st_as_sf()

    return(df)
  }, future.seed = NULL)

  # Use the same name as previously
  names(geos_appended) <- names(processed_census)

  return(geos_appended)
}
