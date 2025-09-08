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
      interpolate_from_variable(to = defav[[1]][1], from = x,
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

#' Interpolate variables using a custom weighting factor, with the weighting
#' factor itself interpolated based on area proportions
#'
#' @param to <`sf data.frame`> Table for which data must be interpolated
#' @param from <`sf data.frame`> A \code{DA} sf data.frame from which
#' variables will be interpolated.
#' @param average_vars <`character vector`> Corresponds to the column names
#' of the variables that are to be interpolated as an average, like a percentage,
#' a median, an index, etc. weighted by the `weight_by` argument.
#' @param additive_vars <`character vector`> Corresponds to the column names of
#' the variables that are 'count' variables.
#' @param weight_by <`character`> Column name in `from` data to be used for weighting
#' the interpolation of both average and additive variables.
#' @param round_additive <`logical`> If additive variables should be rounded,
#' e.g. the population or count of households.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32618} for Montreal.
#'
#' @return Returns the `to` data.frame with the added or modified columns that
#' have been interpolated from the `from`.
interpolate_from_variable <- function(to, from,
  average_vars = c(),
  additive_vars = c(),
  weight_by,
  round_additive = TRUE,
  crs) {
# Ensure that the weight_by column exists
if (!(weight_by %in% names(from))) {
stop(paste("The weight_by column", weight_by, "is missing from the `from` table."))
}

# Interpolate the `from` table to get additive variables, and interpolate that
# variable to do the other interpolation.
das <- from[, c(average_vars, additive_vars, weight_by)]
das <- sf::st_transform(das, crs)
das <- sf::st_set_agr(das, "constant")
# Add DA area
das$DA_area <- get_area(das$geometry)
# Add new table area
destination <- sf::st_transform(to, crs)
destination <-
destination[, names(destination)[!names(destination) %in% additive_vars]]
intersected_table <- suppressWarnings(sf::st_intersection(destination, das))
intersected_table$new_area <- get_area(intersected_table$geometry)
# Get proportion of area per zone
intersected_table$area_prop <-
intersected_table$new_area / intersected_table$DA_area
intersected_table <- sf::st_drop_geometry(intersected_table)


# Calculate weighted proportion using the provided weight_by
intersected_table$weight_by_prop <- intersected_table[[weight_by]] * intersected_table$area_prop

summarized_avg <- lapply(average_vars, \(col_name) {
col_df <- intersected_table[c("ID", "weight_by_prop", col_name)]
interpolate_fast_weighted_mean(col_df, "ID", "weight_by_prop", col_name)
})

summarized_add <- lapply(additive_vars, interpolate_fast_additive_sum,
data = intersected_table, id_col = "ID",
weight_col = "weight_by_prop", .round = round_additive
)

# Concatenate both
summarized <- c(summarized_avg, summarized_add)

out <- if (length(summarized) > 1) {
merg_ <- function(x, y) {
x <- x[!is.na(x$ID), ]
y <- y[!is.na(y$ID), ]
if (identical(x$ID, y$ID)) {
cbind(x, y[2])
} else {
merge(x, y, by = "ID", all = TRUE)
}
}
Reduce(merg_, summarized)
} else {
summarized[[1]]
}

# Return
merge(to[, names(to)[!names(to) %in% c(additive_vars, average_vars)]], out,
by = "ID", all.x = TRUE
)
}

#' Fast Weighted Mean for Interpolation
#'
#' This function computes the weighted mean for each group of IDs interpolated
#' within a data frame. The data frame must have columns for ID, weight, and
#' value. The output is a data frame containing the IDs and their corresponding
#' weighted means.
#'
#' @param df <`data.frame`> A data frame containing the data to be interpolated.
#' It must have columns corresponding to the specified id_col, weight_col, and
#' value_col.
#' @param id_col <`character`> A string representing the name of the ID column
#' in the input data frame.
#' @param weight_col <`character`> A string representing the name of the weight
#' column in the input data frame.
#' @param value_col <`character`> A string representing the name of the value
#' column in the input data frame.
#'
#' @return A data frame with two columns: the ID column (with the same name as
#' in the input data frame) and a column containing the weighted means, named
#' after the input value_col
interpolate_fast_weighted_mean <- function(df, id_col, weight_col, value_col) {

  # Remove rows with NA in value_col and weight_col
  df <- df[!is.na(df[[value_col]]) & !is.na(df[[weight_col]]), ]

  # Calculate the sum of products of value and weight for each group
  sum_val_times_weight <- tapply(df[[value_col]] * df[[weight_col]], df[[id_col]], sum, na.rm = TRUE)

  # Calculate the sum of weights for each group
  sum_weight <- tapply(df[[weight_col]], df[[id_col]], sum, na.rm = TRUE)

  # Compute weighted mean
  weighted_mean <- sum_val_times_weight / sum_weight

  # Create the result as a data frame
  result <- data.frame(ID = names(weighted_mean),
                       avg = as.numeric(weighted_mean))
  names(result)[2] <- value_col

  return(result)
}

#' Fast Summation for Interpolation
#'
#' This function computes the the summation for each group within a
#' data frame. The data frame must have columns for ID and the column to be
#' summed. The output is a data frame containing the IDs and their corresponding
#' summed values.
#'
#' @param col_name <`character`> A string representing the name of the column
#' to be summed in the input data frame.
#' @param data <`data.frame`> A data frame containing the data to be summed. It
#' must have columns corresponding to the specified id_col and col_name.
#' @param id_col <`character`> A string representing the name of the ID column
#' in the input data frame.
#' @param weight_col <`character`> A string representing the name of the weight
#' column in the input data frame. Used only when the weight_col is using areas.
#' Defaults to NULL.
#' @param .round <`logical`> If variables should be rounded,
#' e.g. the population or count of households.
#'
#' @return A data frame with two columns: the ID column (with the same name as
#' in the input data frame) and a column containing the summed values, named
#' after the input col_name.
interpolate_fast_additive_sum <- function(col_name, data, id_col,
  weight_col = NULL, .round = FALSE) {
# Remove rows with NA in col_name and weight_col (if weight_col is not NULL)
nas <- if (!is.null(weight_col)) {
which(is.na(data[[col_name]]) & is.na(data[[weight_col]]))
} else {
which(is.na(data[[col_name]]))
}
if (length(nas) > 0) data <- data[-nas, ]

# Weight the column if weight_col is provided
if (!is.null(weight_col)) {
weighted_col <- data[[col_name]] * data[[weight_col]]
} else {
weighted_col <- data[[col_name]]
}

# Calculate the sum for each group using tapply
summed_data <- tapply(weighted_col, data[[id_col]], sum, na.rm = TRUE)
if (.round) summed_data <- round(summed_data)

# Convert to a data frame and set column names
result <- data.frame(ID = names(summed_data),
Sum = as.numeric(summed_data))
names(result)[2] <- col_name

# Return the results as a data frame
return(result)

}
