#' Get census vectors table for a given set of vectors
#'
#' Given a set of census vectors, this function retrieves the corresponding
#' entries and the parent entries in the census vectors table. The output is a
#' data frame with one row per variable and several columns with information
#' about the variable.
#'
#' @param census_vectors <`character`> One or more vectors from
#' \code{\link[cc.data]{census_vectors}}
#'
#' @return A data frame with the same columns as the census vectors table, but
#' filtered to include only the variables specified in \code{census_vectors} and
#' their parent vectors.
#' @export
census_get_vectors_table <- function(census_vectors) {
  # Get the variable code of the parent vectors
  all_vectors <- census_add_parent_vectors(census_vectors)

  # Output the full table
  census_vectors_table <- cc.data::census_vectors_table[
    cc.data::census_vectors_table$var_code %in% all_vectors,
  ]

  return(census_vectors_table)
}

#' Add parent vectors to a set of census vectors
#'
#' Given a set of census vectors, this function retrieves the corresponding
#' parent vectors from the census vectors table and adds them to the output.
#'
#' @param census_vectors <`character`> One or more vectors from
#' \code{\link[cc.data]{census_vectors}}
#'
#' @return A character vector with the same elements as \code{census_vectors},
#' but also including the parent vectors of those elements, if any.
#' @export
census_add_parent_vectors <- function(census_vectors) {
  # Get the variable code of the parent vectors
  parent_vecs <-
    cc.data::census_vectors_table$parent_vec[
      cc.data::census_vectors_table$var_code %in% census_vectors
    ]

  # Make a vector of both the interested vectors with the parents
  all_vectors <- unique(c(census_vectors, parent_vecs))
  all_vectors <- all_vectors[!is.na(all_vectors)]

  # Return both the census_vectors with the parent vectors in the same string
  return(all_vectors)
}
