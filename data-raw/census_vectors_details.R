## IMPORT CENSUS VECTORS DETAILS ###############################################

census_vectors_details <-
  lapply(cc.data::census_years, function(year) {

    # Get all the raw vectors of the census year
    census_dataset <- paste0("CA", sub("20", "", year))
    raw_vecs <- cancensus::list_census_vectors(census_dataset)

    # Prepare the table
    year_vec_table <- cc.data::census_vectors_table
    vec_table <- year_vec_table[c("var_code", paste0("vec_", year))]
    names(vec_table) <- c("var_code", "vec")
    vec_table$var_code <- paste(vec_table$var_code, year, sep = "_")

    # Add the vector label
    vec_table$vec_label <-
      sapply(vec_table$vec, \(vecs) {
        if (sum(is.na(vecs)) >= 1) return(NA)
        sapply(vecs, \(vec) {
          raw_vecs$label[raw_vecs$vector == vec]
        }, USE.NAMES = FALSE) |> unique()
      }, simplify = FALSE)

    # Add the parent vector
    vec_table$parent_vec <- sapply(year_vec_table$var_code, \(vecs) {
      parent <- cc.data::census_vectors_table$parent[
        cc.data::census_vectors_table$var_code == vecs
      ]
      if (parent) return(NA)
      parent_string <- cc.data::census_vectors_table$parent_vec[
        cc.data::census_vectors_table$var_code == vecs
      ]
      cc.data::census_vectors_table[[paste0("vec_", year)]][
        cc.data::census_vectors_table$var_code == parent_string
      ]
    }, simplify = FALSE)

    # Add the aggregation
    vec_table$aggregation <-
      sapply(vec_table$vec, \(vecs) {
        if (sum(is.na(vecs)) >= 1) return(NA)
        sapply(vecs, \(vec) {
          raw_vecs$aggregation[raw_vecs$vector == vec]
        }, USE.NAMES = FALSE) |> unique()
      }, simplify = FALSE)

    # Add the parent vector label
    vec_table$parent_vec_label <-
      sapply(vec_table$parent_vec, \(vecs) {
        if (sum(is.na(vecs)) >= 1) return(NA)
        sapply(vecs, \(vec) {
          raw_vecs$label[raw_vecs$vector == vec][[1]]
        }, USE.NAMES = FALSE) |> unique()
      }, simplify = FALSE)

    # Return the table
    return(vec_table)

  })
census_vectors_details <- data.table::rbindlist(census_vectors_details)
census_vectors_details <- tibble::as_tibble(census_vectors_details)

usethis::use_data(census_vectors_details, overwrite = TRUE)
