## IMPORT CENSUS VECTORS DETAILS ###############################################

source("data-raw/census_vectors_table.R")

census_vectors_details <-
  lapply(cc.data::census_years, function(year) {

    # Get all the raw vectors of the census year
    census_dataset <- paste0("CA", sub("20", "", year))
    raw_vecs <- cancensus::list_census_vectors(census_dataset)

    # Prepare the table
    year_vec_table <- census_vectors_table
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
vec_table$parent_vec <- lapply(year_vec_table$var_code, function(vc) {

  is_parent <- census_vectors_table$parent[census_vectors_table$var_code == vc]
  if (length(is_parent) == 1 && isTRUE(is_parent)) return(NA)

  parents <- census_vectors_table$parent_vec[census_vectors_table$var_code == vc]

  # parents peut être:
  # - character length 1  (ancien cas)
  # - list length 1 contenant un vecteur (nouveau cas: c_population + c_population_h/f)
  if (length(parents) == 0) return(NA)

  parent_codes <- if (is.list(parents)) parents[[1]] else parents
  if (length(parent_codes) == 0 || all(is.na(parent_codes))) return(NA)

  out <- census_vectors_table[[paste0("vec_", year)]][
    census_vectors_table$var_code %in% parent_codes
  ]

  # garder NA si rien trouvé, sinon vecteur (souvent 1 ou 2 valeurs)
  if (length(out) == 0) return(NA)
  out
})

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
