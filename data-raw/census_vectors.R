## VECTORS OF ALL CENSUS VECTORS ###############################################

source("data-raw/census_vectors_table.R")

# Only show non-parent variables
non_parent_vecs <- census_vectors_table$var_code[!census_vectors_table$parent]

census_vectors <- non_parent_vecs

usethis::use_data(census_vectors, overwrite = TRUE)
