## VECTORS OF ALL CENSUS VECTORS ###############################################

source("data-raw/census_vectors_table.R")

census_vectors <- census_vectors_table$var_code

usethis::use_data(census_vectors, overwrite = TRUE)
