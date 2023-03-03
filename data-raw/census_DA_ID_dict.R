## CRAETE DA ID DICTIONARY #####################################################

census_DA_years_dict <-
  census_build_DA_years_dictionary(DA_data_raw = data_raw$DA)

usethis::use_data(census_DA_years_dict, overwrite = TRUE)

