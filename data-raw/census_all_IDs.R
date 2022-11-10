## CRAETE LIST OF ALL CENSUS IDS DICTIONARY ####################################

census_all_ids <-
  sapply(cc.data::census_scales, \(x) years_reduced[[x]]$ID)

usethis::use_data(census_all_ids, overwrite = TRUE)
