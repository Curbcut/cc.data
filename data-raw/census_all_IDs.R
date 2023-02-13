## CRAETE LIST OF ALL CENSUS IDS DICTIONARY ####################################

census_all_ids <-
  sapply(cc.data::census_scales, \(x) cancensus::get_census(
    dataset = paste0("CA",
                     cc.data::census_years[length(cc.data::census_years)] %% 100),
    regions = list(C = "01"),
    level = x,
    quiet = TRUE
  )$GeoUID)

usethis::use_data(census_all_ids, overwrite = TRUE)
