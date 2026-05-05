## CMHC neighbourhood name matching table ######################################

# Read CSV
cmhc_nbhd_matching <- readr::read_csv(
  "C:/Users/oussa/Downloads/cmhc_nbhd_renames.csv", 
  show_col_types = FALSE
)

usethis::use_data(cmhc_nbhd_matching, overwrite = TRUE)

