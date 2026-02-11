## IMPORT CMAs IDs across Canada ################################################

cma_ids <- cc.pipe::get_census_digital_scales(scales = "cma")$cmasplit |>
  dplyr::distinct(id) |>
  dplyr::mutate(id = as.character(id))

usethis::use_data(cma_ids, overwrite = TRUE)

