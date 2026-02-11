## IMPORT CT IDs for all CMAs in Canada ################################################

cma_ct_ids <- cancensus::get_census(dataset='CA21', regions=list(C="01"),
                          level='CT', use_cache = T, geo_format = NA, quiet = TRUE) |>
               dplyr::select(id = GeoUID, cma_uid = CMA_UID)


usethis::use_data(cma_ct_ids, overwrite = TRUE)

