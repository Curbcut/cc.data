# Accessible here https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/index-eng.cfm
road_network_url <-
  tibble::tibble(year = c(2001, 2006, 2011, 2016, 2021),
                 url = c("https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grnf000r01a_e.zip",
                         "https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grgf000r06a_e.zip",
                         "https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/grnf000r11a_e.zip",
                         "https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/2016/lrnf000r16a_e.zip",
                         "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/RNF-FRR/files-fichiers/lrnf000r21a_e.zip"))

usethis::use_data(road_network_url, overwrite = TRUE)
