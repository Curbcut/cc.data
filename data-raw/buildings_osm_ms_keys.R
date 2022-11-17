## CREATE OSM AND MS KEYS ######################################################

buildings_osm_ms_keys <-
  structure(list(
    osm_link =
      c("http://download.geofabrik.de/north-america/canada/alberta-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/british-columbia-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/manitoba-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/new-brunswick-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/newfoundland-and-labrador-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/northwest-territories-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/nova-scotia-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/nunavut-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/ontario-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/prince-edward-island-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/quebec-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/saskatchewan-latest.osm.pbf",
        "http://download.geofabrik.de/north-america/canada/yukon-latest.osm.pbf"
      ), ms_code =
      c("Alberta", "BritishColumbia", "Manitoba", "NewBrunswick",
        "NewfoundlandAndLabrador", "NorthwestTerritories",
        "NovaScotia", "Nunavut", "Ontario", "PrinceEdwardIsland",
        "Quebec", "Saskatchewan", "YukonTerritory"
      ), addresses =
      c("https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_AB_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_BC_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_MB_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_NB_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_NT_v1.zip",
        NA,
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_NS_v1.zip",
        NA,
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_ON_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_PE_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_QC_v1.zip",
        "https://www150.statcan.gc.ca/n1/pub/46-26-0001/2021001/ODA_SK_v1.zip",
        NA)
  ),
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -13L))

# # Add province code to download addresses
# prov_links <- rvest::read_html("https://www.statcan.gc.ca/en/lode/databases/oda") |>
#   rvest::html_elements("ul") |>
#   rvest::html_elements("li") |>
#   rvest::html_elements("a") |>
#   stringr::str_subset(".zip")
#
# addresses_links_table <-
#   lapply(prov_links, \(x) {
#     link <-
#       rvest::read_html(x) |>
#       rvest::html_element("a") |>
#       rvest::html_attr("href")
#     txt <- rvest::read_html(x) |>
#       rvest::html_text()
#     prov_code <-
#       rvest::read_html(x) |>
#       rvest::html_element("a") |>
#       rvest::html_attr("href") |>
#       stringr::str_extract("(?<=ODA_).*(?=_v)")
#     data.frame(province_code = prov_code, province = txt, link = link)
#   }) |> (\(x) do.call(rbind, x))()

usethis::use_data(buildings_osm_ms_keys, overwrite = TRUE)
