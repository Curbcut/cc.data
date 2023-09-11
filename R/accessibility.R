#' Count of point data per DA
#'
#' The function reads DMTI data from the AWS bucket and classifies each point
#' using sic code. It then sums each variable for every DA.
#'
#' @param DA_table <`sf data.frame`> A data.frame listing most recent DA ID with
#' spatial features.
#'
#' @return A list of two element. First is the count of each accessibility variable
#' for every DA. Second is a dictionary, with the industry title and explanation
#' for every variable code.
#' @export
accessibility_DA_location <- function(DA_table) {

  if (!requireNamespace("matchr", quietly = TRUE)) {
    stop(
      "Package \"matchr\" must be installed to use this function. UPGo-McGill/matchr.",
      call. = FALSE
    )
  }

  if (!all(names(DA_table) == c("ID", "geometry"))) {
    stop("`DA_table` must have only the columns `ID` and `geometry`.")
  }

  # Read DMTI data from bucket ----------------------------------------------

  # Prepare to grab poi data from the bucket
  files <- bucket_list_content("curbcut.amenities")$Key
  txt_shp <- files[grepl("poi/.*(txt$|zip$)", files)]
  all_years <- unique(stringr::str_extract(txt_shp, "\\d{4}"))
  years <- all_years[which(all_years == max(all_years))]
  pois <- txt_shp[grepl(years, txt_shp)]

  # Read the shapefiles
  poi <- lapply(pois, \(f) {
    bucket_read_object_zip_shp(object = f, bucket = "curbcut.amenities")
  })
  poi <- Reduce(rbind, poi)

  # Transform
  poi <- sf::st_transform(poi, crs = 3347)

  # Filter out NAs and empty geometries
  poi <- poi[!is.na(poi$SIC_1), ]
  poi <- poi[!sf::st_is_empty(poi), ]


  # Retail ------------------------------------------------------------------

  # Create the retail dictionary
  retail_general <- tibble::tibble(
    var = "retail_general",
    type = list("53"),
    title = "General Merchandise Stores",
    short = "General",
    exp = paste0("retail stores which sell a number of lines of merchandise, ",
                 "such as dry goods, apparel and accessories, furniture and ",
                 "home furnishings, small wares, hardware, and food"))

  retail_apparel <- tibble::tibble(
    var = "retail_apparel",
    type = list("56"),
    title = "Apparel And Accessory Stores",
    short = "Apparel",
    exp = paste0("retail stores primarily engaged in selling new clothing, ",
                 "shoes, hats, underwear, and related articles for personal ",
                 "wear and adornment")
  )

  retail_furniture <- tibble::tibble(
    var = "retail_furniture",
    type = list("57"),
    title = "Home Furniture, Furnishings, And Equipment Stores",
    short = "Furniture",
    exp = paste0("retail stores selling goods used for furnishing the home, ",
                 "such as furniture, floor coverings, draperies, glass and ",
                 "chinaware, domestic stoves, refrigerators, and other ",
                 "household electrical and gas appliances")
  )

  retail_eating <- tibble::tibble(
    var = "retail_eating",
    type = list("58"),
    title = "Eating And Drinking Places",
    short = "Eating",
    exp = paste0("retail establishments selling prepared foods and drinks ",
                 "for consumption on the premises; and also lunch counters ",
                 "and refreshment stands selling prepared foods and drinks ",
                 "for immediate consumption")
  )

  retail_misc <- tibble::tibble(
    var = "retail_misc",
    type = list("59"),
    title = "Miscellaneous Retail",
    short = "Retail",
    exp = paste0("retail establishments such as drug stores, liquor stores, ",
                 "used merchandise stores, miscellaneous shopping goods stores, ",
                 "non-store retailers, fuel dealers, and miscellaneous retail ",
                 "stores not elsewhere classified")
  )

  retail_dict <- rbind(retail_general, retail_apparel,
                       retail_furniture, retail_eating, retail_misc)

  retail_total <- tibble::tibble(
    var = "retail_total",
    type = list(unlist(retail_dict$type)),
    title = "Retail Trade",
    short = "Retail",
    exp = paste0("establishments engaged in selling merchandise for personal ",
                 "or household consumption and rendering services incidental ",
                 "to the sale of the goods"))

  retail_dict <- rbind(retail_dict, retail_total)
  retail_dict$source <- "DMTI"
  retail_dict$date <- "2022"
  retail_dict$theme <- "retail"

  # Subset the POI for just the retail
  poi_retail <- poi[poi$SIC_MJ_GRP %in% unlist(retail_dict$type), ]

  # How many points per variables are there in each DA
  retail <- sapply(retail_dict$var, \(var) {

    mjr_groups <- retail_dict$type[retail_dict$var == var][[1]]

    points <- poi_retail[poi_retail$SIC_MJ_GRP %in% mjr_groups, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "NAME", "SIC_1")]
    names(points) <- c("DA_ID", "name", "type", "geometry")


    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  retail_data <- Reduce(merge, lapply(retail, `[[`, "DA"))
  # retail_points <- Reduce(merge, lapply(retail, `[[`, "points"))


  # Financial institutions --------------------------------------------------

  # Create the finance dictionary
  finance_depository <- tibble::tibble(
    var = "finance_depository",
    type = list("60"),
    title = "Depository Institutions",
    short = "Depository",
    exp = paste0("institutions that are engaged in deposit banking or closely ",
                 "related functions, including fiduciary activities"))

  finance_nondepository <- tibble::tibble(
    var = "finance_nondepository",
    type = list("61"),
    title = "Non-depository Credit Institutions",
    short = "Credit",
    exp = paste0("establishments engaged in extending credit in the form of ",
                 "loans, but not engaged in deposit banking"))

  finance_dict <- rbind(finance_depository, finance_nondepository)

  finance_total <- tibble::tibble(
    var = "finance_total",
    type = list(unlist(finance_dict$type)),
    title = "Financial Institutions",
    short = "Finance",
    exp = paste0("establishments operating primarily in the fields of finance ",
                 "including includes depository institutions and ",
                 "non-depository credit institutions"))

  finance_dict <- rbind(finance_dict, finance_total)
  finance_dict$source <- "DMTI"
  finance_dict$date <- "2022"
  finance_dict$theme <- "finance"

  # Subset the POI for just the finance
  poi_finance <- poi[poi$SIC_MJ_GRP %in% unlist(finance_dict$type), ]

  # How many points per variables are there in each DA
  finance <- sapply(finance_dict$var, \(var) {

    mjr_groups <- finance_dict$type[finance_dict$var == var][[1]]

    points <- poi_finance[poi_finance$SIC_MJ_GRP %in% mjr_groups, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "NAME", "SIC_1")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  finance_data <- Reduce(merge, lapply(finance, `[[`, "DA"))
  # finance_points <- Reduce(merge, lapply(finance, `[[`, "points"))


  # Food stores -------------------------------------------------------------

  # Create the food dictionary
  food_grocery <- tibble::tibble(
    var = "food_grocery",
    type = list("5411"),
    title = "Grocery and Convenience Stores",
    short = "Groceries",
    exp = paste0("stores, commonly known as supermarkets, food stores, and ",
                 "grocery stores")
  )

  food_meat <- tibble::tibble(
    var = "food_meat",
    type = list("5421"),
    title = "Meat and Fish Markets",
    short = "Meat",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "fresh, frozen, or cured meats, fish, shellfish, and other ",
                 "seafoods")
  )

  food_fruit <- tibble::tibble(
    var = "food_fruit",
    type = list("5431"),
    title = "Fruit and Vegetable Markets",
    short = "Fruit/Veg.",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "fresh fruits and vegetables")
  )

  food_dairy <- tibble::tibble(
    var = "food_dairy",
    type = list("5451"),
    title = "Dairy Products Stores",
    short = "Dairy",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "packaged dairy products to over-the-counter customers")
  )

  food_bakeries <- tibble::tibble(
    var = "food_bakeries",
    type = list("5461"),
    title = "Retail Bakeries",
    short = "Bakeries",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "bakery products")
  )

  food_misc <- tibble::tibble(
    var = "food_misc",
    type = list("5499"),
    title = "Miscellaneous Food Stores",
    short = "Food",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "specialized foods, not elsewhere classified, such as eggs",
                 ", poultry, health foods, spices, herbs, coffee, and tea")
  )

  food_dict <- rbind(food_grocery, food_meat, food_fruit, food_dairy,
                     food_bakeries, food_misc)

  food_total <- tibble::tibble(
    var = "food_total",
    type = list(unlist(food_dict$type)),
    title = "Food Stores",
    short = "Food",
    exp = paste0("retail stores primarily engaged in selling food for home ",
                 "preparation and consumption"))

  food_dict <- rbind(food_dict, food_total)
  food_dict$source <- "DMTI"
  food_dict$date <- "2022"
  food_dict$theme <- "food"

  # Subset the POI for just the food
  poi$sic_short <- gsub("0000$", "", poi$SIC_1)
  poi_food <- poi[poi$sic_short %in% unlist(food_dict$type), ]

  # How many points per variables are there in each DA
  food <- sapply(food_dict$var, \(var) {

    industries <- food_dict$type[food_dict$var == var][[1]]

    points <- poi_food[poi_food$sic_short %in% industries, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "NAME", "SIC_1")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  food_data <- Reduce(merge, lapply(food, `[[`, "DA"))
  # food_points <- Reduce(merge, lapply(food, `[[`, "points"))



  # Amusement and Recreation Services --------------------------------------

  # Create the recreation dictionary
  recreation_dance <- tibble::tibble(
    var = "recreation_dance",
    type = list("791"),
    title = "Dance Studios, Schools, And Halls",
    short = "Dance Studios",
    exp = paste0("establishments primarily engaged in operating dance studios, ",
                 "schools, and public dance halls or ballrooms"))

  recreation_theatrical <- tibble::tibble(
    var = "recreation_theatrical",
    type = list(c("792", "783")),
    title = "Theatrical Producers",
    short = "Meat",
    exp = paste0("Establishments primarily engaged in providing live theatrical ",
                 "presentations and motion pictures")
  )

  recreation_physical <- tibble::tibble(
    var = "recreation_physical",
    type = list("793"),
    title = "Bowling Centers",
    short = "Bowling",
    exp = paste0("establishments primarily engaged in operating reducing and ",
                 "other health clubs, spas, and similar facilities featuring ",
                 "exercise and other active physical fitness conditioning")
  )

  recreation_bowling <- tibble::tibble(
    var = "recreation_bowling",
    type = list("7991"),
    title = "Physical Fitness Facilities",
    short = "Fitness",
    exp = paste0("establishments known to the public as bowling centers or lanes")
  )

  recreation_misc <- tibble::tibble(
    var = "recreation_misc",
    type = list(c("7941", "7948", "7992", "7993", "7996", "7997", "7999")),
    title = "Miscellaneous Amusement And Recreation",
    short = "Recreation",
    exp = paste0("establishments primarily engaged in the operation of sports, ",
                 "amusement, and recreation services, not elsewhere classified, ",
                 "such as bathing beaches, swimming pools, public golf courses, ",
                 "musement parks, ...")
  )


  recreation_dict <- rbind(recreation_dance, recreation_theatrical, recreation_physical, recreation_bowling,
                           recreation_misc)

  recreation_total <- tibble::tibble(
    var = "recreation_total",
    type = list(unlist(recreation_dict$type)),
    title = "Recreation Services",
    short = "Recreation",
    exp = paste0("establishments engaged in providing amusement or entertainment services"))

  recreation_dict <- rbind(recreation_dict, recreation_total)
  recreation_dict$source <- "DMTI"
  recreation_dict$date <- "2022"
  recreation_dict$theme <- "recreation"

  # Subset the POI for just the recreation
  poi$sic_short <- gsub("0000$", "", poi$SIC_1)
  poi_recreation <- poi[poi$sic_short %in% unlist(recreation_dict$type), ]

  # How many points per variables are there in each DA
  recreation <- sapply(recreation_dict$var, \(var) {

    industries <- recreation_dict$type[recreation_dict$var == var][[1]]

    points <- poi_recreation[poi_recreation$sic_short %in% industries, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "NAME", "SIC_1")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  recreation_data <- Reduce(merge, lapply(recreation, `[[`, "DA"))
  # recreation_points <- Reduce(merge, lapply(recreation, `[[`, "points"))


  # Healthcare services -----------------------------------------------------

  read_method <- function(file) {
    content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
    csv_file <- content[grepl("\\.csv", content)]
    utils::unzip(file, files = csv_file, exdir = tempdir())
    suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", csv_file)))
  }

  # Grab from bucket
  healthcare <-
    bucket_read_object(object = "open_db_heathcare_facilities.zip",
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
    tibble::as_tibble()

  # Transform to SF
  healthcare$longitude <- suppressWarnings(as.numeric(healthcare$longitude))
  healthcare <- healthcare[!is.na(healthcare$longitude), ]
  healthcare$latitude <- suppressWarnings(as.numeric(healthcare$latitude))
  healthcare <- healthcare[!is.na(healthcare$latitude), ]
  healthcare <- sf::st_as_sf(healthcare, coords = c("longitude", "latitude"),
                              crs = 4326) |>
    sf::st_transform(3347)

  healthcare$odhf_facility_type[healthcare$odhf_facility_type ==
                                  "nursing and residential care facilities"] <-
    "Nursing and residential care facilities"

  # Create the healthcare dictionary
  healthcare_ambulatory <- tibble::tibble(
    var = "healthcare_ambulatory",
    type = list("Ambulatory health care services"),
    title = "Ambulatory health care services",
    short = "Ambulatory",
    exp = paste0("establishments primarily engaged in providing health care ",
                 "services, directly or indirectly, to ambulatory patients (",
                 "Example: medical clinic, mental health center)"))

  healthcare_hospitals <- tibble::tibble(
    var = "healthcare_hospitals",
    type = list("Hospitals"),
    title = "Hospitals",
    short = "Hospitals",
    exp = paste0("establishments, licensed as hospitals, primarily engaged ",
                 "in providing diagnostic and medical treatment services, and ",
                 "specialized accommodation services to in-patients (Example: ",
                 "emergency department, general hospital)")
  )

  healthcare_nursing <- tibble::tibble(
    var = "healthcare_nursing",
    type = list("Nursing and residential care facilities"),
    title = "Nursing and residential care facilities",
    short = "Nursing",
    exp = paste0("establishments primarily engaged in providing residential ",
                 "care combined with either nursing, supervisory or other ",
                 "types of care as required by the residents (Example: ",
                 "nursing home)")
  )

  healthcare_dict <- rbind(healthcare_ambulatory, healthcare_hospitals,
                           healthcare_nursing)

  healthcare_total <- tibble::tibble(
    var = "healthcare_total",
    type = list(unlist(healthcare_dict$type)),
    title = "Healthcare facilities",
    short = "Healthcare",
    exp = paste0("physical site at which the primary activity is the ",
                 "provision of healthcare"))

  healthcare_dict <- rbind(healthcare_dict, healthcare_total)
  healthcare_dict$source <- "Canadian Open Database of Healthcare Facilities (ODHF)"
  healthcare_dict$date <- "2020"
  healthcare_dict$theme <- "healthcare"


  # How many points per variables are there in each DA
  healthcare <- sapply(healthcare_dict$var, \(var) {

    types <- healthcare_dict$type[healthcare_dict$var == var][[1]]

    points <- healthcare[healthcare$odhf_facility_type %in% types, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "facility_name", "odhf_facility_type")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  healthcare_data <- Reduce(merge, lapply(healthcare, `[[`, "DA"))
  # healthcare_points <- Reduce(merge, lapply(healthcare, `[[`, "points"))


  # Education facilities ----------------------------------------------------

  read_method <- function(file) {
    content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
    csv_file <- content[grepl("\\.csv", content)]
    utils::unzip(file, files = csv_file, exdir = tempdir())
    suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", csv_file),
                                     fileEncoding = "latin1"))
  }

  # Grab from bucket
  educational <-
    bucket_read_object(object = "open_db_educational_facilities.zip",
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
    tibble::as_tibble()

  # Transform to SF
  educational$Longitude <- suppressWarnings(as.numeric(educational$Longitude))
  educational <- educational[!is.na(educational$Longitude), ]
  educational$Latitude <- suppressWarnings(as.numeric(educational$Latitude))
  educational <- educational[!is.na(educational$Latitude), ]
  educational <- sf::st_as_sf(educational, coords = c("Longitude", "Latitude"),
                             crs = 4326) |>
    sf::st_transform(3347)

  # Create the educational dictionary
  educational_kindergarten <- tibble::tibble(
    var = "educational_kindergarten",
    type = list(c("ISCED010", "ISCED020")),
    title = "Early childhood and/or kindergarten education",
    short = "Kindergarten",
    exp = paste0("establishments supporting education programmes designed to ",
                 "support early development in preparation for participation ",
                 "in school and society for children below the age to start ",
                 "primary education"))

  educational_elementary <- tibble::tibble(
    var = "educational_elementary",
    type = list("ISCED1"),
    title = "Elementary education",
    short = "Elementary",
    exp = paste0("establishments supporting education programmes typically ",
                 "designed to provide students with fundamental skills in ",
                 "reading, writing and mathematics and to establish a solid ",
                 "foundation for learning")
  )

  educational_secondary <- tibble::tibble(
    var = "educational_secondary",
    type = list(c("ISCED2", "ISCED3")),
    title = "Secondary education",
    short = "Secondary",
    exp = paste0("establishments supporting education programmes building on ",
                 "primary education, typically with a more subject-oriented ",
                 "curriculum, and preparing for tertiary education and/or ",
                 "providing skills relevant to employment")
  )

  educational_post <- tibble::tibble(
    var = "educational_post",
    type = list("ISCED4Plus"),
    title = "Post-secondary education",
    short = "Post-secondary",
    exp = paste0("establishments supporting education programmes that build on ",
                 "secondary education and prepare for labour market entry and/or ",
                 "tertiary education, and/or provide intermediate to advanced ",
                 "academic and/or professional knowledge, skills and ",
                 "competencies leading to degrees or qualifications")
  )

  educational_dict <- rbind(educational_kindergarten, educational_elementary,
                            educational_secondary, educational_post)

  educational_total <- tibble::tibble(
    var = "educational_total",
    type = list(unlist(educational_dict$type)),
    title = "Educational facilities",
    short = "Educational",
    exp = paste0("educational facilities"))

  educational_dict <- rbind(educational_dict, educational_total)
  educational_dict$source <- "Canadian Open Database of Educational Facilities (ODEF)"
  educational_dict$date <- "2022"
  educational_dict$theme <- "educational"


  # How many points per variables are there in each DA
  educational <- sapply(educational_dict$var, \(var) {

    types <- educational_dict$type[educational_dict$var == var][[1]]

    # Rows that fit with one or multiple types
    points <- rowSums(sapply(types, \(type) educational[[type]] == 1)) > 0
    points <- educational[points, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "Facility_Name")]
    names(points) <- c("DA_ID", "name", "geometry")
    points$type <- var

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  educational_data <- Reduce(merge, lapply(educational, `[[`, "DA"))
  # educational_points <- Reduce(merge, lapply(educational, `[[`, "points"))


  # Cultural facilities -----------------------------------------------------

  read_method <- function(file) {
    content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
    csv_file <- content[grepl("\\.csv", content)]
    csv_file <- csv_file[!grepl("Data_Sources", csv_file)]
    utils::unzip(file, files = csv_file, exdir = tempdir())
    suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", csv_file)))
  }

  cultural <-
    bucket_read_object(object = "open_db_cultural_facilities.zip",
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
    tibble::as_tibble()


  # Transform to SF
  cultural$Longitude <- suppressWarnings(as.numeric(cultural$Longitude))
  cultural <- cultural[!is.na(cultural$Longitude), ]
  cultural$Latitude <- suppressWarnings(as.numeric(cultural$Latitude))
  cultural <- cultural[!is.na(cultural$Latitude), ]
  cultural <- sf::st_as_sf(cultural, coords = c("Longitude", "Latitude"),
                              crs = 4326) |>
    sf::st_transform(3347)


  # Create the cultural dictionary
  cultural_artcentre <- tibble::tibble(
    var = "cultural_artcentre",
    type = list(c("art or cultural centre")),
    title = "Art or cultural centre",
    short = "Art/cultural",
    exp = paste0("establishments primarily engaged in promoting culture and arts"))

  cultural_gallery <- tibble::tibble(
    var = "cultural_gallery",
    type = list("gallery"),
    title = "Gallery",
    short = "Gallery",
    exp = paste0("establishments primarily engaged in the display of artistic works")
  )

  cultural_heritage <- tibble::tibble(
    var = "cultural_heritage",
    type = list(c("heritage or historic site")),
    title = "Heritage or historic site",
    short = "Heritage",
    exp = paste0("sites of cultural, artistic, or historic significance")
  )

  cultural_library <- tibble::tibble(
    var = "cultural_library",
    type = list("library or archives"),
    title = "Library or archive",
    short = "Library",
    exp = paste0("establishments primarily engaged in the display, curation, ",
                 "and sharing of primarily written material such as ",
                 "manuscripts, periodicals, and other items such as maps or images")
  )

  cultural_museum <- tibble::tibble(
    var = "cultural_museum",
    type = list("museum"),
    title = "Museum",
    short = "Museum",
    exp = paste0("establishments primarily engaged in the display, curation, ",
                 "and sharing of collections of artifacts, fine arts, and ",
                 "other objects of artistic, cultural, or historical importance")
  )

  cultural_theatre <- tibble::tibble(
    var = "cultural_theatre",
    type = list("theatre/performance and concert hall"),
    title = "Theatre/performance and concert hall",
    short = "Theatre",
    exp = paste0("establishments primarily engaged in the public performance ",
                 "of artistic or cultural works")
  )

  cultural_misc <- tibble::tibble(
    var = "cultural_misc",
    type = list(c("miscellaneous", "festival site", "artist")),
    title = "Miscellaneous cultural facilities",
    short = "Miscellaneous",
    exp = paste0("establishments associated in some way with promoting or ",
                 "providing culture or arts that do not fall into any of the ",
                 "above categories")
  )

  cultural_dict <- rbind(cultural_artcentre, cultural_gallery,
                         cultural_heritage, cultural_library, cultural_museum,
                         cultural_theatre, cultural_misc)

  cultural_total <- tibble::tibble(
    var = "cultural_total",
    type = list(unlist(cultural_dict$type)),
    title = "Cultural facilities",
    short = "Cultural",
    exp = paste0("establishments associated with promoting or providing culture or arts"))

  cultural_dict <- rbind(cultural_dict, cultural_total)
  cultural_dict$source <- "Canadian Open Database of Cultural and Art Facilities (ODCAF)"
  cultural_dict$date <- "2020"
  cultural_dict$theme <- "cultural"


  # How many points per variables are there in each DA
  cultural <- sapply(cultural_dict$var, \(var) {

    types <- cultural_dict$type[cultural_dict$var == var][[1]]

    points <- cultural[cultural$ODCAF_Facility_Type %in% types, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "Facility_Name", "ODCAF_Facility_Type")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  cultural_data <- Reduce(merge, lapply(cultural, `[[`, "DA"))
  # cultural_points <- Reduce(merge, lapply(cultural, `[[`, "points"))



  # Green spaces ------------------------------------------------------------
#
#   # Grab 2021 parks from OSM
#   osm_pbf <- "http://download.geofabrik.de/north-america/canada-latest.osm.pbf"
#   mp_osm <- osmextract::oe_read(osm_pbf, layer = "multipolygons",
#                                 quiet = FALSE, force_download = FALSE,
#                                 max_file_size = Inf)
#
#   # Include all green spaces from parks to wood
#   parks <- mp_osm[!is.na(mp_osm$leisure) & mp_osm$leisure == "park", ]
#   woods <- mp_osm[!is.na(mp_osm$natural) & mp_osm$natural == "wood", ]
#   rm(mp_osm)
#
#   # Add the mont royal to the parks
#   mont_royal <- woods[grepl("mont-royal", tolower(woods$name)), ]
#   parks <- rbind(parks["osm_id"], mont_royal["osm_id"])
#
#   # Treat polygons for no overlapping features
#   woods <- accessibility_polygons_helper(woods)
#   parks <- accessibility_polygons_helper(parks)
#
#   # total <- rbind(woods, parks)
#   # total <- unique(total["geometry"])
#   # total$ID <- seq_along(total$geometry)
#   # total <- accessibility_polygons_helper(total)
#
#   # Intersect and link every DA to the park/wood/total ID
#   intersected <- sapply(sf::st_intersects(DA_table, woods), c)
#   greenspace_wood <- tibble::tibble(DA_ID = DA_table$ID,
#                  greenspace_parks = intersected)
#
#   intersected <- sapply(sf::st_intersects(DA_table, parks), c)
#   greenspace_parks <- tibble::tibble(DA_ID = DA_table$ID,
#                           greenspace_wood = intersected)
#
#   # intersected <- sapply(sf::st_intersects(DA_table, total), c)
#   # greenspace_total <- tibble::tibble(DA_ID = DA_table$ID,
#   #                         greenspace_total = intersected)
#   greenspace <-
#     Reduce(merge,
#            list(greenspace_wood, greenspace_parks)) |> #, greenspace_total)) |>
#     tibble::as_tibble()
#
#   # Get all areas in a list, which will need to go in the SQL db
#   woods$area <- get_area(woods$geometry)
#   parks$area <- get_area(parks$geometry)
#   # total$area <- get_area(total$geometry)
#   woods <- sf::st_drop_geometry(woods)
#   parks <- sf::st_drop_geometry(parks)
#   # total <- sf::st_drop_geometry(total)
#   greenspace_polygons <- list(greenspace_wood = woods,
#                               greenspace_parks = parks)#,
#                               # greenspace_total = total)
#
#   # Make the green space dictionary
#   greenspace_parks <- tibble::tibble(
#     var = "greenspace_parks",
#     type = list("Park"),
#     title = "Parks",
#     short = "Parks",
#     exp = paste0("areas of open space for recreational use, usually designed and in semi-natural state with grassy areas"))
#
#   greenspace_wood <- tibble::tibble(
#     var = "greenspace_wood",
#     type = list("Wood"),
#     title = "Woods",
#     short = "Woods",
#     exp = paste0("tree-covered areas"))
#
#   # greenspace_total <- tibble::tibble(
#   #   var = "greenspace_total",
#   #   type = list(c("Parks", "Wood")),
#   #   title = "Green space",
#   #   short = "Green space",
#   #   exp = paste0("areas of open space for recreational use and tree-covered areas"))
#
#   greenspace_dict <- rbind(greenspace_parks, greenspace_wood)#, greenspace_total)


  # Return all the measures and the dictionary ------------------------------

  access <- Reduce(merge, list(retail_data, finance_data, food_data, recreation_data,
                               healthcare_data, educational_data, cultural_data))#, ...))
  names(access)[1] <- "DA_ID"
  # polygon_dict <- greenspace
  # polygons <- c(greenspace_polygons)
  # points <- Reduce(merge, list(retail_points, finance_points, food_points,
  #                              healthcare_points))#, ...))
  dict <- Reduce(rbind, list(retail_dict, finance_dict, food_dict, recreation_dict,
                             healthcare_dict, educational_dict, cultural_dict))#greenspace_dict, ...))
  themes <- unique(dict$theme)

  return(list(data = access,
              #polygon_dict = polygon_dict,
              #polygons = polygons,
              dict = dict,
              #points = points,
              themes = themes))

}

#' List pre-processed accessibility themes
#'
#' @return Returns a character vector of pre-processed accessibility themes
#' @export
list_accessibility_themes <- function() {
  cc.data::accessibility_themes
}

#' Accessibility Polygons Helper Function
#'
#' This function processes input polygons and groups them if they are touching
#' or intersecting. The grouped polygons are then returned as a single
#' POLYGON object.
#'
#' @param polygons <`sf`> An object of class \code{sf} containing the input
#' polygons to be processed.
#'
#' @return An object of class \code{sf} with the processed polygons, where
#' touching or intersecting polygons are merged into a single POLYGON object.
accessibility_polygons_helper <- function(polygons) {
  # Separate everything by polygons
  polygons <- sf::st_transform(polygons, 3347)
  polygons <- sf::st_cast(polygons, "MULTIPOLYGON")
  polygons <- sf::st_cast(polygons, "POLYGON")
  polygons <- sf::st_make_valid(polygons)

  # Calculate intersection matrix to merge all touching polygons
  intersects <- sf::st_intersects(polygons, remove_self = FALSE)
  polygons_groupings <- matchr:::reduce_int(intersects)

  polygons_groups <- future.apply::future_lapply(polygons_groupings, \(x) {
    sf::st_union(polygons[x, ])
  })

  # Go back to polygons
  out_polygons <- tibble::tibble(ID = seq_along(polygons_groups))
  out_polygons$geometry <- sapply(polygons_groups, c)
  out_polygons <- sf::st_as_sf(out_polygons, crs = 3347)
  out_polygons <- sf::st_cast(out_polygons, "MULTIPOLYGON")
  polygons <- sf::st_cast(out_polygons, "POLYGON")

  return(polygons)

}
