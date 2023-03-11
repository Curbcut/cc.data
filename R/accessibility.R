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

  if (all(names(DA_table) == c("ID", "geometry"))) {
    stop("`DA_table` must have only the columns `ID` and `geometry`.")
  }

  # Read DMTI data from bucket ----------------------------------------------

  `TKTK PUT IN BUCKET AND EXTRACT FROM THERE`

  poi_1 <- sf::read_sf("calculated_ignore/poi/poi_2021_1.shp")
  poi_2 <- sf::read_sf("calculated_ignore/poi/poi_2021_2.shp")

  poi <- rbind(poi_1, poi_2)
  poi <- sf::st_transform(poi, crs = sf::st_crs(DA_table)$input)

  # Filter out NAs and empty geometries
  poi <- poi[!is.na(poi$SIC_1), ]
  poi <- poi[!sf::st_is_empty(poi), ]


  # Retail ------------------------------------------------------------------

  # Create the retail dictionary
  retail_general <- tibble::tibble(
    var = "retail_general",
    major_group = list("53"),
    title = "General Merchandise Stores",
    short = "General",
    exp = paste0("retail stores which sell a number of lines of merchandise, ",
                 "such as dry goods, apparel and accessories, furniture and ",
                 "home furnishings, small wares, hardware, and food"))

  retail_apparel <- tibble::tibble(
    var = "retail_apparel",
    major_group = list("56"),
    title = "Apparel And Accessory Stores",
    short = "Apparel",
    exp = paste0("retail stores primarily engaged in selling new clothing, ",
                 "shoes, hats, underwear, and related articles for personal ",
                 "wear and adornment")
  )

  retail_furniture <- tibble::tibble(
    var = "retail_furniture",
    major_group = list("57"),
    title = "Home Furniture, Furnishings, And Equipment Stores",
    short = "Furniture",
    exp = paste0("retail stores selling goods used for furnishing the home, ",
                 "such as furniture, floor coverings, draperies, glass and ",
                 "chinaware, domestic stoves, refrigerators, and other ",
                 "household electrical and gas appliances")
  )

  retail_eating <- tibble::tibble(
    var = "retail_eating",
    major_group = list("58"),
    title = "Eating And Drinking Places",
    short = "Eating",
    exp = paste0("retail establishments selling prepared foods and drinks ",
                 "for consumption on the premises; and also lunch counters ",
                 "and refreshment stands selling prepared foods and drinks ",
                 "for immediate consumption")
  )

  retail_misc <- tibble::tibble(
    var = "retail_misc",
    major_group = list("59"),
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
    major_group = list(unlist(retail_dict$major_group)),
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
  poi_retail <- poi[poi$SIC_MJ_GRP %in% unlist(retail_dict$major_group), ]

  # How many points per variables are there in each DA
  retail <- sapply(retail_dict$var, \(var) {

    mjr_groups <- retail_dict$major_group[retail_dict$var == var][[1]]

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
    major_group = list("60"),
    title = "Depository Institutions",
    short = "Depository",
    exp = paste0("institutions that are engaged in deposit banking or closely ",
                 "related functions, including fiduciary activities"))

  finance_nondepository <- tibble::tibble(
    var = "finance_nondepository",
    major_group = list("61"),
    title = "Non-depository Credit Institutions",
    short = "Credit",
    exp = paste0("establishments engaged in extending credit in the form of ",
                 "loans, but not engaged in deposit banking"))

  finance_dict <- rbind(finance_depository, finance_nondepository)

  finance_total <- tibble::tibble(
    var = "finance_total",
    major_group = list(unlist(finance_dict$major_group)),
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
  poi_finance <- poi[poi$SIC_MJ_GRP %in% unlist(finance_dict$major_group), ]

  # How many points per variables are there in each DA
  finance <- sapply(finance_dict$var, \(var) {

    mjr_groups <- finance_dict$major_group[finance_dict$var == var][[1]]

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
    industry = list("5411"),
    title = "Grocery Stores",
    short = "Groceries",
    exp = paste0("stores, commonly known as supermarkets, food stores, and ",
                 "grocery stores"))

  food_meat <- tibble::tibble(
    var = "food_meat",
    industry = list("5421"),
    title = "Meat and Fish Markets",
    short = "Meat",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "fresh, frozen, or cured meats, fish, shellfish, and other ",
                 "seafoods")
  )

  food_fruit <- tibble::tibble(
    var = "food_furniture",
    industry = list("5431"),
    title = "Fruit and Vegetable Markets",
    short = "Fruit/Veg.",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "fresh fruits and vegetables")
  )

  food_dairy <- tibble::tibble(
    var = "food_dairy",
    industry = list("5451"),
    title = "Dairy Products Stores",
    short = "Dairy",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "packaged dairy products to over-the-counter customers")
  )

  food_bakeries <- tibble::tibble(
    var = "food_bakeries",
    industry = list("5461"),
    title = "Retail Bakeries",
    short = "Bakeries",
    exp = paste0("establishments primarily engaged in the retail sale of ",
                 "bakery products")
  )

  food_misc <- tibble::tibble(
    var = "food_misc",
    industry = list("5499"),
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
    industry = list(unlist(food_dict$industry)),
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
  poi_food <- poi[poi$sic_short %in% unlist(food_dict$industry), ]

  # How many points per variables are there in each DA
  food <- sapply(food_dict$var, \(var) {

    industries <- food_dict$industry[food_dict$var == var][[1]]

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

  # Create the food dictionary
  healthcare_grocery <- tibble::tibble(
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

  healthcare_dict <- rbind(healthcare_grocery, healthcare_hospitals,
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
    suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", csv_file)))
  }

  # Grab from bucket
  educational <-
    bucket_read_object(object = "open_db_educational_facilities.zip",
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
    tibble::as_tibble()

  # Transform to SF
  educational$longitude <- suppressWarnings(as.numeric(educational$longitude))
  educational <- educational[!is.na(educational$longitude), ]
  educational$latitude <- suppressWarnings(as.numeric(educational$latitude))
  educational <- educational[!is.na(educational$latitude), ]
  educational <- sf::st_as_sf(educational, coords = c("longitude", "latitude"),
                             crs = 4326) |>
    sf::st_transform(3347)

  educational$odhf_facility_type[educational$odhf_facility_type ==
                                  "nursing and residential care facilities"] <-
    "Nursing and residential care facilities"

  # Create the food dictionary
  educational_grocery <- tibble::tibble(
    var = "educational_ambulatory",
    type = list("Ambulatory health care services"),
    title = "Ambulatory health care services",
    short = "Ambulatory",
    exp = paste0("establishments primarily engaged in providing health care ",
                 "services, directly or indirectly, to ambulatory patients (",
                 "Example: medical clinic, mental health center)"))

  educational_hospitals <- tibble::tibble(
    var = "educational_hospitals",
    type = list("Hospitals"),
    title = "Hospitals",
    short = "Hospitals",
    exp = paste0("establishments, licensed as hospitals, primarily engaged ",
                 "in providing diagnostic and medical treatment services, and ",
                 "specialized accommodation services to in-patients (Example: ",
                 "emergency department, general hospital)")
  )

  educational_nursing <- tibble::tibble(
    var = "educational_nursing",
    type = list("Nursing and residential care facilities"),
    title = "Nursing and residential care facilities",
    short = "Nursing",
    exp = paste0("establishments primarily engaged in providing residential ",
                 "care combined with either nursing, supervisory or other ",
                 "types of care as required by the residents (Example: ",
                 "nursing home)")
  )

  educational_dict <- rbind(educational_grocery, educational_hospitals,
                           educational_nursing)

  educational_total <- tibble::tibble(
    var = "educational_total",
    type = list(unlist(educational_dict$type)),
    title = "educational facilities",
    short = "educational",
    exp = paste0("physical site at which the primary activity is the ",
                 "provision of educational"))

  educational_dict <- rbind(educational_dict, educational_total)
  educational_dict$source <- "Canadian Open Database of educational Facilities (ODHF)"
  educational_dict$date <- "2020"
  educational_dict$theme <- "educational"


  # How many points per variables are there in each DA
  educational <- sapply(educational_dict$var, \(var) {

    types <- educational_dict$type[educational_dict$var == var][[1]]

    points <- educational[educational$odhf_facility_type %in% types, ]

    points_per_DA <- lengths(sf::st_intersects(DA_table, points))

    out <- tibble::tibble(ID = DA_table$ID)
    out[[var]] <- points_per_DA

    points <- sf::st_join(points, DA_table)[c("ID", "facility_name", "odhf_facility_type")]
    names(points) <- c("DA_ID", "name", "type", "geometry")

    return(list(DA = out,
                points = points))
  }, simplify = FALSE, USE.NAMES = TRUE)

  educational_data <- Reduce(merge, lapply(educational, `[[`, "DA"))
  # educational_points <- Reduce(merge, lapply(educational, `[[`, "points"))



























  # Return all the measures and the dictionary ------------------------------

  access <- Reduce(merge, list(retail_data, finance_data, food_data,
                               healthcare_data, ...))
  points <- Reduce(merge, list(retail_points, finance_points, food_points,
                               healthcare_points, ...))
  dict <- Reduce(merge, list(retail_dict, finance_dict, food_dict,
                             healthcare_dict, ...))
  themes <- unique(dict$theme)

  return(list(data = access, dict = dict, points = points, themes))


}

#' List pre-processed accessibility themes
#'
#' @return Returns a character vector of pre-processed accessibility themes
#' @export
list_accessibility_themes <- function() {
  cc.data::accessibility_themes
}
