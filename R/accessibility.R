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

  ## TKTK INSTEAD OF GOING WITH DMTI FOR MOSTLY EVERYTHING, GO LOOK AT
  ## https://www150.statcan.gc.ca/n1/pub/17-26-0002/172600022020001-eng.htm
  ## AND SEE WHAT IS THEIR SOURCES, EX. The Open Database of Educational Facilities

  # Read DMTI data from bucket ----------------------------------------------

  read_method <- function(file) {
    content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
    csv_file <- content[grepl("\\.csv", content)]
    utils::unzip(file, files = csv_file, exdir = tempdir())
    suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", csv_file)))
  }

  bucket_content <- bucket_list_content("curbcut.amenities")$Key
  DMTI <- bucket_content[grepl("^DMTI/.*.zip$", bucket_content)]

  content <- sapply(DMTI, \(x) {
    bucket_read_object(object = x,
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
      tibble::as_tibble()
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Transform to SF
  content_sf <- lapply(content, \(x) {

    x$longitude <- as.numeric(x$longitude)
    x <- x[!is.na(x$longitude), ]
    x$latitude <- as.numeric(x$latitude)
    x <- x[!is.na(x$latitude), ]

    sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(3347)
  })

  # Take .zip out of names
  names(content_sf) <- gsub("^DMTI/|\\.zip$", "", names(content_sf))


  # Class using the `sic` code ----------------------------------------------

  # Special case healthcare
  hc <- content_sf$dmti_healthcare_2021
  hc$sic_1 <- substr(hc$sic_1, 1, 3)
  hc <- hc[hc$sic_1 %in% c("801", "805", "806", "804"), ]
  sic_df <- data.frame(
    sic_1 = c("801", "805", "806", "804"),
    industry = c("Doctors' offices and clinics",
                 "Nursing and personal care facilities",
                 "Hospitals",
                 "Other health practitioners' offices and clinics"),
    exp = c(paste0("Establishments of licensed practitioners having ",
                   "the degree of M.D. and engaged in the practice ",
                   "of general or specialized medicine and surgery"),
            paste0("Establishments primarily engaged in providing ",
                   "inpatient nursing and rehabilitative services"),
            paste0("Establishments primarily engaged in providing ",
                   "diagnostic services, treatments, general medical ",
                   "and surgical services and other hospital services"),
            paste0("Establishments of licensed practitioners engaging in ",
                   "the practice ",
                   "of chiropractic medicine, practice of optometry, ",
                   "practice of podiatry, or practice of other ",
                   "health fields not elsewhere classified")))
  merged <- merge(hc, sic_df)["industry"]
  hc <- list(df = merged,
             sic_def = sic_df)

  # Prepare some industries to filter out
  filter_out <- c("Drive-In Motion Picture Theaters",
                  "Candy, Nut, and Confectionery Stores")
  content_sf_ <- content_sf[names(content_sf) != "dmti_healthcare_2021"]
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(content_sf_))
    point_data <- future.apply::future_lapply(content_sf_, \(x) {
      requireNamespace("sf", quietly = TRUE)

      if (!"sic_1" %in% names(x)) return({
        x$industry <- NA
        list(df = x["industry"])
      })

      # Bring sic down to 4 digits
      x$sic_1 <- substr(x$sic_1, 1, 4)

      # Scrape the SIC meaning
      unique_sics <- unique(x$sic_1)
      unique_sics <- unique_sics[unique_sics != ""]

      sics <- sapply(unique_sics, \(sic) {
        page <-
          httr::GET(paste0("https://www.naics.com/sic-industry-description/?code=",
                           sic)) |>
          httr::content(as = "text")

        title <- stringr::str_extract(page, "(?<=<title>).*(?=</title>)") |>
          stringr::str_extract("(?<=\\d{4} ).*(?= &#8211)")
        exp <- stringr::str_extract(page, "(?<=</h6>).*?(?=\\.)")

        return(list(title = title, exp = exp))
      }, simplify = FALSE, USE.NAMES = TRUE)

      # Merge industry naming to each point data
      sic_df <- tibble::tibble(sic_1 = unique_sics,
                               industry = sapply(sics, `[[`, "title"),
                               exp = sapply(sics, `[[`, "exp"))
      merged <- merge(x, sic_df)["industry"]
      # Filter out too small industries (possibly misclassified)
      kept_industry <-
        table(merged$industry)[table(merged$industry) > nrow(merged)*0.01] |>
        names()
      kept_industry <- kept_industry[!kept_industry %in% filter_out]

      # Manually filtering out some industries
      kept_industry <-
        kept_industry[!kept_industry %in% "Operators of Nonresidential Buildings"]

      # Additionally filter out other industries
      out <- merged[merged$industry %in% kept_industry, ]

      # Filter back `sic_df` to only use the kept industries
      sic_df <- sic_df[sic_df$industry %in% kept_industry, ]

      pb()

      # Return
      return(list(df = out,
                  sic_def = sic_df))
    }, future.seed = NULL)
  })

  # Concatenate back with health practictioners
  point_data <- c(point_data, list(dmti_healthcare_2021 = hc))

  sic_def <- lapply(point_data, `[[`, "sic_def")
  point_data <- lapply(point_data, `[[`, "df")

  # Add source to sic_fed
  sic_def <- lapply(sic_def, \(x) {
    if (!is.null(x)) x$source <- "DMTI Spatial"
    if (!is.null(x)) x$date <- "2021"
    x
  })


  # Additional datasets -----------------------------------------------------

  content <- sapply("open_db_heathcare_facilities.zip", \(x) {
    bucket_read_object(object = x,
                       bucket = "curbcut.amenities",
                       objectext = ".zip",
                       method = read_method) |>
      tibble::as_tibble()
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Transform to SF
  content_sf <- lapply(content, \(x) {

    x$longitude <- as.numeric(x$longitude)
    x <- x[!is.na(x$longitude), ]
    x$latitude <- as.numeric(x$latitude)
    x <- x[!is.na(x$latitude), ]

    sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326) |>
      sf::st_transform(3347)
  })

  # Take .zip out of names
  names(content_sf) <- gsub("\\.zip$", "", names(content_sf))

  # Format like previous DMTI data, and combine
  hospitals <- content_sf$open_db_heathcare_facilities[
    content_sf$open_db_heathcare_facilities$odhf_facility_type == "Hospitals",
    "odhf_facility_type"
  ]
  names(hospitals)[1] <- "industry"
  point_data$dmti_healthcare_2021 <- point_data$dmti_healthcare_2021[
    !point_data$dmti_healthcare_2021$industry == "Hospitals",
  ]
  point_data$dmti_healthcare_2021 <-
    rbind(point_data$dmti_healthcare_2021, hospitals)
  # Update sic_def
  sic_def$dmti_healthcare_2021$source[
    sic_def$dmti_healthcare_2021$industry == "Hospitals"
  ] <- "Canadian Open Database of Healthcare Facilities"
  sic_def$dmti_healthcare_2021$date[
    sic_def$dmti_healthcare_2021$industry == "Hospitals"
  ] <- "2020"


  # Sum point number per DA -------------------------------------------------

  # Intersect
  names(DA_table)[1] <- "DA_ID"
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(point_data))
    point_data <- future.apply::future_lapply(point_data, \(x) {
      out <- sf::st_intersection(x, DA_table) |>
        sf::st_drop_geometry()
      pb()
      out
    }, future.seed = NULL)
  })

  # Count industry per DA
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(point_data))
    point_DA <-
      future.apply::future_lapply(point_data, \(x) {
        df <- sf::st_drop_geometry(x)
        df$industry[is.na(df$industry)] <- ""

        df <- as.data.frame(table(df$industry, df$DA_ID))
        colnames(df) <- c("industry", "DA_ID", "count")

        pb()
        df <- df[df$count > 0, ]
        tibble::as_tibble(df)
      }, future.seed = NULL)
  })


  # Separate industries -----------------------------------------------------

  progressr::with_progress({
    point_DA <-
      lapply(point_DA, \(x) {
        out <- sapply(as.character(unique(x$industry)), \(z) {
          x[x$industry == z, c("DA_ID", "count")]
        }, simplify = FALSE, USE.NAMES = TRUE)

        total <- x[c("DA_ID", "count")]
        total <- stats::aggregate(total$count, by = list(DA_ID = total$DA_ID), sum) |>
          tibble::as_tibble()
        names(total)[2] <- "count"

        c(out, list(Total = total))
      })
  })


  # Clean up ----------------------------------------------------------------

  # If there's only one industry and a total, just keep the industry
  point_DA <- lapply(point_DA, \(x) {
    if (length(x) == 2) x[1] else x
  })
  # Adjust wifi hotspot
  names(point_DA$dmti_wifihotspots_2021) <- "Public Wifi Hotspot"
  sic_def$dmti_wifihotspots_2021 <-
    data.frame(sic_1 = "",
               industry = "Public Wifi Hotspot",
               exp = "Locations that offer WiFi access",
               source = "DMTI Spatial",
               date = 2021)
  # Clean up other names
  names(point_DA$dmti_arenas_2021) <-
    "Amusement and Recreation Services"
  sic_def$dmti_arenas_2021$industry <- "Amusement and Recreation Services"
  names(point_DA$dmti_education_2021)[
    names(point_DA$dmti_education_2021) ==
      "Schools and Educational Services, Not Elsewhere Classified"] <-
    "Other Schools and Educational Services"
  sic_def$dmti_education_2021$industry[
    sic_def$dmti_education_2021$industry ==
      "Schools and Educational Services, Not Elsewhere Classified"] <-
    "Other Schools and Educational Services"
  names(point_DA$dmti_cinemas_2021) <-
    "Motion Picture Theaters"
  sic_def$dmti_cinemas_2021$industry <- "Motion Picture Theaters"
  names(point_DA$dmti_fooddistribution_2021)[
    names(point_DA$dmti_fooddistribution_2021) ==
      "Meat and Fish (Seafood) Markets, Including Freezer Provisioners"] <-
    "Meat and Fish (Seafood) Markets"
  sic_def$dmti_fooddistribution_2021$industry[
    sic_def$dmti_fooddistribution_2021$industry ==
      "Meat and Fish (Seafood) Markets, Including Freezer Provisioners"] <-
    "Meat and Fish (Seafood) Markets"


  # One variable, one dataframe ---------------------------------------------

  point_DA <- mapply(\(field_name, industries) {
    year <- stringr::str_extract(field_name, "\\d{4}$")
    field <- stringr::str_extract(field_name, "(?<=dmti_).*(?=_\\d{4}$)")

    names(industries) <-
      paste(field,
            tolower(stringr::str_extract(names(industries), ".*?(?=$|[^a-zA-Z])")),
            year, sep = "_")

    industries
  }, names(point_DA), point_DA)

  # Match the same process to the SIC code definitions
  sic_def <-
    mapply(\(field_name, industries) {
      year <- industries$date
      field <- stringr::str_extract(field_name, "(?<=dmti_).*(?=_\\d{4}$)")

      industries$var_code <-
        paste(field,
              tolower(stringr::str_extract(industries$industry, ".*?(?=$|[^a-zA-Z])")),
              year, sep = "_")

      industries
    }, names(sic_def), sic_def, SIMPLIFY = FALSE)


  # Merge to single dataframe -----------------------------------------------

  point_DA <- lapply(point_DA, \(x) {
    mapply(\(var_code, df) {
      names(df)[2] <- var_code
      df
    }, names(x), x, SIMPLIFY = FALSE)
  })

  point_DA <-
    Reduce(\(a, b) merge(a, b, by = "DA_ID", all = TRUE),
           lapply(point_DA,
                  \(x) Reduce(\(a, b) merge(a, b, by = "DA_ID", all = TRUE), x)))

  # Populate all the DA table
  out_df <- merge(sf::st_drop_geometry(DA_table),
                    point_DA, all = TRUE)
  out_df[is.na(out_df)] <- 0


  # Clean up dictionary and add 'Total' -------------------------------------

  dict <- Reduce(rbind, sic_def)[c("var_code", "industry", "exp", "source", "date")]
  dict$theme <- stringr::str_extract(dict$var_code, "^.*?(?=_)")
  themes <- unique(dict$theme)

  # Add total to themes with subthemes
  with_sub <- table(dict$theme)[table(dict$theme) > 1]
  for (i in names(with_sub)) {
    var_code <- paste0(i, "_total_2021")

    industry <- if (i == "education") {
      "Educational facilities"
    } else if (i == "fooddistribution") {
      "Food Distribution Depots"
    } else if (i == "healthcare") {
      "Hospital and Doctor Services"
    } else if (i == "retail") {
      "Retail Establishment"
    }

    exp <- if (i == "education") {
      "Establishments providing academic or technical instruction"
    } else if (i == "fooddistribution") {
      paste0("Establishments primarily engaged in selling food for home ",
             "preparation and consumption")
    } else if (i == "healthcare") {
      paste0("Establishments primarily engaged in furnishing medical, ",
             "surgical, and other health services to persons")
    } else if (i == "retail") {
      paste0("Establishments of retailing including shopping centers and ",
             "department stores")
    }

    source <- paste0(unique(dict$source[dict$theme == i]), collapse = " & ")
    date <- paste0(unique(dict$date[dict$theme == i]), collapse = " & ")

    dict <- rbind(dict, tibble::tibble(var_code = var_code,
                                       industry = industry,
                                       theme = i,
                                       exp = exp,
                                       source = source,
                                       date = date))
  }


  # Return tables and dictionaries ------------------------------------------

  return(list(data = tibble::as_tibble(out_df),
              dict = dict,
              themes = themes))

}

#' List pre-processed accessibility themes
#'
#' @return Returns a character vector of pre-processed accessibility themes
#' @export
list_accessibility_themes <- function() {
  cc.data::accessibility_themes
}








