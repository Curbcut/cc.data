#' Build CanALE index for a given year
#'
#' This function builds the Canadian Active Living Environment (CanALE) index
#' for a given year based on three measures: intersection density, dwelling
#' density, and points of interest (POI) density.
#'
#' @param years <`numeric`> Indicating the years of the streets network to use.
#' Options are census years starting from 2001. Defaults to \code{\link{census_years}}.
#' @param DA_table <`sf data.frame`> An \code{sf} object representing the
#' dissemination areas (DAs) to calculate the CanALE index for.
#'
#' @details Workflow from CanALE User Guide http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf
#'
#' @return A data.frame object representing the CanALE index for the given year
#' and DAs.
#' @export
build_canale <- function(years = cc.data::census_years[2:length(cc.data::census_years)],
                         DA_table) {

  # Intersection density measure --------------------------------------------

  # three_ways <- sapply(years, get_all_three_plus_ways,
  #                      simplify = FALSE, USE.NAMES = TRUE)
  # names(three_ways) <- years
  # qs::qsave(three_ways, "calculated_ignore/canale/three_ways.qs")
  three_ways <- qs::qread("calculated_ignore/canale/three_ways.qs")

  # sql_table_list <- cc.data::db_list_tables()
  # non_missing_ids <-
  #   DA_table$ID[paste0("ttm_foot_", DA_table$ID) %in% sql_table_list]
  # ttm_foot <- db_read_ttm(mode = "foot", DA_ID = non_missing_ids)
  # qs::qsave(ttm_foot, "calculated_ignore/canale/ttm_foot.qs")
  ttm_foot <- qs::qread("calculated_ignore/canale/ttm_foot.qs")
  ttm_foot <- ttm_foot[names(ttm_foot) %in% DA_table$ID]

  # Subset only the DAs in a 15 minutes walk
  ttm_foot <- lapply(ttm_foot, \(x) {
    x[x[[2]] <= (15*60), ]
  })

  # How many 3+ way intersection per DA buffer
  DA_table_tw <- DA_table
  intersects <- lapply(three_ways, \(x) sf::st_intersects(DA_table_tw, x))
  DA_table_tw <- lapply(intersects, \(x) {
    nb <- lengths(x)
    # Add the number of intersects in the DA_table
    df <- DA_table_tw
    df$three_ways <- nb
    df
  })
  DA_table_tw <- lapply(DA_table_tw, sf::st_drop_geometry)

  # How many intersections in the DAs accessible in a 15 minute walks
  progressr::with_progress({
    p <- progressr::progressor(length(ttm_foot) * length(DA_table_tw))
    DA_table_tw <- lapply(DA_table_tw, \(da_year) {
      out <- future.apply::future_lapply(seq_along(ttm_foot), \(i) {
        x <- ttm_foot[[i]]
        ID <- names(ttm_foot)[[i]]
        all <- da_year$three_ways[da_year$ID %in% x$DA_ID]
        p()
        tibble::tibble(ID = ID,
                       three_ways = sum(all))
      })
      Reduce(rbind, out)
    })
  })

  DA_table_tw_yr <- lapply(seq_along(DA_table_tw), \(i) {
    df <- DA_table_tw[[i]]
    df$year <- years[[i]]
    df
  })

  DA_table_tw_yr <- Reduce(rbind, DA_table_tw_yr)
  DA_table_tw_yr$int_d <- ecdf(DA_table_tw_yr$three_ways)(DA_table_tw_yr$three_ways)

  DA_table_tw_yr <- lapply(seq_along(years), \(i) {
    out <- DA_table_tw_yr[DA_table_tw_yr$year == years[[i]], ]
    out <- out[c(1, 4)]
    names(out)[2] <- paste0("int_d_", years[[i]])
    return(out)
  })

  int_d <- Reduce(merge, DA_table_tw_yr) |>
    tibble::as_tibble()


  # # Calculate the mean and standard deviation
  # unlisted_tw_values <- lapply(DA_table_tw, `[[`, "three_ways") |> unlist()
  # mean_data <- mean(unlisted_tw_values, na.rm = TRUE)
  # sd_data <- stats::sd(unlisted_tw_values, na.rm = TRUE)
  #
  # # Calculate the z-score for each data point
  # DA_table_tw <- mapply(\(x, year) {
  #   df <- DA_table
  #   x <- x[order(x$ID, df$ID), ]
  #   df[[paste0("int_d_", year)]] <- (x$three_ways - mean_data) / sd_data
  #   df
  # }, DA_table_tw, names(DA_table_tw), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  #
  # int_d <- Reduce(\(x, y) merge(x, y, by = "ID"),
  #                 lapply(DA_table_tw, sf::st_drop_geometry))


  # Dwelling density measure ------------------------------------------------

  dwell_vecs <- c(CA21 = "v_CA21_4", CA16 = "v_CA16_404", CA11 = "v_CA11F_2",
                  CA06 = "v_CA06_98", CA01 = "v_CA01_96")

  cancensus_dataset <- paste0("CA", gsub("^20", "", years))

  # Get dwellings information from the census
  dwellings <-
    lapply(cancensus_dataset, \(c) {
      tryCatch({cancensus::get_census(
        dataset = c,
        regions = list(C = 01),
        level = "DA",
        vectors = c(dwellings = unname(dwell_vecs[c])),
        geo_format = "sf",
        quiet = TRUE
      )}, error = function(e) {
        regions <- cancensus::list_census_regions(c)
        regions <- regions$region[regions$level == "PR"]
        regions <- lapply(regions, \(r) {
          cancensus::get_census(
            dataset = c,
            regions = list(PR = r),
            level = "DA",
            vectors = c(dwellings = unname(dwell_vecs[c])),
            geo_format = "sf",
            quiet = TRUE
          )
        })
        regions <- lapply(regions, `[`, c("GeoUID", "dwellings", "geometry"))
        return(Reduce(rbind, regions))
      })
    })

  # Calculate dwelling density using the most recent DA table
  dwellings <- lapply(dwellings, \(dw) {
    dw <- sf::st_transform(dw, 3347)
    dw <- sf::st_make_valid(dw)

    dw$previous_area <- get_area(dw)
    dwellings_cut <- sf::st_intersection(DA_table, dw)
    dwellings_cut$new_area <- get_area(dwellings_cut)
    dwellings_cut <- sf::st_drop_geometry(dwellings_cut)

    dwellings_cut$area_pct <- dwellings_cut$new_area / dwellings_cut$previous_area
    dwellings_cut$n_dwellings <- dwellings_cut$dwellings * dwellings_cut$area_pct

    # For each ID, get the density
    dwelling_density <- stats::aggregate(cbind(n_dwellings, new_area) ~ ID,
                                  data = dwellings_cut,
                                  FUN = \(...) sum(..., na.rm = TRUE))
    dwelling_density$density <- dwelling_density$n_dwellings / dwelling_density$new_area
    dwelling_density
  })

  # Dwelling density for every DA in the 15 minutes walk
  progressr::with_progress({
    p <- progressr::progressor(length(dwellings) * length(ttm_foot))
    dwellings <- lapply(dwellings, \(dw) {
      out <- future.apply::future_lapply(seq_along(ttm_foot), \(i) {
        x <- ttm_foot[[i]]
        ID <- names(ttm_foot)[[i]]
        all <- dw[dw$ID %in% x$DA_ID, ]
        p()
        tibble::tibble(ID = ID,
                       density = stats::weighted.mean(all$density, all$new_area))
      })
      Reduce(rbind, out)
    })
  })


  dwellings_yr <- lapply(seq_along(dwellings), \(i) {
    df <- dwellings[[i]]
    df$year <- years[[i]]
    df
  })

  dwellings_yr <- Reduce(rbind, dwellings_yr)
  dwellings_yr$dwl_d <- ecdf(dwellings_yr$density)(dwellings_yr$density)

  dwellings_yr <- lapply(seq_along(years), \(i) {
    out <- dwellings_yr[dwellings_yr$year == years[[i]], ]
    out <- out[c(1, 4)]
    names(out)[2] <- paste0("dwl_d_", years[[i]])
    return(out)
  })

  dwl_d <- Reduce(merge, dwellings_yr) |>
    tibble::as_tibble()





  # # Calculate the mean and standard deviation
  # unlisted_d_values <- lapply(dwellings, `[[`, "density") |> unlist()
  # mean_data <- mean(unlisted_d_values, na.rm = TRUE)
  # sd_data <- stats::sd(unlisted_d_values, na.rm = TRUE)
  #
  # # Calculate the z-score for each data point
  # dwellings_z <- mapply(\(x, year) {
  #   df <- DA_table
  #   x <- x[order(x$ID, df$ID), ]
  #   df[[paste0("dwl_d_", year)]] <- (x$density - mean_data) / sd_data
  #   df
  # }, dwellings, years, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  #
  # # Merge
  # dwl_d <- Reduce(\(x, y) merge(x, y, by = "ID"),
  #                 lapply(dwellings_z, sf::st_drop_geometry))

  # Points of interest measure ----------------------------------------------

  pois <- sapply(years, \(year) {

    poi_year <- if (year == 2001) 2002 else year

    # Prepare to grab poi data from the bucket
    files <- bucket_list_content("curbcut.amenities")$Key
    txt_shp <- files[grepl("poi/.*(txt$|zip$)", files)]
    year_shps <- txt_shp[grepl(poi_year, txt_shp)]

    poi <- lapply(year_shps, \(f) {
      # If it is a txt file, read it from the bucket with the read.csv method
      if (grepl("\\.txt$", f)) {
        out <- bucket_read_object(object = f,
                                  objectext = ".txt",
                                  method = utils::read.csv,
                                  bucket = "curbcut.amenities")
        return(out)
      }

      # If it's not a txt file, it's a shapefile. Read it using the following
      # function.
      bucket_read_object_zip_shp(object = f, bucket = "curbcut.amenities")
    })
    poi <- Reduce(rbind, poi)

    # Get the right column indicating the SIC codes
    sic_col <- if (year %in% c(2001)) "SIC.1" else "SIC_1"

    # Grab the major groups (from the SIC code)
    poi$sic_major_group <- stringr::str_extract(poi[[sic_col]], "^\\d{2}")

    # Get a list of all the major group that we want to keep as amenities
    list_items <- rvest::read_html("https://www.osha.gov/data/sic-manual") |>
      rvest::html_elements("li")
    filtered_list_items <-
      list_items[stringr::str_detect(rvest::html_text(list_items), "Division")]

    divisions <- lapply(filtered_list_items, \(li) {
      # Extract the title of the <li>
      title <- rvest::html_text(rvest::html_element(li, "a"))
      # Extract the <ul> elements for the current <li>
      ul_elements <- rvest::html_elements(li, "a")
      ul_vec <- sapply(ul_elements, \(x) {
        stringr::str_extract(rvest::html_text(x), "(?<=Major Group )\\d{2}")
      })
      ul_vec <- ul_vec[!is.na(ul_vec)]
      # Return
      tibble::tibble(division = title,
                     major_groups = as.numeric(ul_vec))
    })
    divisions <- tibble::as_tibble(data.table::rbindlist(divisions))

    # Filter in only the amenities we are interested in
    amenities <- c("Division G: Retail Trade",
                   "Division H: Finance, Insurance, And Real Estate",
                   "Division I: Services")
    major_groups <- divisions$major_groups[divisions$division %in% amenities]
    # Manually add missing major groups
    major_groups <- c(major_groups, 41)
    poi <- poi[poi$sic_major_group %in% major_groups, ]

    # Make it as SF
    poi <- sf::st_as_sf(poi, coords = c("X", "Y"), crs = 4326)
    poi <- sf::st_transform(poi, crs = 3347)

    # Calculate the POI number for each DA
    df <- DA_table
    intersects <- sf::st_intersects(df, poi)
    df$poi <- lengths(intersects)

    # For each DA, calculate the number of POI in the other DAs accessible in
    # a 15 minutes walk
    df <- sf::st_drop_geometry(df)
    df$poi_15min <- future.apply::future_sapply(df$ID, \(x) {
      das_15min <- ttm_foot[[which(names(ttm_foot) == x)]]$DA_ID

      sum(df$poi[df$ID %in% das_15min])
    })

    # Return the poi score for the year
    df <- df[c(1,3)]
    return(df)
  }, simplify = FALSE, USE.NAMES = TRUE)


  pois_yr <- lapply(seq_along(pois), \(i) {
    df <- pois[[i]]
    df$year <- years[[i]]
    df
  })

  pois_yr <- Reduce(rbind, pois_yr)
  pois_yr$poi <- ecdf(pois_yr$poi_15min)(pois_yr$poi_15min)

  pois_yr <- lapply(seq_along(years), \(i) {
    out <- pois_yr[pois_yr$year == years[[i]], ]
    out <- out[c(1, 4)]
    names(out)[2] <- paste0("poi_", years[[i]])
    return(out)
  })

  poi <- Reduce(merge, pois_yr) |>
    tibble::as_tibble()

  # # Calculate the mean and standard deviation
  # unlisted_p_values <- lapply(pois, `[[`, "poi_15min") |> unlist()
  # mean_data <- mean(unlisted_p_values, na.rm = TRUE)
  # sd_data <- stats::sd(unlisted_p_values, na.rm = TRUE)
  #
  # # Calculate the z-score for each data point
  # poi_z <- mapply(\(x, year) {
  #   df <- DA_table
  #   x <- x[order(x$ID, df$ID), ]
  #   df[[paste0("poi_", year)]] <- (x$poi_15min - mean_data) / sd_data
  #   df
  # }, pois, years, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  #
  # # Merge
  # poi <- Reduce(\(x, y) merge(x, y, by = "ID"),
  #                 lapply(poi_z, sf::st_drop_geometry))


  # Sum the three z index for the final score -------------------------------

  DA_table <- sf::st_drop_geometry(DA_table)

  canale <- lapply(years, \(year) {
    df <-
      Reduce(\(x, y) merge(x, y, by = "ID"),
             list(int_d[c(1, which(grepl(paste0("int_d_", year), names(int_d))))],
                  dwl_d[c(1, which(grepl(paste0("dwl_d_", year), names(dwl_d))))],
                  poi[c(1, which(grepl(paste0("poi_", year), names(poi))))]))

    df[[paste0("canale_", year)]] <- rowSums(df[2:4])
    df <- df[c(1,5)]
    df[is.na(df)] <- NA

    return(df)
  })

  canale <- tibble::as_tibble(Reduce(merge, canale))

  # Return ------------------------------------------------------------------

  return(canale)
}

#' Get all three+ way intersections in Canadian streets for a given year
#'
#' This function downloads the streets network for a given year and returns all
#' the intersections with three or more streets.
#'
#' @param year <`numeric`> indicates the year of the streets network to use.
#' Options are census years starting 2001.
#'
#' @return An \code{sf} object representing the three+ way intersections in the
#' streets network.
#' @export
get_all_three_plus_ways <- function(year) {#, OSM_cache = TRUE) {
  # # Grab 2021 streets from OSM
  # osm_pbf <- "http://download.geofabrik.de/north-america/canada-210101.osm.pbf"
  # streets <- osmextract::oe_read(osm_pbf, layer = "lines",
  #                              quiet = FALSE, force_download = !OSM_cache,
  #                               max_file_size = Inf)
  #
  # # Retain footbpaths and exclude limited-access highways and their entrances and
  # # exits.
  # fclasses <- c('trunk', 'primary', 'primary', 'secondary', 'tertiary',
  #               'unclassified', 'residential', 'pedestrian', 'living_street',
  #               'footway', 'steps', 'path')
  # streets <- streets[streets$highway %in% fclasses, ]
  # # Transform streets to the Stats Can CRS
  # streets <- sf::st_transform(streets, 3347)

  # Download the streets network
  url <- cc.data::road_network_url$url[cc.data::road_network_url$year == year]
  temp_zip_file <- tempfile()
  utils::download.file(url, temp_zip_file)
  temp_folder <- tempfile()

  # get the shapefile name
  listed_files <- utils::unzip(temp_zip_file, list = TRUE)$Name
  shp <- if (year == 2001) {
    stringr::str_subset(listed_files, ".e00$")
  } else {
    listed_files <- utils::unzip(temp_zip_file, list = TRUE)$Name
    stringr::str_subset(listed_files, ".shp$")
  }
  utils::unzip(temp_zip_file, exdir = temp_folder)

  streets <- sf::st_read(paste0(temp_folder, "/", shp))
  streets <- sf::st_transform(streets, 3347)

  # Filter out highways
  if (year == 2001) {
    streets <- streets[streets$RANK1 == 0 & streets$RANK2 == 0, ]
  } else if (year == 2006) {
    streets <- streets[!streets$RANK %in% c("1", "2"), ]
  } else {
    streets <- streets[!streets$RANK %in% c("1", "2", "3"), ]
  }

  # Make a grid on all Canada to separate the work charge to different cores
  grid <- sf::st_make_grid(streets, n = c(500, 200))
  grid <- cbind(tibble::tibble(ID = seq_along(grid)), grid)
  grid <- sf::st_as_sf(tibble::as_tibble(grid))
  grid <- sf::st_filter(grid, streets)

  # Shuffle the grid so that each core handles a similar amount of calculations
  grid <- grid[sample(grid), ]
  # Get all the three way intersections in all the grids
  grid_intersects <- sf::st_intersects(grid, streets)
  progressr::with_progress({
    pb <- progressr::progressor(length(grid_intersects))
    three_ways <- future.apply::future_lapply(seq_along(grid_intersects), \(x) {
      out <- sf::st_intersection(grid[x, ], streets[grid_intersects[[x]], ])
      if (nrow(out) == 0) return({
        pb()
        NULL})
      out <- sf::st_intersection(out)
      out <- out[sf::st_is(out, type = "POINT"), ]
      pb()
      tibble::tibble(unique(out["geometry"]))
    })
  })
  three_ways <- three_ways[!sapply(three_ways, is.null)]
  three_ways <- three_ways[sapply(three_ways, nrow) > 0]
  three_ways <- data.table::rbindlist(three_ways)

  # Get all the three way intersections in streets that touch the lines of the
  # grid and that we potentially missed in the first step
  grid_lines <- sf::st_cast(grid, "MULTILINESTRING")
  streets_on_lines <- sf::st_filter(streets, grid_lines)
  streets_on_lines <- sf::st_intersection(streets_on_lines)
  streets_on_lines <-
    streets_on_lines[sf::st_is(streets_on_lines, type = "POINT"), ]
  streets_on_lines <- unique(streets_on_lines["geometry"])
  names(streets_on_lines) <- c("geometry")

  # Bind all three+ way intersections
  three_ways <- rbind(three_ways, streets_on_lines)
  three_ways <- tibble::as_tibble(three_ways) |> sf::st_as_sf() |> unique()
  three_ways$ID <- paste0("tw_", seq_along(three_ways$geometry))

  # Return
  return(three_ways)
}
