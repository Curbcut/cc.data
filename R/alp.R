#' Build ALP index for a given year
#'
#' This function builds the Active Living Potential dataset
#' for a given year based on three measures: intersection density, dwelling
#' density, and points of interest (POI) density.
#'
#' @param years <`numeric`> Indicating the years of the streets network to use.
#' Options are census years starting from 2001. Defaults to \code{\link{census_years}}.
#' @param DB_table <`sf data.frame`> An \code{sf} object representing the
#' dissemination areas (DAs) to calculate the CanALE index for.
#'
#' @details Workflow from CanALE User Guide http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf
#'
#' @return A data.frame object representing the CanALE index for the given year
#' and DAs.
#' @export
build_alp <- function(
  years = census_years[2:length(cc.data::census_years)],
  DB_table = bucket_read_object_zip_shp("DB_shp_carto.zip", "curbcut.rawdata")
) {
  # Intersection density measure --------------------------------------------

  three_ways <- sapply(
    years,
    get_all_three_plus_ways,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  names(three_ways) <- years
  # qs::qsave(three_ways, "calculated_ignore/alp/three_ways.qs")
  # three_ways <- qs::qread("calculated_ignore/alp/three_ways.qs")

  # Get travel time matrices by foot. Only 900 seconds (15 minutes walk)
  ttm_foot <- {
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    query <- "SELECT * FROM ttm_foot_DB WHERE travel_seconds < 900"
    DBI::dbGetQuery(conn, query)
  }
  # In the ttm_foot, add self
  ttm_foot <- rbind(
    ttm_foot,
    tibble::tibble(
      from = DB_table$DBUID,
      to = DB_table$DBUID,
      travel_seconds = 0
    )
  )
  # qs::qsave(ttm_foot, "calculated_ignore/alp/ttm_foot.qs")
  # ttm_foot <- qs::qread("calculated_ignore/alp/ttm_foot.qs")

  # How many 3+ way intersection per DB buffer
  DB_table_tw <- DB_table
  intersects <- lapply(three_ways, \(x) sf::st_intersects(DB_table_tw, x))
  DB_table_tw <- lapply(intersects, \(x) {
    # Add the number of intersects in the DB_table
    df <- DB_table_tw
    df$three_ways <- lengths(x)
    df
  })
  DB_table_tw <- lapply(DB_table_tw, sf::st_drop_geometry)

  # How many intersections in the DAs accessible in a 15 minute walks

  # Convert ttm_foot to a more efficient lookup structure
  data.table::setDT(ttm_foot)

  # Function to process each db_year
  process_db_year <- function(db_year) {
    require(data.table)
    data.table::setDT(db_year)

    # Ensure 'DBUID' in 'db_year' and 'from' in 'ttm_foot' are of the same type (character)
    db_year[, DBUID := as.character(DBUID)]
    ttm_foot[, from := as.character(from)]
    ttm_foot[, to := as.character(to)]

    # Create a key for joining
    data.table::setkey(ttm_foot, from)

    # Join 'ttm_foot' to 'db_year', carrying over the 'three_ways' value
    # This creates a table with every 'from' matched to its 'to' and includes the 'three_ways' for each 'to'
    joined <- ttm_foot[
      db_year,
      .(from, to, three_ways = i.three_ways),
      on = .(to = DBUID),
      nomatch = 0
    ]

    # Aggregate the 'three_ways' by 'from', effectively summing for each original DBUID based on its 'to' DBUIDs
    result <- joined[, .(three_ways = sum(three_ways, na.rm = TRUE)), by = from]

    return(result)
  }

  # Apply the function to each db_year
  DB_table_tw <- lapply(DB_table_tw, process_db_year)

  # Add a year column
  DB_table_tw_yr <- lapply(seq_along(DB_table_tw), \(i) {
    df <- DB_table_tw[[i]]
    df$year <- years[[i]]
    df
  })

  DB_table_tw_yr <- Reduce(rbind, DB_table_tw_yr)
  DB_table_tw_yr_fin <- DB_table_tw_yr
  DB_table_tw_yr_fin$int_d <- stats::ecdf(DB_table_tw_yr$three_ways)(
    DB_table_tw_yr$three_ways
  )

  DB_table_tw_yr_fin <- lapply(seq_along(years), \(i) {
    out <- DB_table_tw_yr_fin[DB_table_tw_yr_fin$year == years[[i]], ]
    out <- out[, c(1, 4)]
    names(out)[2] <- paste0("int_d_", years[[i]])
    return(out)
  })

  int_d <- Reduce(merge, DB_table_tw_yr_fin) |>
    tibble::as_tibble()

  # Dwelling density measure ------------------------------------------------

  cancensus_dataset <- paste0("CA", gsub("^20", "", years))

  # Get dwellings information from the census
  progressr::with_progress({
    pb <- progressr::progressor(steps = length(cancensus_dataset) * 13)
    dwellings <-
      lapply(cancensus_dataset, \(c) {
        regions <- cancensus::list_census_regions(c)
        regions <- regions$region[regions$level == "PR"]
        regions <- lapply(regions, \(r) {
          out <- cancensus::get_census(
            dataset = c,
            regions = list(PR = r),
            level = "DB",
            geo_format = NA,
            quiet = TRUE
          )
          pb()
          out
        })
        regions <- lapply(regions, `[`, c("GeoUID", "Dwellings"))
        return(Reduce(rbind, regions))
      })
  })
  names(dwellings) <- years

  # Get the spatial features of DBs
  DB_sf <- sapply(
    years,
    \(x) {
      file <- sprintf("DB_shp_%s.zip", x)
      if (x == 2021) file <- "DB_shp_carto.zip"
      cc.data::bucket_read_object_zip_shp(file, "curbcut.rawdata")
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  dwellings_sf <- mapply(
    \(dw, sf) {
      other_name_col <- "BLOCKUID" %in% names(sf)
      col_name <- if (other_name_col) "BLOCKUID" else "DBUID"
      merge(
        dw,
        sf[c(col_name, "geometry")],
        by.x = "GeoUID",
        by.y = col_name
      ) |>
        tibble::as_tibble() |>
        sf::st_as_sf()
    },
    dwellings,
    DB_sf,
    SIMPLIFY = FALSE
  )
  qs::qsave(dwellings_sf, file = "calculated_ignore/alp/dwellings_sf.qs")

  # Calculate dwelling density using the most recent DA table
  dwellings <- lapply(dwellings_sf, \(dw) {
    dw <- sf::st_transform(dw, 3347)
    dw <- sf::st_make_valid(dw)

    dw$previous_area <- get_area(dw)
    dwellings_cut <- sf::st_intersection(DB_table, dw)
    dwellings_cut$new_area <- get_area(dwellings_cut)
    dwellings_cut <- sf::st_drop_geometry(dwellings_cut)

    dwellings_cut$area_pct <- dwellings_cut$new_area /
      dwellings_cut$previous_area
    dwellings_cut$n_dwellings <- dwellings_cut$Dwellings *
      dwellings_cut$area_pct

    # For each ID, get the density
    dwelling_density <- stats::aggregate(
      cbind(n_dwellings, new_area) ~ DBUID,
      data = dwellings_cut,
      FUN = \(...) sum(..., na.rm = TRUE)
    )
    dwelling_density$density <- dwelling_density$n_dwellings /
      dwelling_density$new_area
    dwelling_density
  })

  # Function to process each db_year
  process_db_year <- function(db_year) {
    # Load required package
    require(data.table)

    ttm_foot_dt <- data.table(ttm_foot)
    db_year_dt <- data.table(db_year)

    # Convert 'DBUID' in 'db_year' and 'from', 'to' in 'ttm_foot' to character to ensure matching types
    db_year_dt[, DBUID := as.character(DBUID)]
    ttm_foot_dt[, from := as.character(from)]
    ttm_foot_dt[, to := as.character(to)]

    # Ensure 'db_year' is a data.table and set keys for joining
    setkey(db_year_dt, DBUID)
    setkey(ttm_foot_dt, from, to)

    # Join 'ttm_foot' to 'db_year' on 'to' matching 'DBUID' to bring 'density' and 'new_area' over to 'ttm_foot'
    # Note: Using 'new_area' as weights requires it to be available in the final joined table
    joined <- ttm_foot_dt[
      db_year_dt,
      .(from, to, density = i.density, new_area = i.new_area),
      on = .(to = DBUID),
      nomatch = 0
    ]

    # Compute weighted mean of 'density' by 'from', using 'new_area' as weights
    result <- joined[,
      .(density_weighted_mean = weighted.mean(density, new_area, na.rm = TRUE)),
      by = from
    ]

    return(result)
  }

  # Apply the function to each db_year
  dwellings <- lapply(dwellings, process_db_year)

  # Add a year column
  dwellings_yr <- lapply(seq_along(dwellings), \(i) {
    df <- dwellings[[i]]
    df$year <- years[[i]]
    df
  })

  dwellings_yr <- Reduce(rbind, dwellings_yr)
  dwellings_yr_fin <- dwellings_yr
  dwellings_yr_fin$dwl_d <- stats::ecdf(dwellings_yr$density)(
    dwellings_yr$density
  )

  dwellings_yr_fin <- lapply(seq_along(years), \(i) {
    out <- dwellings_yr_fin[dwellings_yr_fin$year == years[[i]], ]
    out <- out[, c(1, 4)]
    names(out)[2] <- paste0("dwl_d_", years[[i]])
    return(out)
  })

  dwl_d <- Reduce(merge, dwellings_yr_fin) |>
    tibble::as_tibble()

  # Points of interest measure ----------------------------------------------

  pois <- sapply(
    years,
    \(year) {
      poi_year <- if (year == 2001) 2002 else year

      # Prepare to grab poi data from the bucket
      files <- bucket_list_content("curbcut.amenities")$Key
      txt_shp <- files[grepl("poi/.*(txt$|zip$)", files)]
      year_shps <- txt_shp[grepl(poi_year, txt_shp)]

      poi <- lapply(year_shps, \(f) {
        # If it is a txt file, read it from the bucket with the read.csv method
        if (grepl("\\.txt$", f)) {
          out <- bucket_read_object(
            object = f,
            objectext = ".txt",
            method = utils::read.csv,
            bucket = "curbcut.amenities"
          )
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

      # # Filter in only the amenities we are interested in
      # amenities <- c("Division G: Retail Trade",
      #                "Division H: Finance, Insurance, And Real Estate",
      #                "Division I: Services")
      # major_groups <- divisions$major_groups[divisions$division %in% amenities]
      major_groups <- c(41, 52:89)
      # Manually add missing major groups
      major_groups <- c(major_groups, 41)
      poi <- poi[poi$sic_major_group %in% major_groups, ]

      # Make it as SF
      poi <- sf::st_as_sf(poi, coords = c("X", "Y"), crs = 4326)
      poi <- sf::st_transform(poi, crs = 3347)

      # Calculate the POI number for each DA
      df <- DB_table
      intersects <- sf::st_intersects(df, poi)
      df$poi <- lengths(intersects)

      # For each DA, calculate the number of POI in the other DAs accessible in
      # a 15 minutes walk
      df <- sf::st_drop_geometry(df)
      require(data.table)
      df <- data.table::data.table(df)
      ttm_foot_dt <- data.table::data.table(ttm_foot)
      df[, DBUID := as.character(DBUID)]
      ttm_foot_dt[, from := as.character(from)]
      ttm_foot_dt[, to := as.character(to)]
      data.table::setkey(ttm_foot, from)
      joined <- ttm_foot_dt[
        df,
        .(from, to, poi = i.poi),
        on = .(to = DBUID),
        nomatch = 0
      ]
      result <- joined[, .(poi = sum(poi, na.rm = TRUE)), by = from]

      # Return the poi score for the year
      return(result)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  pois_yr <- lapply(seq_along(pois), \(i) {
    df <- pois[[i]]
    df$year <- years[[i]]
    df
  })

  pois_yr <- Reduce(rbind, pois_yr)
  pois_yr_fin <- pois_yr
  pois_yr_fin$poi <- stats::ecdf(pois_yr_fin$poi)(pois_yr_fin$poi)

  pois_yr_fin <- lapply(seq_along(years), \(i) {
    out <- pois_yr_fin[pois_yr_fin$year == years[[i]], ]
    out <- out[, c(1, 2)]
    names(out)[2] <- paste0("poi_", years[[i]])
    return(out)
  })

  poi <- Reduce(merge, pois_yr_fin) |>
    tibble::as_tibble()

  # Sum the three z index for the final score -------------------------------

  DB_table <- sf::st_drop_geometry(DB_table)

  alp <- lapply(years, \(year) {
    df <-
      Reduce(
        \(x, y) merge(x, y, by = "from"),
        list(
          int_d[c(1, which(grepl(paste0("int_d_", year), names(int_d))))],
          dwl_d[c(1, which(grepl(paste0("dwl_d_", year), names(dwl_d))))],
          poi[c(1, which(grepl(paste0("poi_", year), names(poi))))]
        )
      )

    df[[paste0("alp_", year)]] <- rowSums(df[2:4])
    df <- df[c(1, 5)]
    df[is.na(df)] <- NA

    return(df)
  })

  alp_final <- tibble::as_tibble(Reduce(merge, alp))
  names(alp_final)[1] <- "ID"

  #   # Create a statistical model ----------------------------------------------
  #
  #   formod <- Reduce(merge, list(DB_table_tw_yr, dwellings_yr, pois_yr))
  #   alp_formod <- mapply(\(df, year) {
  #     names(df)[2] <- "alp"
  #     df$year <- year
  #     df
  #   }, alp, years, SIMPLIFY = FALSE)
  #   alp_formod <- Reduce(rbind, alp_formod)
  #   formod <- merge(formod, alp_formod)
  #   formod <- formod[!is.na(formod$alp), ]
  #
  #   lm(alp ~ three_ways + density + poi_15min, data = formod) |> summary()

  # Return ------------------------------------------------------------------

  return(alp_final)
}

#' Get all three+ way intersections in Canadian streets for a given year
#'
#' This function downloads the streets network for a given year and returns all
#' the intersections with three or more streets.
#'
#' @param year <`numeric`> indicates the year of the streets network to use.
#' Options are census years starting 2001.
#' @param crs <`numeric`> CRS
#'
#' @return An \code{sf} object representing the three+ way intersections in the
#' streets network.
#' @export
get_all_three_plus_ways <- function(year, crs) {
  #, OSM_cache = TRUE) {
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
  streets <- sf::st_transform(streets, crs = 3347)

  # Filter out highways
  if (year == 2001) {
    streets <- streets[streets$RANK1 == 0 & streets$RANK2 == 0, ]
  } else if (year <= 2006) {
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
      if (nrow(out) == 0)
        return({
          pb()
          NULL
        })
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
