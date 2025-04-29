#' @title Split Ottawa-Gatineau CMA by Province
#' @description This function retrieves census metropolitan areas (CMAs) and provinces for a given census year,
#' identifies Ottawa-Gatineau (B), and splits it into two separate CMAs: one for Ontario and one for Quebec.
#' The function assigns appropriate names and GeoUIDs to the split regions.
#'
#' @param census_year Character. The census year (e.g., "CA21").
#' @return sf object containing CMAs with Ottawa-Gatineau split by province.
#' @export
cma_split_ottawa <- function(census_year = "CA21") {
  
  # Retrieve list of CMAs and Provinces
  census_regions <- cancensus::list_census_regions(census_year)
  cma_list <- census_regions |> dplyr::filter(level == "CMA") |> dplyr::select(region, name)
  province_list <- census_regions |> dplyr::filter(level == "PR") |> dplyr::select(region, name)
  
  # Retrieve spatial census data for CMAs and provinces
  cma_all <- cancensus::get_census(dataset = census_year, 
                                   regions = list(CMA = cma_list$region), 
                                   geo_format = "sf", 
                                   level = "CMA")
  
  provinces_sf <- cancensus::get_census(dataset = census_year, 
                                        regions = list(PR = province_list$region), 
                                        geo_format = "sf", 
                                        level = "PR")
  
  # Identify Ottawa-Gatineau CMA and corresponding provinces
  ottawa_cma <- dplyr::filter(cma_all, name == "Ottawa - Gatineau (B)")
  ottawa_provinces <- dplyr::filter(provinces_sf, name %in% c("Ontario (Ont.)", "Quebec (Que.)"))
  
  # Split Ottawa-Gatineau 
  if (nrow(ottawa_cma) > 0 & nrow(ottawa_provinces) > 0) {
    
    ottawa_split <- sf::st_intersection(ottawa_cma, ottawa_provinces) |>
      dplyr::mutate(
        name = dplyr::case_when(
          name.1 == "Ontario (Ont.)" ~ "Ottawa - Ontario (B)",
          name.1 == "Quebec (Que.)" ~ "Ottawa - Québec (B)"
        ),
        GeoUID = dplyr::case_when( # Assign appropriate GeoUIDs
          name.1 == "Ontario (Ont.)" ~ "35505",
          name.1 == "Quebec (Que.)" ~ "24505"
        )
      ) |>
      dplyr::select(GeoUID, name, geometry) 
    
    # Remove original Ottawa-Gatineau and add split regions
    cma_all <- dplyr::filter(cma_all, name != "Ottawa - Gatineau (B)") |>
      dplyr::bind_rows(ottawa_split)
  }
  
  # Final formatting
  cma_all <- cma_all |>
    dplyr::rename(geouid = GeoUID) |> 
    dplyr::select(geouid, name, geometry) |> 
    dplyr::arrange(name)
  
  return(cma_all)
}

#' CMHC CMA Data load
#'
#' This script provides a suite of functions to fetch, process, reshape, and standardize
#' CMHC housing data for CMAs using cmhc package.
#' It includes functions to retrieve data from the CMHC API, clean and format the results,
#' reshape the data into a structured format, and apply standardized abbreviations to column
#' and table names.
#'
#' The function line is designed to handle multiple requests, process data for different geographical
#' identifiers.
#'

#' Fetch CMHC Data
#'
#' Retrieves data from the CMHC API for a specified CMA level.
#'
#' @param survey The CMHC survey to query (e.g., "Rms").
#' @param series The data series to retrieve (e.g., "Vacancy Rate").
#' @param dimension The dimension to filter by (e.g., "Structure Type").
#' @param breakdown The breakdown level for the data (e.g., "Historical Time Periods").
#' @param geo_uid The geographical identifier for the CMA.
#' @return A data frame containing the retrieved CMHC data, or `NULL` if an error occurs.
cmhc_fetch_cma_data <- function(survey, series, dimension, breakdown, geo_uid) {
  tryCatch(
    cmhc::get_cmhc(
      survey = survey, 
      series = series, 
      dimension = dimension, 
      breakdown = breakdown, 
      geo_uid = geo_uid),
    error = function(e) {
      return(NULL)
    }
  )
}

#' Process CMHC Results
#'
#' Cleans and formats the retrieved CMHC data. Converts `GeoUID` to character,
#' extracts year and month from `DateString`, and renames columns for consistency.
#'
#' @param results A data frame containing raw CMHC data.
#' @return A cleaned and processed data frame with standardized column names and formats.
cmhc_process_cma <- function(results) {
  if (!is.null(results) && nrow(results) > 0) {
    results <- results |> dplyr::mutate(GeoUID = as.character(GeoUID))  
    
    if ("DateString" %in% colnames(results)) {
      results <- results |> 
        dplyr::rename(year_month = DateString) |> 
        dplyr::mutate(
          year = as.numeric(stringr::str_extract(year_month, "\\d{4}")),
          month = dplyr::case_when(
            stringr::str_detect(year_month, "Jan|January") ~ "01",
            stringr::str_detect(year_month, "Feb|February") ~ "02",
            stringr::str_detect(year_month, "Mar|March") ~ "03",
            stringr::str_detect(year_month, "Apr|April") ~ "04",
            stringr::str_detect(year_month, "May") ~ "05",
            stringr::str_detect(year_month, "Jun|June") ~ "06",
            stringr::str_detect(year_month, "Jul|July") ~ "07",
            stringr::str_detect(year_month, "Aug|August") ~ "08",
            stringr::str_detect(year_month, "Sep|September") ~ "09",
            stringr::str_detect(year_month, "Oct|October") ~ "10",
            stringr::str_detect(year_month, "Nov|November") ~ "11",
            stringr::str_detect(year_month, "Dec|December") ~ "12",
            TRUE ~ ""
          ),
          year_month = paste0(year, month)
        ) |> dplyr::select(-month)
    } else if ("Year" %in% colnames(results)) {
      results <- results |> 
        dplyr::rename(year_month = Year) |> 
        dplyr::mutate(year_month = as.character(year_month))
    }
  }
  return(results)
}

#' Clean Column Names
#'
#' Standardizes column names by trimming spaces, converting to lowercase,
#' replacing special characters with underscores, and removing trailing underscores.
#'
#' @param name_vector A vector of column names to clean.
#' @return A vector of cleaned and standardized column names.
cmhc_clean_cma_names <- function(name_vector) {
  name_vector |> 
    stringr::str_trim() |>  
    stringr::str_to_lower() |>  
    stringr::str_replace_all("[^a-z0-9_ ]", "_") |>  
    stringr::str_replace_all("\\s+", "_") |>  
    stringr::str_replace_all("_+", "_") |>  
    stringr::str_replace_all("_$", "")  
}

#' @title Reshape CMHC Results
#'
#' @description Transforms the CMHC raw data into a structured format by filtering and pivoting
#' based on series and dimension values. Automatically simplifies column names when the data is annual.
#'
#' @param results The CMHC data frame to reshape.
#' @param dimension The column name used as a filter for restructuring (e.g., "Structure Type").
#' @return A named list of reshaped data frames, each corresponding to a unique Series and Dimension.
cmhc_reshape_cma_results <- function(results, dimension) {
  results_list <- list()
  
  if (!is.null(results) && nrow(results) > 0) {
    for (s in unique(results$Series)) {
      
      if (!is.null(dimension)) {
        for (d in unique(results[[dimension]])) {
          
          df_filtered <- results |> 
            dplyr::filter(Series == s, !!rlang::sym(dimension) == d) |> 
            dplyr::select(GeoUID, year_month, Value) |> 
            tidyr::pivot_wider(
              names_from = year_month, 
              values_from = Value,
              names_prefix = paste0(s, "_", dimension, "_", d, "_")
            ) |> 
            dplyr::filter(!is.na(GeoUID))
          
          colnames(df_filtered) <- cmhc_clean_cma_names(colnames(df_filtered))
          colnames(df_filtered) <- cmhc_simplify_if_single_month(colnames(df_filtered))
          
          list_name <- cmhc_clean_cma_names(paste(s, dimension, d, sep = "_"))
          
          if (nrow(df_filtered) > 0) {
            results_list[[list_name]] <- df_filtered
          }
        }
      } else {
        df_filtered <- results |> 
          dplyr::filter(Series == s) |> 
          dplyr::select(GeoUID, year_month, Value) |> 
          tidyr::pivot_wider(
            names_from = year_month, 
            values_from = Value,
            names_prefix = paste0(s, "_")
          ) |> 
          dplyr::filter(!is.na(GeoUID))
        
        colnames(df_filtered) <- cmhc_clean_cma_names(colnames(df_filtered))
        colnames(df_filtered) <- cmhc_simplify_if_single_month(colnames(df_filtered))
        
        list_name <- cmhc_clean_cma_names(s)
        
        if (nrow(df_filtered) > 0) {
          results_list[[list_name]] <- df_filtered
        }
      }
    }
  }
  
  return(results_list)
}


#' Apply CMHC Data Abbreviations to Columns and Table Names
#'
#' This function replaces long column names and table names with standardized abbreviations
#' using the predefined named list `cmhc_data_abbreviations`.
#'
#' @param cmhc_cma_vectors A list containing CMHC data frames.
#' @return A modified list of data frames with standardized names.
cmhc_abbreviations <- function(cmhc_cma_vectors) {
  if (exists("cmhc_data_abbreviations") && is.list(cmhc_data_abbreviations)) {
    
    # Convert list to named vector for string replacement
    abbreviation_vector <- unlist(cmhc_data_abbreviations)
    names(abbreviation_vector) <- names(cmhc_data_abbreviations)
    
    # Apply abbreviations to column names
    cmhc_cma_vectors$CMA <- lapply(cmhc_cma_vectors$CMA, function(df) {
      if (is.data.frame(df)) {
        colnames(df) <- stringr::str_replace_all(colnames(df), abbreviation_vector)
      }
      return(df)
    })
    
    # Apply abbreviations to table names
    names(cmhc_cma_vectors$CMA) <- stringr::str_replace_all(names(cmhc_cma_vectors$CMA), abbreviation_vector)
  }
  
  return(cmhc_cma_vectors)
}

#' Simplify column names by removing month if all are annual (same MM)
#'
#' @param col_names Character vector of column names.
#' @return Vector with simplified names (_YYYYMM → _YYYY if all MM identical).
cmhc_simplify_if_single_month <- function(col_names) {
  # Extraire tous les suffixes YYYYMM dans les noms de colonnes
  ym_matches <- stringr::str_extract(col_names, "\\d{6}")
  ym_matches <- ym_matches[!is.na(ym_matches)]
  
  months <- substr(ym_matches, 5, 6)
  
  # Si tous les mois sont les mêmes, enlever MM des noms
  if (length(months) > 0 && length(unique(months)) == 1) {
    # Remplacer tous les _YYYYMM par _YYYY
    col_names <- stringr::str_replace_all(col_names, "_(\\d{4})(\\d{2})", "_\\1")
  }
  
  return(col_names)
}


#' Get CMHC Data for CMA
#'
#' Fetches, processes, and formats CMHC  data for CMAs.
#' This function loops through predefined requests, retrieves data, processes it,
#' reshapes it, and applies abbreviation transformations.
#'
#' @param requests A list of requests, each specifying a CMHC survey, series, dimension, and breakdown.
#' @param cma_all A data frame containing all CMAs with geographical identifiers.
#' @return A structured list containing processed CMHC data with applied abbreviations.
#' @export
cmhc_get_cma <- function(requests, cma_all) {
  cmhc_vectors <- list(CMA = list())
  results_list <- list()
  
  for (geo_uid in unique(cma_all$geouid)) {
    for (req in requests) {
      survey <- req$survey
      series <- req$series
      dimension <- req$dimension
      breakdown <- req$breakdown
      
      if (breakdown == "Historical Time Periods") {
        results <- cmhc_fetch_cma_data(survey, series, dimension, breakdown, geo_uid)
        results <- cmhc_process_cma(results)
        reshaped_data <- cmhc_reshape_cma_results(results, dimension)
        
        for (key in names(reshaped_data)) {
          if (is.null(results_list[[key]])) {
            results_list[[key]] <- reshaped_data[[key]]
          } else {
            results_list[[key]] <- dplyr::bind_rows(results_list[[key]], reshaped_data[[key]])
          }
        }
      }
    }
  }
  
  cmhc_vectors$CMA <- results_list
  
  # Apply abbreviations to column and table names
  cmhc_vectors <- cmhc_abbreviations(cmhc_vectors)
  
  cmhc_vectors$CMA <- lapply(cmhc_vectors$CMA, function(df) {
    if (is.data.frame(df) && "geouid" %in% colnames(df)) {
      df <- df |> dplyr::rename(id = geouid)
    }
    return(df)
  })
  
  return(cmhc_vectors)
}

#' @title Generate Correspondence Tables for CT(s) Between Two Census Periods (e.g., 1996 and 2021)
#' @description Generate flat correspondence tables for CTs of any selected CMA(s) or all CMAs.
#' @param census_years Character vector. Census years to compare with the reference year (default CA21).
#' @param cma_codes Character vector. CMA GeoUID(s) to process. If NULL, processes all CMAs.
#' @param reference_year Character. The reference census year (default "CA21").
#' @return A flat named list of correspondence tables.
#' @export
cmhc_ct_correspondences <- function(census_years = c("CA1996", "CA01", "CA06", "CA11", "CA16"),
                                    cma_codes = NULL,
                                    reference_year = "CA21") {
  
  generate_correspondence_table <- function(year_1, year_2, cma_code) {
    # Load CT geometries for both census years
    ct_1 <- cancensus::get_census(year_1, regions = list(CMA = cma_code), geo_format = "sf", level = "CT") |> 
      dplyr::rename(geouid = GeoUID)
    
    ct_2 <- cancensus::get_census(year_2, regions = list(CMA = cma_code), geo_format = "sf", level = "CT") |> 
      dplyr::rename(geouid = GeoUID)
    
    # Calculate areas for each CT
    ct_1 <- ct_1 |> dplyr::mutate(area_1 = units::drop_units(sf::st_area(ct_1)))
    ct_2 <- ct_2 |> dplyr::mutate(area_2 = units::drop_units(sf::st_area(ct_2)))
    
    # Rename geouid in second dataset
    names(ct_2)[names(ct_2) == "geouid"] <- "geouid_old"
    
    # Find intersections between CTs of both years
    inter <- sf::st_intersection(ct_1, ct_2[c("geouid_old", "area_2")])
    if (nrow(inter) == 0) return(NULL)
    
    # Calculate proportion of intersected areas
    inter <- inter |> 
      dplyr::mutate(
        intersected_area = units::drop_units(sf::st_area(inter)),
        proportion_area_1 = intersected_area / area_1,
        proportion_area_2 = intersected_area / area_2
      )
    
    # Keep matches where both proportions are at least 90%
    correspondence_table <- inter |> 
      dplyr::filter(proportion_area_1 >= 0.9 & proportion_area_2 >= 0.9) |> 
      dplyr::select(geouid, geouid_old) |> 
      dplyr::distinct() |> 
      dplyr::mutate(status = ifelse(geouid == geouid_old, "stable", "changed"))
    
    # Extract the last two digits of census years for column naming
    year_ref_num <- substr(gsub("CA", "", year_1), nchar(gsub("CA", "", year_1)) - 1, nchar(gsub("CA", "", year_1)))
    year_cmp_num <- substr(gsub("CA", "", year_2), nchar(gsub("CA", "", year_2)) - 1, nchar(gsub("CA", "", year_2)))
    
    # Rename columns based on census years
    correspondence_table <- correspondence_table |>
      dplyr::rename(
        !!paste0("geouid_", year_ref_num) := geouid,
        !!paste0("geouid_", year_cmp_num) := geouid_old
      ) |> 
      dplyr::select(
        paste0("geouid_", year_ref_num), 
        paste0("geouid_", year_cmp_num), 
        "status"
      )
    
    return(correspondence_table)
  }
  
  # If no CMA codes provided, retrieve all CMAs for the reference year
  if (is.null(cma_codes)) {
    cma_codes <- cancensus::list_census_regions(reference_year) |>
      dplyr::filter(level == "CMA") |> 
      dplyr::pull(region)
  }
  
  result <- list()
  
  for (cma_code in cma_codes) {
    for (year in census_years) {
      table <- generate_correspondence_table(reference_year, year, cma_code)
      if (!is.null(table)) {
        # Generate full year labels for naming
        year_ref_label <- ifelse(nchar(gsub("CA", "", reference_year)) == 2,
                                 paste0("20", gsub("CA", "", reference_year)),
                                 gsub("CA", "", reference_year))
        year_cmp_label <- ifelse(nchar(gsub("CA", "", year)) == 2,
                                 paste0("20", gsub("CA", "", year)),
                                 gsub("CA", "", year))
        
        # Create the name for the correspondence table
        name <- paste0("correspondence_", year_ref_label, "_", year_cmp_label)
        result[[name]] <- table
      }
    }
  }
  
  return(result)
}

#' CMHC CT Data load
#' 
#' @title Fetch CMHC CT Data
#' @description Retrieves CMHC data for a specified census tract (CT) and year, optionally for a specific month.
#'
#' @param survey Character. The CMHC survey identifier (e.g., "Scss", "Msss").
#' @param series Character. The series within the survey (e.g., "Starts", "Vacancy Rates").
#' @param dimension Character. The dimension of the data (e.g., "Dwelling Type", "Bedroom Type").
#' @param breakdown Character. The geographic breakdown (e.g., "Census Tracts").
#' @param geo_uid Character. The GeoUID of the census tract or area.
#' @param year Integer. The year of the data to fetch.
#' @param month Integer, optional. The month of the data to fetch. 
#'
#' @return A tibble with the requested CMHC data if successful, or NULL if the query fails.
cmhc_fetch_ct_data <- function(survey, series, dimension, breakdown, geo_uid, year, month = NULL) {
  tryCatch({
    if (!is.null(month)) {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = breakdown,
        geo_uid = geo_uid,
        year = year,
        month = sprintf("%02d", month)
      )
    } else {
      cmhc::get_cmhc(
        survey = survey,
        series = series,
        dimension = dimension,
        breakdown = breakdown,
        geo_uid = geo_uid,
        year = year
      )
    }
  }, error = function(e) {
    return(NULL)
  })
}


#' Apply Census Tract Correspondence
#'
#' Applies the appropriate correspondence table to adjust GeoUIDs from historical census geography to the 2021 geography.
#'
#' @param data A data frame containing CMHC data with a Census geography column and a GeoUID column.
#' @param ct_correspondence_list A named list of correspondence tables linking older GeoUIDs to 2021 GeoUIDs.
#' @return A data frame where GeoUIDs have been updated to the 2021 census geography, or an error if correspondence cannot be applied.
apply_ct_correspondence <- function(data, ct_correspondence_list) {
  if (!is.null(data) && "Census geography" %in% colnames(data)) {
    census_geo <- unique(data$`Census geography`)
    
    if (length(census_geo) != 1) {
      stop("ERROR: Census geography contains multiple values")
    }
    
    corr_table_name <- paste0("correspondence_2021_", census_geo)
    
    if (corr_table_name %in% names(ct_correspondence_list)) {
      correspondence_table <- ct_correspondence_list[[corr_table_name]]
      
      if (nrow(correspondence_table) == 0) {
        stop("ERROR: The correspondence table is empty")
      }
      
      old_geouid_col <- setdiff(names(correspondence_table)[grepl("^geouid_\\d+$", names(correspondence_table))], "geouid_21")
      
      if (length(old_geouid_col) == 1) {
        data <- data |>
          dplyr::rename(!!rlang::sym(old_geouid_col) := GeoUID) |>
          dplyr::left_join(correspondence_table, by = old_geouid_col) |>
          dplyr::select(geouid_21, geometry, dplyr::everything()) |>
          dplyr::rename(GeoUID = geouid_21) |>
          dplyr::select(-dplyr::one_of(old_geouid_col))
      } else {
        stop("ERROR: Unable to find a valid geouid_XX column")
      }
    } else {
      stop("ERROR: Correspondence table not found")
    }
  } else {
    stop("ERROR: Census geography column missing from CMHC data")
  }
  
  return(data)
}

#' Process CMHC Census Tract Data
#'
#' Processes raw CMHC data for census tracts by reshaping it and organizing it into a structured list format.
#'
#' @param results A data frame containing raw CMHC data for census tracts.
#' @param cmhc_vectors A list where the processed census tract data will be stored.
#' @param dimension The name of the column to use for creating subcategories (e.g., dwelling type, bedroom type).
#' @return A list of data frames with census tract data organized by series and dimension.
cmhc_process_ct <- function(results, cmhc_vectors, dimension) {
  if (!is.null(results) && nrow(results) > 0) {
    if ("DateString" %in% colnames(results)) {
      results <- results |>
        dplyr::rename(year_month = DateString)
    } else if ("Year" %in% colnames(results) & "Month" %in% colnames(results)) {
      results <- results |>
        dplyr::mutate(year_month = paste0(Year, sprintf("%02d", as.numeric(Month))))
    } else if ("Year" %in% colnames(results)) {
      results <- results |>
        dplyr::rename(year_month = Year)
    } else {
      stop("ERROR: Unable to generate year_month correctly")
    }
    
    for (s in unique(results$Series)) {
      for (d in unique(results[[dimension]])) {
        df_filtered <- results |>
          dplyr::filter(Series == s, !!rlang::sym(dimension) == d) |>
          dplyr::select(GeoUID, year_month, Value) |>
          tidyr::pivot_wider(
            names_from = year_month, 
            values_from = Value,
            names_prefix = paste0(s, "_", d, "_"),
            values_fn = dplyr::first
          ) |>
          dplyr::filter(!is.na(GeoUID))  
        
        df_filtered <- df_filtered |>
          dplyr::left_join(results |> dplyr::select(GeoUID) |> dplyr::distinct(), by = "GeoUID") |>
          dplyr::distinct(GeoUID, .keep_all = TRUE)
        
        colnames(df_filtered) <- colnames(df_filtered) |>
          stringr::str_to_lower() |>
          stringr::str_replace_all(" ", "_") |>
          stringr::str_replace_all("\\+", "plus")
        
        list_name <- stringr::str_to_lower(stringr::str_replace_all(paste(s, d, sep = "_"), "[^a-zA-Z0-9 ]", "_")) |>
          stringr::str_replace_all(" ", "_")
        
        if (nrow(df_filtered) > 0) {
          cmhc_vectors$CT[[list_name]] <- df_filtered  
        }
      }
    }
  }
  
  return(cmhc_vectors)
}

#' Retrieve and Process CMHC Census Tract Data
#'
#' Retrieves CMHC data for census tracts across multiple CMAs and processes the results into structured tables.
#'
#' @param requests A list of request objects, each containing survey, series, dimension, breakdown, years, and optional months.
#' @param cma_all A data frame containing CMAs with a \code{geouid} column specifying the geographic identifiers.
#' @param ct_correspondence_list A named list of correspondence tables linking older GeoUIDs to 2021 GeoUIDs.
#' @return A list containing processed census tract data for each series and dimension combination.
#' @export

cmhc_get_ct <- function(requests, cma_all, ct_correspondence_list) {
  cmhc_vectors <- list(CT = list())  # Initialize the final list
  
  for (cma in cma_all$geouid) {
    print(paste("Processing CMA:", cma))  
    
    for (req in requests) {
      survey <- req$survey
      series <- req$series
      dimension <- req$dimension
      breakdown <- req$breakdown
      years <- req$years
      months <- req$months  
      
      if (breakdown == "Census Tracts" & !is.null(years)) {
        
        results_list <- list()  # Initialize an empty list for each CMA
        
        for (year in years) {
          if (!is.null(months)) {
            for (month in months) {
              print(paste("Fetching data for:", cma, "year:", year, "month:", month))
              
              tryCatch({
                data <- cmhc_fetch_ct_data(
                  survey = survey,
                  series = series,
                  dimension = dimension,
                  breakdown = breakdown,
                  geo_uid = cma,
                  year = year,
                  month = month
                )
                
                if (!is.null(data) && nrow(data) > 0) {
                  print("Data successfully retrieved.")
                  data <- apply_ct_correspondence(data, ct_correspondence_list)
                  results_list <- append(results_list, list(data))
                } else {
                  print("No data retrieved for this GeoUID.")
                }
              }, error = function(e) {
                print(paste("Error during data retrieval:", e$message))
              })
            }
          } else {
            print(paste("Fetching data for:", cma, "year:", year))
            
            tryCatch({
              data <- cmhc_fetch_ct_data(
                survey = survey,
                series = series,
                dimension = dimension,
                breakdown = breakdown,
                geo_uid = cma,
                year = year
              )
              
              if (!is.null(data) && nrow(data) > 0) {
                print("Data successfully retrieved.")
                data <- apply_ct_correspondence(data, ct_correspondence_list)
                results_list <- append(results_list, list(data))
              } else {
                print("No data retrieved for this GeoUID.")
              }
            }, error = function(e) {
              print(paste("Error during data retrieval:", e$message))
            })
          }
        }
        
        # Merge results and store in cmhc_vectors
        if (length(results_list) > 0) {
          results <- dplyr::bind_rows(results_list)
          cmhc_vectors <- cmhc_process_ct(results, cmhc_vectors, dimension)
        }
      }
    }
  }
  
  # Rename "geouid" to "id" in all elements of cmhc_vectors$CT
  cmhc_vectors$CT <- lapply(cmhc_vectors$CT, function(df) {
    if ("geouid" %in% names(df)) {
      names(df)[names(df) == "geouid"] <- "id"
    }
    return(df)
  })
  
  return(cmhc_vectors)
}
