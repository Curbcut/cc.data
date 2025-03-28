#' CMHC Data load
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
cmhc_fetch_data <- function(survey, series, dimension, breakdown, geo_uid) {
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
cmhc_process_results <- function(results) {
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
cmhc_clean_names <- function(name_vector) {
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
cmhc_reshape_results <- function(results, dimension) {
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
          
          colnames(df_filtered) <- cmhc_clean_names(colnames(df_filtered))
          colnames(df_filtered) <- cmhc_simplify_if_single_month(colnames(df_filtered))
          
          list_name <- cmhc_clean_names(paste(s, dimension, d, sep = "_"))
          
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
        
        colnames(df_filtered) <- cmhc_clean_names(colnames(df_filtered))
        colnames(df_filtered) <- cmhc_simplify_if_single_month(colnames(df_filtered))
        
        list_name <- cmhc_clean_names(s)
        
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
        results <- cmhc_fetch_data(survey, series, dimension, breakdown, geo_uid)
        results <- cmhc_process_results(results)
        reshaped_data <- cmhc_reshape_results(results, dimension)
        
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

