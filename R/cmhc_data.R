#' @title Split Ottawa-Gatineau CMA by Province
#' @description This function retrieves census metropolitan areas (CMAs) and provinces for a given census year,
#' identifies Ottawa-Gatineau (B), and splits it into two separate CMAs: one for Ontario and one for Quebec.
#' The function assigns appropriate names and GeoUIDs to the split regions.
#'
#' @param census_year Character. The census year (e.g., "CA21").
#' @return A spatial dataframe (sf object) containing CMAs with Ottawa-Gatineau split by province.
#' @export
split_ottawa_gatineau_cma <- function(census_year = "CA21") {
  
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

# Run function
cma_all <- split_ottawa_gatineau_cma("CA21")



#' Fetch CMHC Data
#'
#' Retrieves data from the CMHC API for CMA level.
#'
#' @param survey The CMHC survey to query.
#' @param series The data series to retrieve.
#' @param dimension The dimension to filter by (if applicable).
#' @param breakdown The breakdown level for the data.
#' @param geo_uid The geographical identifier.
#' @return A data frame with CMHC data or `NULL` if an error occurs.
#' @export
fetch_cmhc_data <- function(survey, series, dimension, breakdown, geo_uid) {
  tryCatch(
    cmhc::get_cmhc(
      survey = survey, 
      series = series, 
      dimension = dimension, 
      breakdown = breakdown, 
      geo_uid = geo_uid,
      refresh = TRUE
    ),
    error = function(e) {
      return(NULL)
    }
  )
}

#' Process CMHC Results
#'
#' Cleans and formats the retrieved CMHC data.
#' Converts `GeoUID` to character, extracts year and month from `DateString`, 
#' and renames columns accordingly.
#'
#' @param results A data frame containing CMHC data.
#' @return A cleaned and processed data frame.
#' @export
process_results <- function(results) {
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
#' @param name_vector A vector of column names.
#' @return A vector of cleaned column names.
#' @export
clean_names <- function(name_vector) {
  name_vector |> 
    stringr::str_trim() |>  
    stringr::str_to_lower() |>  
    stringr::str_replace_all("[^a-z0-9_ ]", "_") |>  
    stringr::str_replace_all("\\s+", "_") |>  
    stringr::str_replace_all("_+", "_") |>  
    stringr::str_replace_all("_$", "")  
}

#' Reshape CMHC Results
#'
#' Transforms the data into a structured format by filtering, pivoting, 
#' and joining it with a reference dataset.
#'
#' @param results The CMHC data frame to reshape.
#' @param cma_all A reference dataset with geographical identifiers.
#' @param dimension The column name used as a filter for restructuring.
#' @return A list of reshaped data frames, each corresponding to a unique series.
#' @export
reshape_results <- function(results, cma_all, dimension) {
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
            ) |> dplyr::filter(!is.na(GeoUID))
          
          colnames(df_filtered) <- clean_names(colnames(df_filtered))
          list_name <- clean_names(paste(s, dimension, d, sep = "_"))
          
          df_final <- df_filtered |> 
            dplyr::left_join(cma_all |> dplyr::mutate(geouid = as.character(geouid)), by = "geouid") |>  
            dplyr::mutate(name = stringr::str_replace_all(name, " \\(B\\)$", "")) |>  
            dplyr::relocate(name, .before = dplyr::everything())
          
          if (!is.null(df_final) && nrow(df_final) > 0) {
            results_list[[list_name]] <- df_final
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
          ) |> dplyr::filter(!is.na(GeoUID))
        
        colnames(df_filtered) <- clean_names(colnames(df_filtered))
        list_name <- clean_names(s)
        
        df_final <- df_filtered |> 
          dplyr::left_join(cma_all |> dplyr::mutate(geouid = as.character(geouid)), by = "geouid") |>  
          dplyr::mutate(name = stringr::str_replace_all(name, " \\(B\\)$", "")) |>  
          dplyr::relocate(name, .before = dplyr::everything())
        
        if (!is.null(df_final) && nrow(df_final) > 0) {
          results_list[[list_name]] <- df_final
        }
      }
    }
  }
  return(results_list)
}


#' Get CMHC Data for CMA
#'
#' Fetches and processes CMHC housing data for different Census Metropolitan Areas (CMA).
#' This function loops through a set of predefined requests, retrieves the corresponding 
#' data using the `fetch_cmhc_data` function, processes it, and reshapes it for analysis.
#'
#' @param requests A list of requests, each specifying a CMHC survey, series, dimension, and breakdown.
#' @param cma_all A data frame containing all CMAs with geographical identifiers.
#' @return A list containing processed CMHC data for each series and dimension.
#' @export
get_cmhc_cma <- function(requests, cma_all) {
  cmhc_vectors <- list(CMA = list())
  results_list <- list()
  
  for (geo_uid in unique(cma_all$geouid)) {
    for (req in requests) {
      survey <- req$survey
      series <- req$series
      dimension <- req$dimension
      breakdown <- req$breakdown
      
      if (breakdown == "Historical Time Periods") {
        results <- fetch_cmhc_data(survey, series, dimension, breakdown, geo_uid)
        results <- process_results(results)
        reshaped_data <- reshape_results(results, cma_all, dimension)
        
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
  return(cmhc_vectors)
}

#' List of CMHC Data Requests for CMA
#'
#' Defines a structured list of CMHC data requests for different surveys, series, 
#' and dimensions. These requests are used in `get_cmhc_cma()` to retrieve and process
#' the corresponding housing market data.
#'
#' @return A list of CMHC data requests, including survey type, series name, 
#' dimension for grouping, and breakdown type.
requests_cma <- list(
  list(
    survey = "Scss", 
    series = "Starts", 
    dimension = "Dwelling Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Starts", 
    dimension = "Intended Market",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Starts (SAAR)", 
    dimension = NULL,
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Completions", 
    dimension = "Dwelling Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Completions", 
    dimension = "Intended Market",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Under Construction", 
    dimension = "Dwelling Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Under Construction", 
    dimension = "Intended Market",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Length of Construction", 
    dimension = "Dwelling Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Length of Construction", 
    dimension = "Intended Market",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Scss", 
    series = "Share absorbed at completion", 
    dimension = "Dwelling Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Vacancy Rate", 
    dimension = "Bedroom Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Vacancy Rate", 
    dimension = "Year of Construction",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Vacancy Rate", 
    dimension = "Structure Size",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Vacancy Rate", 
    dimension = "Rent Ranges",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Average Rent", 
    dimension = "Bedroom Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Average Rent", 
    dimension = "Year of Construction",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Average Rent", 
    dimension = "Structure Size",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Median Rent", 
    dimension = "Bedroom Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Median Rent", 
    dimension = "Year of Construction",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Median Rent", 
    dimension = "Structure Size",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Rental Universe", 
    dimension = "Bedroom Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Rental Universe", 
    dimension = "Year of Construction",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Rms", 
    series = "Rental Universe", 
    dimension = "Structure Size",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Seniors", 
    series = "Rental Housing Vacancy Rates", 
    dimension = "Unit Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Seniors", 
    series = "Spaces", 
    dimension = "Unit Type",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Seniors", 
    series = "Universe and Number of Residents", 
    dimension = "Spaces and Residents",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Seniors", 
    series = "Heavy Care Spaces", 
    dimension = "Vacancy Rate and Average Rent",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Seniors", 
    series = "Proportion of Standard Spaces", 
    dimension = "Rent Range",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Core Housing Need", 
    series = "Housing Standards", 
    dimension = "% of Households in Core Housing Need",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Core Housing Need", 
    series = "Housing Standards", 
    dimension = "Households in Core Housing Need",
    breakdown = "Historical Time Periods"),
  list(
    survey = "Core Housing Need", 
    series = "Housing Standards", 
    dimension = "Households Tested For Core Housing Need",
    breakdown = "Historical Time Periods")
)

#' Execute CMHC Data Retrieval
#'
#' Runs the `get_cmhc_cma` function using the predefined `requests_cma` list 
#' to fetch CMHC data for different Census Metropolitan Areas.
#'
#' @return A structured list of CMHC housing market data.

bench::mark(
  cmhc_cma_vectors <- get_cmhc_cma(requests_cma, cma_all),
  min_iterations = 1
)

#' use table of cmhc_data_abbreviations for replacing column names and table names.
#' This ensures consistency and shortens long variable names.
#' 
#' Apply cmhc_data_abbreviations to Column and Table Names
#'
#' This function replaces column names in a dataset based on a predefined set of cmhc_data_abbreviations.
#'
#' @param df A data frame where column names should be transformed.
#' @param cmhc_data_abbreviations A tibble containing original names and corresponding cmhc_data_abbreviations.
#' @return The dataframe with transformed column names.
#' @export
apply_cmhc_data_abbreviations <- function(df, cmhc_data_abbreviations) {
  if (is.data.frame(df)) {
    new_colnames <- colnames(df) |> stringr::str_to_lower()
    
    for (i in seq_along(cmhc_data_abbreviations$original)) {
      new_colnames <- stringr::str_replace_all(new_colnames, stringr::fixed(cmhc_data_abbreviations$original[i]), cmhc_data_abbreviations$abbreviation[i])
    }
    
    colnames(df) <- new_colnames
    return(df)
  }
  return(df)
}

#' Apply cmhc_data_abbreviations to CMHC Data Columns
#'
#' This applies the `apply_cmhc_data_abbreviations` function to all datasets in `cmhc_cma_vectors$CMA`,
#' updating column names based on the predefined abbreviation table.
#'
#' @return The modified list of data frames with updated column names.
cmhc_cma_vectors$CMA <- lapply(cmhc_cma_vectors$CMA, function(df) apply_cmhc_data_abbreviations(df, cmhc_data_abbreviations))

#' Apply cmhc_data_abbreviations to Table Names
#'
#' This renames all table names in `cmhc_cma_vectors$CMA` using the abbreviation rules.
#'
#' @return The modified list of dataset names with cmhc_data_abbreviations applied.
names(cmhc_cma_vectors$CMA) <- names(cmhc_cma_vectors$CMA) |> 
  stringr::str_to_lower() |> 
  purrr::map_chr(~{
    name <- .x
    for (i in seq_along(cmhc_data_abbreviations$original)) {
      name <- stringr::str_replace_all(name, stringr::fixed(cmhc_data_abbreviations$original[i]), cmhc_data_abbreviations$abbreviation[i])
    }
    name
  })

  usethis::use_data(cmhc_cma_vectors, overwrite = TRUE)



