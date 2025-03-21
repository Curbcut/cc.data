#' Process CANSIM data and merge with census ID and geometries 
#'
#' This function retrieves Maintenance and repair expenditures data from a specified CANSIM table, processes it 
#' by reshaping the dataset into a wide format, and merges it with provincial geometries.
#'
#' @param table_id The CANSIM table ID to retrieve. Default is `"34-10-0095-01"`.
#' @return A spatial dataframe (`sf`) containing expenditure data joined with provincial geometries.
#' @export
process_maintenance_exp <- function(table_id) {
  
  options(scipen=999) 
  
  # Retrieves data from the CANSIM database and normalizes it for further processing.
  maintenance_df <- cansim::get_cansim_connection(table_id) |> 
    cansim::collect_and_normalize()
  
  # Filters out Canada-level data, keeping only province-level observations.
  maintenance_df <- dplyr::filter(maintenance_df, GEO != "Canada")
  
  # Converts the dataset into a pivoted format where each expenditure type and date become column names.
  maintenance_pivot <- maintenance_df |>
    dplyr::select(REF_DATE, GEO, `Type of expenditure`, val_norm) |>
    tidyr::pivot_wider(names_from = c(`Type of expenditure`, REF_DATE), 
                       values_from = val_norm, names_sep = "_")
  
  # Replaces spaces with underscores and renames expenditure types into shorter, meaningful names.
  maintenance_pivot <- maintenance_pivot |>
    dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) |>
    dplyr::rename_with(~ gsub("Total_expenditures", "total_exp", .x, ignore.case = TRUE)) |>
    dplyr::rename_with(~ gsub("Owner_occupied_expenditures", "owner_exp", .x, ignore.case = TRUE)) |>
    dplyr::rename_with(~ gsub("Landlord_and_tenant_occupied_expenditures", "tenant_exp", .x, ignore.case = TRUE))
  
  # Loads information about census regions and extracts only province-level data.
  census_regions <- cancensus::list_census_regions("CA21")
  province_list <- dplyr::filter(census_regions, level == "PR") |>
    dplyr::select(region, name)
  
  # Retrieves spatial information (geometries) for each province from the census.
  provinces_sf <- cancensus::get_census(dataset = "CA21", 
                                        regions = list(PR = province_list$region), 
                                        geo_format = "sf", 
                                        level = "PR") |>
    dplyr::select(name, geometry, GeoUID) 
  
  # Matches the province names in the CANSIM dataset with the names in the census dataset.
  province_mapping <- c(
    "Newfoundland and Labrador" = "Newfoundland and Labrador (N.L.)",
    "Prince Edward Island" = "Prince Edward Island (P.E.I.)",
    "Nova Scotia" = "Nova Scotia (N.S.)",
    "New Brunswick" = "New Brunswick (N.B.)",
    "Quebec" = "Quebec (Que.)",
    "Ontario" = "Ontario (Ont.)",
    "Manitoba" = "Manitoba (Man.)",
    "Saskatchewan" = "Saskatchewan (Sask.)",
    "Alberta" = "Alberta (Alta.)",
    "British Columbia" = "British Columbia (B.C.)",
    "Yukon" = "Yukon (Y.T.)",
    "Northwest Territories" = "Northwest Territories (N.W.T.)",
    "Nunavut" = "Nunavut (Nvt.)"
  )
  
  # Updates province names in the expenditure dataset to match the census dataset.
  maintenance_pivot <- maintenance_pivot |>
    dplyr::mutate(GEO = dplyr::recode(GEO, !!!province_mapping)) |>
    dplyr::rename(name = GEO)
  
  # Joins the expenditure data with the spatial census data based on province names.
  results <- dplyr::left_join(provinces_sf, maintenance_pivot, by = "name")
  
  # - Removes province abbreviations in parentheses to keep only the full province name.
  # - Renames "GeoUID" to lowercase "geouid" for consistency.
  results <- results |>
    dplyr::mutate(name = gsub("\\s*\\(.*?\\)", "", name)) |>
    dplyr::rename(geouid = GeoUID)
  
  return(results)
}



#' @title Process Wage Rate Index Data from CANSIM
#' @description This function retrieves wage rate index data from a specified CANSIM table, processes it, 
#' and returns two cleaned datasets: one for basic construction union wage rates and another 
#' for construction union wage rates including selected pay supplements.
#'
#' @param table_id A string specifying the CANSIM table ID (default: "18-10-0140-01").
#' @return A list containing two data frames:
#'   \item{constr_basic}{Basic construction union wage rate indexes}
#'   \item{constr_union_composite}{Construction union wage rate indexes including selected pay supplements}
#' @export
process_wages_tab <- function(table_id) {
  
  # Internal function: reformats date column names (YYYY-MM → YYYYMM)
  format_date_names <- function(x) {
    stringr::str_replace_all(x, "-", "")  # Remove "-" to get YYYYMM format
  }
  
  # Load wage rate data from CANSIM
  construction <- cansim::get_cansim_connection(table_id) |> 
    cansim::collect_and_normalize()
  
  # Process "Basic construction union wage rate indexes"
  constr_basic <- construction |>
    dplyr::filter(
      `Type of wage indexes` == "Basic construction union wage rate indexes",
      `Construction trades` == "Composite",
      GEO != "Canada"
    ) |>
    dplyr::select(ref_date = REF_DATE, name = GEO, value = VALUE) |>
    tidyr::pivot_wider(
      names_from = ref_date, 
      values_from = value, 
      names_prefix = "wages_basic_"
    ) |>
    dplyr::rename_with(format_date_names, dplyr::starts_with("wages_basic_"))  # Rename columns to YYYYMM format
  
  # Process "Construction union wage rate indexes including selected pay supplements"
  constr_union_composite <- construction |>
    dplyr::filter(
      `Type of wage indexes` == "Construction union wage rate indexes including selected pay supplements",
      `Construction trades` == "Composite",
      GEO != "Canada"
    ) |>
    dplyr::select(ref_date = REF_DATE, name = GEO, value = VALUE) |>
    tidyr::pivot_wider(
      names_from = ref_date, 
      values_from = value, 
      names_prefix = "wages_pay_sup_"
    ) |>
    dplyr::rename_with(format_date_names, dplyr::starts_with("wages_pay_sup_"))  # Rename columns to YYYYMM format
  
  # Retrieve the list of CMAs
  census_regions <- cancensus::list_census_regions("CA21")
  
  # Filter to include only CMA-level regions
  cma_list <- dplyr::filter(census_regions, level == "CMA") |> 
    dplyr::select(region, name)
  
  # Load census spatial data for CMAs
  cma_all <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CMA = cma_list$region), 
                                   geo_format = "sf", 
                                   level = "CMA") |>    
    dplyr::select(name, geometry, GeoUID) 
  
  # Standardize CMA names to ensure consistency
  geo_mapping <- c(
    "St. John's, Newfoundland and Labrador" = "St. John's (B)",
    "Halifax, Nova Scotia" = "Halifax (B)",
    "Saint John, New Brunswick" = "Saint John (B)",
    "Moncton, New Brunswick" = "Moncton (B)",
    "Québec, Quebec" = "Québec (B)",
    "Saguenay, Quebec" = "Saguenay (B)",
    "Montréal, Quebec" = "Montréal (B)",
    "Ottawa-Gatineau, Ontario part, Ontario/Quebec" = "Ottawa - Gatineau (B)",
    "Toronto, Ontario" = "Toronto (B)",
    "Hamilton, Ontario" = "Hamilton (B)",
    "St. Catharines-Niagara, Ontario" = "St. Catharines - Niagara (B)",
    "Kitchener-Cambridge-Waterloo, Ontario" = "Kitchener - Cambridge - Waterloo (B)",
    "London, Ontario" = "London (B)",
    "Windsor, Ontario" = "Windsor (B)",
    "Greater Sudbury, Ontario" = "Greater Sudbury / Grand Sudbury (B)",
    "Thunder Bay, Ontario" = "Thunder Bay (B)",
    "Oshawa, Ontario" = "Oshawa (B)",
    "Winnipeg, Manitoba" = "Winnipeg (B)",
    "Regina, Saskatchewan" = "Regina (B)",
    "Saskatoon, Saskatchewan" = "Saskatoon (B)",
    "Calgary, Alberta" = "Calgary (B)",
    "Edmonton, Alberta" = "Edmonton (B)",
    "Vancouver, British Columbia" = "Vancouver (B)",
    "Victoria, British Columbia" = "Victoria (B)",
    "Kelowna, British Columbia" = "Kelowna (B)"
  )
  
  # Apply name standardization
  constr_basic$name <- dplyr::recode(constr_basic$name, !!!geo_mapping)
  constr_union_composite$name <- dplyr::recode(constr_union_composite$name, !!!geo_mapping)
  
  # Merge wage rate data with geographic CMA information
  constr_basic <- cma_all |>
    dplyr::left_join(constr_basic, by = "name")
  
  constr_union_composite <- cma_all |>
    dplyr::left_join(constr_union_composite, by = "name")
  
  # Clean names by removing any content in parentheses
  constr_basic <- constr_basic |>
    dplyr::mutate(name = stringr::str_replace_all(name, "\\s*\\(.*?\\)", "")) |>
    dplyr::rename(geouid = GeoUID)
  
  constr_union_composite <- constr_union_composite |>
    dplyr::mutate(name = stringr::str_replace_all(name, "\\s*\\(.*?\\)", "")) |>
    dplyr::rename(geouid = GeoUID)
  
  # Return the processed data as a named list
  return(list(
    constr_basic = constr_basic,
    constr_union_composite = constr_union_composite
  ))
}


#' @title Process Construction Price Data from CANSIM
#' @description This function retrieves construction price data from a specified CANSIM table, processes it, 
#' and returns three cleaned datasets: one for apartment buildings, one for residential buildings, 
#' and another for non-residential buildings. It also merges the data with geographic information.
#'
#' @param table_id A string specifying the CANSIM table ID (default: "18-10-0276-01").
#' @return A list containing three data frames:
#'   \item{constr_price_apt}{Construction price data for apartment buildings}
#'   \item{constr_price_resi}{Construction price data for residential buildings}
#'   \item{constr_price_non_resi}{Construction price data for non-residential buildings}
#' @export
process_constr_price <- function(table_id) {
  
  # Load construction price data from CANSIM
  constr_price <- cansim::get_cansim_connection(table_id) |> 
    cansim::collect_and_normalize()
  
  # Retrieve the list of CMAs
  census_regions <- cancensus::list_census_regions("CA21")
  cma_list <- census_regions |> dplyr::filter(level == "CMA") |> dplyr::select(region, name)
  
  # Load census spatial data for CMAs
  cma_all <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CMA = cma_list$region), 
                                   geo_format = "sf", 
                                   level = "CMA") |>    
    dplyr::select(name, geometry, GeoUID)
  
  # Standardize city names for consistency
  geo_mapping <- c(
    "St. John's, Newfoundland and Labrador" = "St. John's (B)",
    "Halifax, Nova Scotia" = "Halifax (B)",
    "Moncton, New Brunswick" = "Moncton (B)",
    "Montréal, Quebec" = "Montréal (B)",
    "Ottawa-Gatineau, Ontario part, Ontario/Quebec" = "Ottawa - Gatineau (B)",
    "Toronto, Ontario" = "Toronto (B)",
    "Winnipeg, Manitoba" = "Winnipeg (B)",
    "Saskatoon, Saskatchewan" = "Saskatoon (B)",
    "Calgary, Alberta" = "Calgary (B)",
    "Edmonton, Alberta" = "Edmonton (B)",
    "Vancouver, British Columbia" = "Vancouver (B)"
  )
  
  # Function to filter and reshape data for a specific building type
  process_building_type <- function(building_type, prefix) {
    constr_price |>
      dplyr::filter(
        `Type of building` == building_type,
        Division == "Division composite",
        GEO != "Eleven census metropolitan area composite"
      ) |>
      dplyr::select(ref_date = REF_DATE, name = GEO, value = val_norm) |>
      tidyr::pivot_wider(
        names_from = ref_date, 
        values_from = value, 
        names_prefix = prefix
      ) |>
      dplyr::mutate(name = dplyr::recode(name, !!!geo_mapping)) |> # Apply city name mapping
      dplyr::left_join(cma_all, by = "name") |>  # Merge with census data
      dplyr::relocate(name, GeoUID)  # Move 'name' and 'GeoUID' to the beginning
  }
  
  # Process different building types
  constr_price_apt <- process_building_type("Apartment buildings", "constr_price_apt_")
  constr_price_resi <- process_building_type("Residential buildings", "constr_price_resi_")
  constr_price_non_resi <- process_building_type("Non-residential buildings", "constr_price_non_resi_")
  
  # Function to rename columns in YYYYQX format (e.g., 201701 -> 2017Q1)
  rename_quarterly <- function(df) {
    new_names <- names(df)
    new_names <- stringr::str_replace_all(new_names, "-", "")  # Remove dashes to get YYYYMM
    new_names <- stringr::str_replace(new_names, "(\\d{4})01$", "\\1q1")
    new_names <- stringr::str_replace(new_names, "(\\d{4})04$", "\\1q2")
    new_names <- stringr::str_replace(new_names, "(\\d{4})07$", "\\1q3")
    new_names <- stringr::str_replace(new_names, "(\\d{4})10$", "\\1q4")
    names(df) <- new_names
    return(df)
  }
  
  # Apply the renaming function at the very end
  constr_price_apt <- rename_quarterly(constr_price_apt)
  constr_price_resi <- rename_quarterly(constr_price_resi)
  constr_price_non_resi <- rename_quarterly(constr_price_non_resi)
  
  constr_price_apt <- constr_price_apt |>
    dplyr::rename(geouid = GeoUID)
  
  constr_price_resi <- constr_price_resi |>
    dplyr::rename(geouid = GeoUID)
  
  constr_price_non_resi <- constr_price_non_resi |>
    dplyr::rename(geouid = GeoUID)
  
  # Return the three tables as a list
  return(list(
    constr_price_apt = constr_price_apt,
    constr_price_resi = constr_price_resi,
    constr_price_non_resi = constr_price_non_resi
  ))
}

#' @title Process Unemployment Data from CANSIM
#' @description This function retrieves unemployment rate data from a specified CANSIM table, processes it, 
#' and merges it with census province-level geometries. The function standardizes province names, 
#' reshapes the data into a wide format, and applies geographic identifiers for spatial analysis.
#'
#' @param table_id A string specifying the CANSIM table ID (default: "14-10-0287-03"), which contains unemployment rate data.
#' @return A spatial data frame (`sf`) containing unemployment data merged with province geometries.
#' 
#' @export
process_unemployment_tab <- function(table_id) {
  
  # Function to reformat column names from YYYY-MM to YYYYMM
  format_date_names <- function(x) {
    stringr::str_replace_all(x, "-", "")  # Remove hyphens to get YYYYMM format
  }
  
  # Load unemployment rate data from CANSIM
  unemp <- cansim::get_cansim_connection(table_id) |> 
    cansim::collect_and_normalize() |>
    dplyr::filter(
      `Labour force characteristics` == "Unemployment rate",
      Gender == "Total - Gender",
      `Age group` == "15 years and over",
      Statistics == "Estimate",
      `Data type` == "Seasonally adjusted",
      GEO != "Canada"
    )
  
  # Reshape data into a wide format with unemployment rates as columns
  unemp_year <- unemp |>
    dplyr::select(ref_date = REF_DATE, name = GEO, value = VALUE) |>
    tidyr::pivot_wider(
      names_from = ref_date, 
      values_from = value, 
      names_prefix = "unemp_"
    ) |>
    dplyr::rename_with(format_date_names, dplyr::starts_with("unemp_")) 
  
  # Retrieve census region metadata
  census_regions <- cancensus::list_census_regions("CA21")
  province_list <- dplyr::filter(census_regions, level == "PR") |> dplyr::select(region, name)
  
  # Load province-level geometries for spatial analysis
  provinces_sf <- cancensus::get_census(dataset = "CA21", 
                                        regions = list(PR = province_list$region), 
                                        geo_format = "sf", 
                                        level = "PR") |>    
    dplyr::select(name, geometry, GeoUID) 
  
  # Standardize province names for consistency with census data
  province_mapping <- c(
    "Newfoundland and Labrador" = "Newfoundland and Labrador (N.L.)",
    "Prince Edward Island" = "Prince Edward Island (P.E.I.)",
    "Nova Scotia" = "Nova Scotia (N.S.)",
    "New Brunswick" = "New Brunswick (N.B.)",
    "Quebec" = "Quebec (Que.)",
    "Ontario" = "Ontario (Ont.)",
    "Manitoba" = "Manitoba (Man.)",
    "Saskatchewan" = "Saskatchewan (Sask.)",
    "Alberta" = "Alberta (Alta.)",
    "British Columbia" = "British Columbia (B.C.)",
    "Yukon" = "Yukon (Y.T.)",
    "Northwest Territories" = "Northwest Territories (N.W.T.)",
    "Nunavut" = "Nunavut (Nvt.)"
  )
  
  # Apply name mapping to unemployment data
  unemp_year <- unemp_year |>
    dplyr::mutate(name = dplyr::recode(name, !!!province_mapping))
  
  # Merge unemployment data with census geographic information
  results <- dplyr::left_join(provinces_sf, unemp_year, by = "name")
  
  # Remove province abbreviations in parentheses to match standardized naming
  results <- results |>
    dplyr::mutate(name = gsub("\\s*\\(.*?\\)", "", name)) |>
    dplyr::rename(geouid = GeoUID)
  
  return(results)
}

#' @title Process Elasticities Data and Merge with Census Metropolitan Areas
#'
#' @description This function retrieves census metropolitan area (CMA) spatial data, 
#' downloads elasticity data from a provided URL, standardizes the names, and merges both datasets.
#' The resulting dataset includes geographic and elasticity values for each CMA.
#'
#' @param file_url The URL of the elasticity Excel file.
#' @return A spatial data frame (`sf`) containing census metropolitan areas merged with elasticity values.
#' 
#' @export
process_elasticities_tab <- function(file_url) {
  
  # Download the Excel file to a temporary file
  temp_file <- tempfile(fileext = ".xls")
  download.file(file_url, temp_file, mode = "wb")
  
  # Load census metropolitan area (CMA) regions
  census_regions <- cancensus::list_census_regions("CA21")
  cma_list <- census_regions |> dplyr::filter(level == "CMA") |> dplyr::select(region, name)
  
  # Retrieve spatial census data for CMAs
  cma_all <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CMA = cma_list$region), 
                                   geo_format = "sf", 
                                   level = "CMA") |>    
    dplyr::select(name, geometry, GeoUID) 
  
  # Standardize CMA names by removing parentheses and extra content
  cma_all <- cma_all |> dplyr::mutate(name = gsub("\\s*\\(.*?\\)", "", name))
  
  # Load elasticity data from the temporary Excel file
  elasticities <- readxl::read_excel(temp_file)
  
  # Filter relevant CMAs and select necessary columns
  elasticities <- elasticities |>
    dplyr::filter(cma == 1) |>
    dplyr::select(city, prov, elasticity, `Below Median`, `Below Average`) 
  
  # Rename "city" column to "name" for consistency
  elasticities <- elasticities |> dplyr::rename(name = city)
  
  # Remove 'prov' column as it is not needed
  elasticities$prov <- NULL
  
  # Standardize CMA names for consistency with census data
  name_mapping <- c(
    "Greater Sudbury/Grand Sudbury" = "Greater Sudbury / Grand Sudbury",
    "Kitchener-Cambridge-Waterloo" = "Kitchener - Cambridge - Waterloo",
    "Ottawa-Gatineau" = "Ottawa - Gatineau",
    "St. Catharines-Niagara" = "St. Catharines - Niagara",
    "Trois-Rivieres" = "Trois-Rivières",
    "Quebec" = "Québec",
    "Saint John" = "Saint John",
    "Montreal" = "Montréal",
    "St. John's" = "St. John's",
    "Abbotsford-Mission" = "Abbotsford - Mission"
  )
  
  # Apply name mapping to standardize names
  elasticities <- elasticities |> dplyr::mutate(name = dplyr::recode(name, !!!name_mapping))
  
  # Merge elasticity data with spatial census data
  final_data <- dplyr::right_join(cma_all, elasticities, by = "name")
  
  # Rename columns to lowercase and consistent format
  final_data <- final_data |>
    dplyr::rename(
      geouid = GeoUID,
      below_median = `Below Median`,
      below_average = `Below Average`
    )
  
  # Remove the temporary file
  unlink(temp_file)
  
  return(final_data)
}


#' @title Process Bank of Canada Interest Rate Data
#'
#' @description This function downloads and processes Bank of Canada interest rate data 
#' from a provided JSON URL. It extracts, reformats, and reshapes the data into a wide format, 
#' keeping only observations from 1991 onwards.
#'
#' @param file_url The URL of the JSON file containing interest rate data.
#' @return A formatted data frame with interest rates from 1991 onwards, structured in a wide format.
#' 
#' @export
process_interest_rates <- function(file_url) {
  
  # Download the JSON file to a temporary file
  temp_file <- tempfile(fileext = ".json")
  download.file(file_url, temp_file, mode = "wb")
  
  # Load JSON data from the temporary file
  json_data <- jsonlite::fromJSON(temp_file)
  
  # Extract and reformat interest rate data
  interest_rates <- json_data$observations |> 
    dplyr::mutate(
      bank_rate = as.numeric(V122530$v),  # Convert interest rate column to numeric
      bank_rate_yearmois = gsub("-", "", substr(d, 1, 7))  # Transform date format to YYYYMM
    ) |> 
    dplyr::filter(as.numeric(substr(d, 1, 4)) >= 1991) |>  # Keep only data from 1991 onwards
    dplyr::select(bank_rate_yearmois, bank_rate) |>  # Select relevant columns
    tidyr::pivot_wider(names_from = bank_rate_yearmois, values_from = bank_rate, 
                       names_prefix = "bank_rate_") |>  # Convert to wide format
    dplyr::mutate(name = "Canada", geouid = "01") |>  # Add static columns
    dplyr::select(name, geouid, dplyr::everything())  # Reorder columns
  
  # Remove the temporary file
  unlink(temp_file)
  
  return(interest_rates)
}