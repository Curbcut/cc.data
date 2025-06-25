#' @title Retrieve and Reshape Housing Maintenance Expenditures
#' @description Downloads and processes CANSIM data on housing maintenance and repair expenditures by province. 
#' The function reshapes the data into wide format and returns three data frames: total, owner-occupied, and tenant-occupied expenditures.
#'
#' @param table_id CANSIM table ID to download. Default is `"34-10-0095-01"`.
#' @return A named list of three `data.frame`s: `housing_maint_exp_total`, `housing_maint_exp_owner`, `housing_maint_exp_tenant`.
#' Each data frame contains one row per province (`id`) and one column per year of expenditure.
#' @export
cansim_maintenance_exp <- function() {
  
  # Step 1: Retrieve and normalize CANSIM data
  maintenance_df <- cansim::get_cansim_connection("34-10-0095-01") |>
    cansim::collect_and_normalize()
  
  # Step 2: Filter out Canada-level aggregates
  maintenance_df <- dplyr::filter(maintenance_df, GEO != "Canada")
  
  # Step 3: Correct GeoUID for known mismatches
  maintenance_df <- dplyr::mutate(
    maintenance_df,
    GeoUID = dplyr::case_when(
      GEO == "Newfoundland and Labrador" ~ "10",
      GEO == "British Columbia" ~ "59",
      GEO == "Northwest Territories" ~ "61",
      GEO == "Yukon" ~ "60",
      GEO == "Nunavut" ~ "62",
      TRUE ~ GeoUID
    )
  )
  
  # Step 4: Reshape into wide format (pivot)
  maintenance_pivot <- maintenance_df |>
    dplyr::select(REF_DATE, id = GeoUID, `Type of expenditure`, val_norm) |>
    tidyr::pivot_wider(
      names_from = c(`Type of expenditure`, REF_DATE),
      values_from = val_norm,
      names_sep = "_"
    ) |>
    dplyr::filter(!is.na(id)) # Remove rows with missing IDs
  
  # Step 5: Clean column names
  maintenance_pivot <- maintenance_pivot |>
    dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) |>
    dplyr::rename_with(~ gsub("Total_expenditures", "housing_maint_exp_total", .x, ignore.case = TRUE)) |>
    dplyr::rename_with(~ gsub("Owner_occupied_expenditures", "housing_maint_exp_owner", .x, ignore.case = TRUE)) |>
    dplyr::rename_with(~ gsub("Landlord_and_tenant_occupied_expenditures", "housing_maint_exp_tenant", .x, ignore.case = TRUE))
  
  # Step 6: Return three separate data frames
  return(list(
    housing_maint_exp_total  = dplyr::select(maintenance_pivot, id, dplyr::starts_with("housing_maint_exp_total_")),
    housing_maint_exp_owner  = dplyr::select(maintenance_pivot, id, dplyr::starts_with("housing_maint_exp_owner_")),
    housing_maint_exp_tenant = dplyr::select(maintenance_pivot, id, dplyr::starts_with("housing_maint_exp_tenant_"))
  ))
}

#' @title Process Wage Rate Index Data from CANSIM
#' @description Retrieves and processes CANSIM wage rate index data, returning two datasets: 
#' one for basic construction union wage rates and one for those including selected pay supplements.
#'
#' @param table_id A string specifying the CANSIM table ID (default: "18-10-0140-01").
#' @return A list of two data frames:
#'   \item{union_wage_index_basic}{Basic union wage rate indexes by CMA}
#'   \item{union_wage_index_supp}{Union wage rate indexes with supplements by CMA}
#' @export
cansim_wages <- function() {
  
  clean_cma_names <- function(df) {
    df |>
      dplyr::mutate(name = stringr::str_replace_all(name, "\\s*\\(.*?\\)", "")) |>
      dplyr::rename(id = GeoUID) |>
      dplyr::select(-name, -geometry)
  }
  
  construction <- cansim::get_cansim_connection("18-10-0140-01") |> 
    cansim::collect_and_normalize()
  
  get_wage_data <- function(data, index_type, prefix) {
    data |>
      dplyr::filter(
        `Type of wage indexes` == index_type,
        `Construction trades` == "Composite",
        GEO != "Canada"
      ) |>
      dplyr::select(ref_date = REF_DATE, name = GEO, value = VALUE) |>
      tidyr::pivot_wider(
        names_from = ref_date, 
        values_from = value, 
        names_prefix = prefix
      ) |>
      dplyr::rename_with(format_date_names, dplyr::starts_with(prefix))
  }
  
  union_wage_index_basic <- get_wage_data(construction, 
                                          "Basic construction union wage rate indexes", 
                                          "union_wage_index_basic_")
  
  union_wage_index_supp <- get_wage_data(construction, 
                                         "Construction union wage rate indexes including selected pay supplements", 
                                         "union_wage_index_supp_")
  
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
  
  # List of CMAs
  cma_all <- cancensus::list_census_regions("CA21") |>
    dplyr::filter(level == "CMA") |>
    dplyr::select(region, name) |>
    {\(cma_list) cancensus::get_census(
      dataset = "CA21",
      regions = list(CMA = cma_list$region),
      geo_format = "sf",
      level = "CMA"
    )}() |>
    dplyr::select(name, geometry, GeoUID)
  
  union_wage_index_basic$name <- dplyr::recode(union_wage_index_basic$name, !!!geo_mapping)
  union_wage_index_supp$name  <- dplyr::recode(union_wage_index_supp$name, !!!geo_mapping)
  
  union_wage_index_basic <- cma_all |>
    dplyr::inner_join(union_wage_index_basic, by = "name") |>
    clean_cma_names() |>
    sf::st_drop_geometry()
  
  union_wage_index_supp <- cma_all |>
    dplyr::inner_join(union_wage_index_supp, by = "name") |>
    clean_cma_names() |>
    sf::st_drop_geometry()
  
  return(list(
    union_wage_index_basic = union_wage_index_basic,
    union_wage_index_supp = union_wage_index_supp
  ))
}


#' @title Process Construction Price Data from CANSIM
#' @description This function retrieves construction price data from a specified CANSIM table, processes it, 
#' and returns three cleaned datasets: one for apartment buildings, one for residential buildings, 
#' and another for non-residential buildings. It also merges the data with geographic information.
#'
#' @param table_id A string specifying the CANSIM table ID (default: "18-10-0276-01").
#' @return A list containing three data frames:
#'   \item{construction_price_apartment}{Construction price data for apartment buildings}
#'   \item{construction_price_residential}{Construction price data for residential buildings}
#'   \item{construction_price_nonres}{Construction price data for non-residential buildings}
#' @export
cansim_construction_price <- function() {
  
  constr_price <- cansim::get_cansim_connection("18-10-0276-01") |> 
    cansim::collect_and_normalize()
  
  census_regions <- cancensus::list_census_regions("CA21")
  cma_list <- dplyr::filter(census_regions, level == "CMA") |> 
    dplyr::select(region, name)
  
  # Get CMA data without geometry
  cma_all <- cancensus::get_census(
    dataset = "CA21", 
    regions = list(CMA = cma_list$region), 
    geo_format = "sf", 
    level = "CMA"
  ) |> 
    sf::st_drop_geometry() |>  # Drop geometry here
    dplyr::select(name, GeoUID)
  
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
      dplyr::mutate(name = dplyr::recode(name, !!!geo_mapping)) |>
      dplyr::inner_join(cma_all, by = "name") |>
      dplyr::rename(id = GeoUID)
  }
  
  rename_quarterly <- function(df) {
    names(df) <- names(df) |>
      stringr::str_replace_all("-", "") |>
      stringr::str_replace("(\\d{4})01$", "\\1q1") |>
      stringr::str_replace("(\\d{4})04$", "\\1q2") |>
      stringr::str_replace("(\\d{4})07$", "\\1q3") |>
      stringr::str_replace("(\\d{4})10$", "\\1q4")
    df
  }
  
  construction_price_apartment <- process_building_type("Apartment buildings", "construction_price_apartment_") |>
    rename_quarterly() |>
    dplyr::select(id, dplyr::everything(), -name)
  
  construction_price_residential <- process_building_type("Residential buildings", "construction_price_residential_") |>
    rename_quarterly() |>
    dplyr::select(id, dplyr::everything(), -name)
  
  construction_price_nonres <- process_building_type("Non-residential buildings", "construction_price_nonres_") |>
    rename_quarterly() |>
    dplyr::select(id, dplyr::everything(), -name)
  
  return(list(
    construction_price_apartment   = construction_price_apartment,
    construction_price_residential = construction_price_residential,
    construction_price_nonres      = construction_price_nonres
  ))
}

  #' Helper function: Reformat column names from "YYYY-MM" to "YYYYMM"
  format_date_names <- function(x) {
    stringr::str_replace_all(x, "-", "")  # Remove hyphens to get YYYYMM format
  }

#' @title Process Unemployment Data from CANSIM
#' @description Downloads and reshapes CANSIM table 14-10-0287-03 to get monthly unemployment rates
#'              for each geography (excluding Canada) in wide format (one column per month).
#' @return A data frame with one row per geographic unit and columns for unemployment rates by month (YYYYMM).
#' @export
cansim_unemployment <- function() {
  
  # Load unemployment rate data from CANSIM
  unemp <- cansim::get_cansim_connection("14-10-0287-03") |> 
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
  unemp_wide <- unemp |>
    dplyr::select(ref_date = REF_DATE, id = GeoUID, value = VALUE) |>
    tidyr::pivot_wider(
      names_from = ref_date, 
      values_from = value, 
      names_prefix = "unemployment_rate_"
    ) |>
    dplyr::rename_with(format_date_names, dplyr::starts_with("unemployment_rate_"))  
  
  return(unemp_wide)
}

#' @title Process and Merge Elasticity Data with CMA Identifiers
#' 
#' @description This function loads Census Metropolitan Area (CMA) data from Statistics Canada's 2021 census,
#' reads elasticity estimates from a local Excel file, standardizes CMA names for consistency, and merges
#' both datasets. The output is a clean table containing CMA identifiers and elasticity estimates only.
#' 
#' @param file_path A string specifying the full path to the local Excel file containing elasticity data.
#' 
#' @return A tibble with two columns: \code{id} (GeoUID of the CMA) and \code{elasticity_estimates}.
#' @export
boc_elasticities <- function(file_path) {
  
  # Step 1: Retrieve list of CMA regions
  cma_list <- cancensus::list_census_regions("CA21") |>
    dplyr::filter(level == "CMA") |>
    dplyr::select(region, name)
  
  # Step 2: Download spatial geometries for all CMAs
  cma_all <- cancensus::get_census(
    dataset = "CA21",
    regions = list(CMA = cma_list$region),
    geo_format = "sf",
    level = "CMA"
  ) |>
    dplyr::select(name, geometry, GeoUID) |>
    dplyr::mutate(name = gsub("\\s*\\(.*?\\)", "", name))  # Remove text in parentheses
  
  # Step 3: Read elasticity data from Excel
  elasticities <- readxl::read_excel(file_path) |>
    dplyr::filter(cma == 1) |>  # Keep rows flagged as CMA
    dplyr::select(city, elasticity) |>  # Only keep needed columns
    dplyr::rename(name = city)
  
  # Step 4: Standardize CMA names for consistency with census data
  name_mapping <- c(
    "Greater Sudbury/Grand Sudbury" = "Greater Sudbury / Grand Sudbury",
    "Kitchener-Cambridge-Waterloo" = "Kitchener - Cambridge - Waterloo",
    "Ottawa-Gatineau" = "Ottawa - Gatineau",
    "St. Catharines-Niagara" = "St. Catharines - Niagara",
    "Trois-Rivieres" = "Trois-Rivières",
    "Quebec" = "Québec",
    "Montreal" = "Montréal",
    "St. John's" = "St. John's",
    "Abbotsford-Mission" = "Abbotsford - Mission"
  )
  
  elasticities <- elasticities |>
    dplyr::mutate(name = dplyr::recode(name, !!!name_mapping))
  
  # Step 5: Merge and clean final dataset
  final_data <- dplyr::inner_join(cma_all, elasticities, by = "name") |>
    dplyr::rename(
      id = GeoUID,
      elasticity_estimates = elasticity) |>
      dplyr::select(id, elasticity_estimates) |>
    sf::st_drop_geometry()  # Drop spatial information
  
  return(final_data)
}

#' @title Process Bank of Canada Interest Rate Data
#'
#' @description This function downloads and processes Bank of Canada interest rate data 
#' from the official Valet API. It extracts, reformats, and reshapes the data into a wide format, 
#' keeping only observations from 1991 onwards.
#'
#' @return A formatted data frame with interest rates from 1991 onwards, structured in a wide format.
#' 
#' @export
boc_interest_rates <- function() {
  
  # Define the API URL with full date range
  api_url <- "https://www.bankofcanada.ca/valet/observations/V122530/json?start_date=1900-01-01"
  
  # Load JSON data from the temporary file
  json_data <- jsonlite::fromJSON(api_url)
  
  # Extract and reformat interest rate data
  interest_rates <- json_data$observations |> 
    dplyr::mutate(
      bank_rate = as.numeric(V122530$v),  # Convert interest rate column to numeric
      bank_rate_yearmois = gsub("-", "", substr(d, 1, 7))  # Transform date format to YYYYMM
    ) |> 
    dplyr::filter(as.numeric(substr(d, 1, 4)) >= 1991) |>  # Keep only data from 1991 onwards
    dplyr::select(bank_rate_yearmois, bank_rate) |>  # Select relevant columns
    tidyr::pivot_wider(names_from = bank_rate_yearmois, values_from = bank_rate, 
                       names_prefix = "interest_rate_") |>  # Convert to wide format
    dplyr::mutate(name = "Canada", geouid = "01") |>  # Add static columns
    dplyr::select(id=geouid, dplyr::everything())  # Reorder columns
  
  return(interest_rates)
}

#' Retrieve and reshape Canada-level monthly mortgage rate data
#'
#' Downloads and reshapes monthly mortgage rate data from Statistics Canada (CANSIM table 34-10-0145-01),
#' restricted to dates starting from January 1990. Returns a single-row data frame with one column per month.
#'
#' @return A data frame with one row (id = "01") and columns in the format `mortgage_rate_YYYYMM`
#' @export
cansim_mortgage_rates <- function() {
  
  # Step 1: Download and normalize CANSIM table
  tab <- cansim::get_cansim("34-10-0145-01") |>
    cansim::normalize_cansim_values()
  
  # Step 2: Filter from January 1990 onward and assign constant ID
  df <- tab |>
    dplyr::mutate(id = "01") |>
    dplyr::filter(REF_DATE >= "1990-01") |>
    dplyr::select(REF_DATE, id, val_norm)
  
  # Step 3: Reshape to wide format with one column per REF_DATE
  df_wide <- df |>
    tidyr::pivot_wider(
      names_from = REF_DATE,
      values_from = val_norm,
      names_prefix = "mortgage_rate_"
    ) |>
    # Step 4: Clean column names: remove dashes from YYYY-MM
    dplyr::rename_with(~ gsub("-", "", .x, fixed = TRUE))
  
  return(df_wide)
}

