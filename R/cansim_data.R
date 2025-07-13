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

#' Download and process IRCC Excel table on Admissions of Permanent Residents
#'
#' Downloads and processes the IRCC Excel file containing annual admission data for Permanent Residents
#' by Census Metropolitan Area (CMA), then links it to 2021 CMA GeoUIDs using the `cancensus` package.
#'
#' The source includes annual counts from 2006 to 2016 (partial for 2016).
#'
#' @return A tibble with one row per CMA, containing annual values and the CMA GeoUID as `id`.
#' @export
ircc_pr_data <- function() {
  # Step 1: Download Excel file
  url <- "https://www.ircc.canada.ca/opendata-donneesouvertes/data/IRCC_PRadmiss_0015_E.xls"
  temp_file <- tempfile(fileext = ".xls")
  
  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))
  if (response$status_code != 200) {
    stop("Download failed: HTTP ", response$status_code)
  }
  
  # Step 2: Read and clean raw data
  raw_data <- readxl::read_excel(temp_file, col_names = FALSE)
  
  cleaned_data <- raw_data |>
    dplyr::filter(!is.na(...2)) |>
    dplyr::select(...2, ...4:...14) |>
    rlang::set_names(c("destination", as.character(2006:2015), "2016")) |>
    dplyr::mutate(
      dplyr::across(-destination, ~ as.numeric(dplyr::if_else(.x %in% c("--", NA), "0", .x))),
      destination = destination |>
        stringr::str_remove("Total$") |>
        stringr::str_trim(),
      join_key = destination
    )
  
  # Step 3: Get CMA 2021 GeoUIDs with matching join_key
  cma_lookup <- cancensus::get_census("CA21", list(C = "01"), level = "CMA",
                                      use_cache = FALSE, geo_format = NA, quiet = TRUE) |>
    dplyr::filter(!stringr::str_detect(`Region Name`, "\\(partie d")) |>
    dplyr::mutate(
      join_key = stringr::str_split_fixed(
        stringr::str_remove(`Region Name`, " \\([A-Z]\\)$"),
        " / ",
        2
      )[, 1]
    ) |>
    dplyr::select(id = GeoUID, join_key)
  
  # Step 4: Merge and finalize table
  output <- cleaned_data |>
    dplyr::left_join(cma_lookup, by = "join_key") |>
    dplyr::filter(!is.na(id)) |>
    dplyr::select(id, dplyr::starts_with("20")) |>
    dplyr::rename_with(~ paste0("pr_admission_", .x), .cols = -id)
  
  return(output)
}

#' Process IRCC monthly study permit data at the province/territory level
#'
#' Downloads and processes the IRCC Excel file containing monthly totals of study permit holders,
#' and joins with 2021 provincial GeoUIDs from the Census.
#'
#' @return A tibble with columns: `id` (GeoUID), monthly and annual study permit counts,
#'         renamed as `international_students_<yearmonth>` or `<year>`.
#' @export
ircc_international_students_data <- function() {
  
  # Step 1: Download and read the file
  url <- "https://www.ircc.canada.ca/opendata-donneesouvertes/data/IRCC_M_TRStudy_0007_E.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))
  raw <- readxl::read_excel(temp_file, col_names = FALSE)
  
  # Step 2: Clean province names
  cleaned <- raw |>
    dplyr::filter(!is.na(...1)) |>
    dplyr::rename(id = ...1) |>
    dplyr::select(-...2) |>
    dplyr::mutate(
      id = stringr::str_remove(id, "Total$"),
      id = stringr::str_trim(id),
      dplyr::across(dplyr::starts_with("..."), ~ as.numeric(dplyr::na_if(., "--")))
    )
  
  # Step 3: Get GeoUIDs for provinces
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    use_cache = FALSE,
    geo_format = NA,
    quiet = TRUE
  ) |>
    dplyr::select(GeoUID, region_name_census = `Region Name`) |>
    dplyr::mutate(id = stringr::str_remove(region_name_census, " \\(.*\\)$"))
  
  # Step 4: Join with GeoUIDs
  joined <- dplyr::left_join(cleaned, provinces, by = "id") |>
    dplyr::filter(!is.na(GeoUID)) |>
    dplyr::select(GeoUID, dplyr::everything(), -region_name_census)
  
  # Step 5: Build column names and select final structure
  col_start <- 2  # after GeoUID
  cols_to_keep <- 1 # GeoUID column
  new_names <- "id"
  
  for (year in 2015:2021) {
    month_indices <- c(
      col_start + 1, col_start + 2, col_start + 3,
      col_start + 5, col_start + 6, col_start + 7,
      col_start + 9, col_start + 10, col_start + 11,
      col_start + 13, col_start + 14, col_start + 15
    )
    year_total_index <- col_start + 17
    
    cols_to_keep <- c(cols_to_keep, month_indices, year_total_index)
    new_names <- c(
      new_names,
      paste0("international_students_", year, sprintf("%02d", 1:12)),
      paste0("international_students_", year)
    )
    col_start <- col_start + 17
  }
  
  # Step 6: Output
  output <- joined |>
    dplyr::select(dplyr::all_of(cols_to_keep)) |>
    rlang::set_names(new_names)
  
  return(output)
}

#' Process IRCC monthly work permit data at the province/territory level
#'
#' Downloads and processes the IRCC CSV file containing monthly totals of work permit holders,
#' and joins with 2021 provincial GeoUIDs from the Census.
#'
#' @return A tibble with columns: `id` (GeoUID), monthly and annual totals renamed
#'         as `imp_admission_<yearmonth>` or `imp_admission_<year>`.
#' @export
ircc_imp_admission_data <- function() {

  # Step 1: Read the CSV from the web
  url <- "https://www.ircc.canada.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv"
  raw <- readr::read_tsv(url, show_col_types = FALSE)

  # Step 2: Clean and aggregate
  monthly_totals <- raw |>
    janitor::clean_names() |>
    dplyr::select(
      year = en_year,
      month = en_month,
      province = en_province_territory,
      total = total
    ) |>
    dplyr::mutate(total = readr::parse_number(total, na = "--")) |>
    dplyr::group_by(province, year, month) |>
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

  # Step 3: Pivot to wide format for monthly + yearly
  monthly_wide <- monthly_totals |>
    dplyr::mutate(
      month_num = dplyr::case_when(
        month == "Jan" ~ 1, month == "Feb" ~ 2, month == "Mar" ~ 3,
        month == "Apr" ~ 4, month == "May" ~ 5, month == "Jun" ~ 6,
        month == "Jul" ~ 7, month == "Aug" ~ 8, month == "Sep" ~ 9,
        month == "Oct" ~ 10, month == "Nov" ~ 11, month == "Dec" ~ 12
      ),
      date_col = sprintf("%d%02d", year, month_num)
    ) |>
    dplyr::select(province, date_col, total) |>
    tidyr::pivot_wider(names_from = date_col, values_from = total)

  annual_wide <- monthly_totals |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = year, values_from = total)

  combined <- dplyr::full_join(monthly_wide, annual_wide, by = "province")

  # Step 4: Join with 2021 provincial GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    use_cache = FALSE,
    geo_format = NA,
    quiet = TRUE
  ) |>
    dplyr::select(GeoUID, region_name_census = `Region Name`) |>
    dplyr::mutate(province = stringr::str_remove(region_name_census, " \\(.*\\)$"))

  joined <- dplyr::left_join(combined, provinces, by = "province") |>
    dplyr::filter(!is.na(GeoUID)) |>
    dplyr::select(GeoUID, dplyr::everything(), -province, -region_name_census)

  # Step 5: Rename columns with "imp_admission_YYYYMM" and "imp_admission_YYYY"
  output <- joined |>
    dplyr::rename(id = GeoUID) |>
    dplyr::rename_with(
      .fn = ~ paste0("imp_admission_", .x),
      .cols = dplyr::matches("^\\d{6}$|^\\d{4}$")
    )

  return(output)
}

#' Process IRCC monthly TFWP work permit data at the province/territory level
#'
#' Downloads and processes the IRCC Excel file containing monthly totals of TFWP work permit holders,
#' and joins with 2021 provincial GeoUIDs from the Census.
#'
#' @return A tibble with columns: `id` (GeoUID), monthly permit counts (e.g., 201501), and yearly totals (e.g., 2015)
#' @export
ircc_admission_tfwp_data <- function() {
  # Étape 1 : Télécharger et lire le fichier Excel
  url <- "https://www.ircc.canada.ca/opendata-donneesouvertes/data/EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))
  raw <- readxl::read_excel(temp_file, col_names = FALSE)
  
  # Étape 2 : Nettoyage de base
  cleaned <- raw |>
    dplyr::filter(!is.na(...1)) |>
    dplyr::rename(Destination = ...1) |>
    dplyr::select(-...2) |>
    dplyr::mutate(Destination = stringr::str_remove(Destination, "Total$"),
                  Destination = stringr::str_trim(Destination)) |>
    dplyr::filter(
      !Destination %in% c("Canada - Temporary Foreign Worker Program (TFWP) work permit holders by province/territoire of intended destination, program and year in which permit(s) became effective, January 2015 - March 2025",
                          "Province/territory and program", "Total", "Province/territory not stated") &
        !stringr::str_starts(Destination, "Notes:") &
        !stringr::str_starts(Destination, "For further") &
        !stringr::str_starts(Destination, "Source:")
    )
  
  # Étape 3 : Sélection et renommage des colonnes utiles
  indices_a_garder <- c(1)
  nouveaux_noms <- c("Destination")
  col_depart_annee <- 2
  
  for (annee in 2015:2024) {
    indices_mois <- c(
      col_depart_annee, col_depart_annee + 1, col_depart_annee + 2,
      col_depart_annee + 4, col_depart_annee + 5, col_depart_annee + 6,
      col_depart_annee + 8, col_depart_annee + 9, col_depart_annee + 10,
      col_depart_annee + 12, col_depart_annee + 13, col_depart_annee + 14
    )
    indice_total_annuel <- col_depart_annee + 16
    indices_a_garder <- c(indices_a_garder, indices_mois, indice_total_annuel)
    noms_mois <- paste0(annee, sprintf("%02d", 1:12))
    nouveaux_noms <- c(nouveaux_noms, noms_mois, as.character(annee))
    col_depart_annee <- col_depart_annee + 17
  }
  
  # Étape 4 : Appliquer sélection et noms
  tableau <- cleaned |>
    dplyr::select(dplyr::all_of(indices_a_garder)) |>
    rlang::set_names(nouveaux_noms) |>
    dplyr::mutate(across(.cols = -Destination, .fns = ~ as.numeric(dplyr::na_if(., "--"))))
  
  # Étape 5 : Ajout des GeoUIDs
  provinces <- cancensus::get_census(
    dataset = 'CA21',
    regions = list(C = "01"),
    level = 'PR',
    use_cache = FALSE,
    geo_format = NA,
    quiet = TRUE
  ) |>
    dplyr::select(GeoUID, region_name_census = `Region Name`) |>
    dplyr::mutate(Destination = stringr::str_remove(region_name_census, " \\(.*\\)$"))
  
  joined <- dplyr::left_join(tableau, provinces, by = "Destination") |>
    dplyr::filter(!is.na(GeoUID)) |>
    dplyr::select(GeoUID, Destination, dplyr::everything(), -region_name_census)
  
  # Étape 6 : Réorganisation finale
  noms <- names(joined)
  mois <- sort(noms[grepl("^\\d{6}$", noms)])
  annees <- sort(noms[grepl("^\\d{4}$", noms)])
  final <- joined |>
    dplyr::rename(id = GeoUID, province = Destination) |>
    dplyr::select(id, dplyr::all_of(mois), dplyr::all_of(annees))
  
  return(final)
}
