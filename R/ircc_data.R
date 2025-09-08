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
#' Downloads and processes the IRCC CSV file containing monthly totals of work permit holders (International Mobility Program),
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
#' @return A tibble with columns: `id` (GeoUID), monthly and annual totals renamed
#'         as `tfwp_admission_<yearmonth>` or `tfwp_admission_<year>`.
#' @export
ircc_admission_tfwp_data <- function() {
  # Step 1: Download and read the Excel file
  url <- "https://www.ircc.canada.ca/opendata-donneesouvertes/data/EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))
  raw <- readxl::read_excel(temp_file, col_names = FALSE)
  
  # Step 2: Basic cleaning
  cleaned <- raw |>
    dplyr::filter(!is.na(...1)) |>
    dplyr::rename(Destination = ...1) |>
    dplyr::select(-...2) |>
    dplyr::mutate(
      Destination = Destination |> stringr::str_remove("Total$") |> stringr::str_trim()
    ) |>
    dplyr::filter(
      !Destination %in% c(
        "Canada - Temporary Foreign Worker Program (TFWP) work permit holders by province/territoire of intended destination, program and year in which permit(s) became effective, January 2015 - March 2025",
        "Province/territory and program", "Total", "Province/territory not stated"
      ),
      !stringr::str_starts(Destination, "Notes:"),
      !stringr::str_starts(Destination, "For further"),
      !stringr::str_starts(Destination, "Source:")
    )
  
  # Step 3: Column selection and renaming
  indices_to_keep <- c(1)
  new_names <- c("Destination")
  col_start <- 2
  
  for (year in 2015:2024) {
    month_indices <- c(
      col_start, col_start + 1, col_start + 2,
      col_start + 4, col_start + 5, col_start + 6,
      col_start + 8, col_start + 9, col_start + 10,
      col_start + 12, col_start + 13, col_start + 14
    )
    year_total_index <- col_start + 16
    indices_to_keep <- c(indices_to_keep, month_indices, year_total_index)
    month_names <- paste0(year, sprintf("%02d", 1:12))
    new_names <- c(new_names, month_names, as.character(year))
    col_start <- col_start + 17
  }
  
  # Step 4: Apply selection and convert data
  table <- cleaned |>
    dplyr::select(dplyr::all_of(indices_to_keep)) |>
    rlang::set_names(new_names) |>
    dplyr::mutate(across(-Destination, ~ as.numeric(dplyr::na_if(., "--"))))
  
  # Step 5: Join with GeoUIDs
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
  
  joined <- dplyr::left_join(table, provinces, by = "Destination") |>
    dplyr::filter(!is.na(GeoUID)) |>
    dplyr::select(GeoUID, dplyr::everything(), -Destination, -region_name_census)
  
  # Step 6: Rename columns and finalize output
  output <- joined |>
    dplyr::rename(id = GeoUID) |>
    dplyr::rename_with(
      .fn = ~ paste0("tfwp_admission_", .x),
      .cols = dplyr::matches("^\\d{6}$|^\\d{4}$")
    )
  
  return(output)
}