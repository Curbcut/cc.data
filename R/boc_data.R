#' Download and reshape short-term inflation expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term inflation expectations), reshapes the table to wide format with one
#' row per province and one column per quarter, and adds the corresponding GeoUID
#' from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter named `inflation_ce_<period>`.
#' @export
boc_inflation_expectations <- function() {
  
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C1_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 120, col_names = TRUE)
  
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C1E_SHORT_TERM_"))
  
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C1E_SHORT_TERM_", "", id_raw),
      province = dplyr::case_when(
        id == "AB" ~ "Alberta",
        id == "BC" ~ "British Columbia",
        id == "MB" ~ "Manitoba",
        id == "ON" ~ "Ontario",
        id == "QC" ~ "Québec",
        id == "SK" ~ "Saskatchewan",
        id == "AT" ~ "Atlantic",
        TRUE ~ id
      )
    ) |>
    dplyr::select(province, date, value)
  
  df_atlantic <- df_long |>
    dplyr::filter(province == "Atlantic") |>
    dplyr::mutate(province = NULL) |>
    tidyr::crossing(province = c(
      "Nova Scotia", "New Brunswick",
      "Prince Edward Island", "Newfoundland and Labrador"
    ))
  
  df_clean <- dplyr::bind_rows(
    df_long |> dplyr::filter(province != "Atlantic"),
    df_atlantic
  )
  
  df_wide <- df_clean |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = value,
      names_prefix = "inflation_ce_"
    ) |>
    dplyr::arrange(province)
  
  names(df_wide) <- stringr::str_replace_all(names(df_wide), "(\\d{4})Q(\\d)", "\\1q\\2")
  
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, dplyr::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape short-term inflation uncertainty expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term inflation uncertainty), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "uncertainty_ce_2014q4", ...).
#' @export
boc_inflation_uncertainty_expectations <- function() {
  
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C2_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 198, col_names = TRUE)
  
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C2E_UNCERTAINTY_SHORT_TERM_"))
  
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(id = gsub("CES_C2E_UNCERTAINTY_SHORT_TERM_", "", id_raw))
  
  df_long <- df_long |>
    dplyr::mutate(
      province = dplyr::case_when(
        id == "AB" ~ "Alberta",
        id == "BC" ~ "British Columbia",
        id == "MB" ~ "Manitoba",
        id == "ON" ~ "Ontario",
        id == "QC" ~ "Québec",
        id == "SK" ~ "Saskatchewan",
        id == "AT" ~ "Atlantic",
        TRUE ~ id
      )
    ) |>
    dplyr::select(province, date, value)
  
  df_expanded <- df_long |>
    dplyr::filter(province == "Atlantic") |>
    dplyr::mutate(province = NULL) |>
    tidyr::crossing(province = c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))
  
  df_clean <- df_long |>
    dplyr::filter(province != "Atlantic") |>
    dplyr::bind_rows(df_expanded)
  
  df_wide <- df_clean |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = value,
      names_prefix = "inflation_uncertainty_ce_"
    ) |>
    dplyr::arrange(province)
  
  # Renommer les colonnes : uncertainty_ce_2014Q4 -> uncertainty_ce_2014q4
  names(df_wide) <- stringr::str_replace_all(names(df_wide), "(\\d{4})Q(\\d)", "\\1q\\2")
  
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, dplyr::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape selected CES_C8 components (e.g. RENT, FOOD)
#'
#' This function extracts selected components from CES_C8_DEMOGRAPHICS,
#' reshapes to wide format with one row per province and columns like
#' `inflation_food_ce_2015q1`, `inflation_rent_ce_2015q2`, etc.
#' Atlantic values are duplicated to NS, NB, PEI, NL.
#'
#' @param component_keys Character vector. List of components to extract (e.g. c("FOOD", "RENT"))
#'
#' @return A named list of data.frames. One per component with `id` and wide-format columns.
#' @export
boc_inflation_components_expectations <- function(component_keys) {
  
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C8_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 120, col_names = TRUE)
  
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province)) |>
    dplyr::select(id = GeoUID, province)
  
  df_long <- df |>
    tidyr::pivot_longer(cols = -date, names_to = "col", values_to = "value") |>
    dplyr::filter(grepl(paste0("CES_.*E_(", paste(component_keys, collapse = "|"), ")_"), col)) |>
    dplyr::mutate(
      component = tolower(sub("CES_.*E_([A-Z]+)_[A-Z]{2}", "\\1", col)),
      prov_code = sub(".*_([A-Z]{2})$", "\\1", col),
      province = dplyr::case_when(
        prov_code == "AB" ~ "Alberta",
        prov_code == "BC" ~ "British Columbia",
        prov_code == "MB" ~ "Manitoba",
        prov_code == "ON" ~ "Ontario",
        prov_code == "QC" ~ "Québec",
        prov_code == "SK" ~ "Saskatchewan",
        prov_code == "AT" ~ "Atlantic",
        TRUE ~ prov_code
      ),
      quarter = tolower(gsub("-", "q", date)),
      col_name = paste0("inflation_", component, "_ce_", quarter)
    ) |>
    dplyr::select(component, province, col_name, value)
  
  atl_expanded <- df_long |>
    dplyr::filter(province == "Atlantic") |>
    dplyr::mutate(province = NULL) |>
    tidyr::crossing(province = c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))
  
  df_clean <- df_long |>
    dplyr::filter(province != "Atlantic") |>
    dplyr::bind_rows(atl_expanded)
  
  df_split <- split(df_clean, df_clean$component)
  
  results <- lapply(df_split, function(df_sub) {
    df_wide <- df_sub |>
      dplyr::select(province, col_name, value) |>
      tidyr::pivot_wider(names_from = col_name, values_from = value) |>
      dplyr::left_join(provinces, by = "province") |>
      dplyr::select(id, dplyr::everything(), -province) |>
      dplyr::arrange(as.integer(id))
    return(df_wide)
  })
  
  return(results)
}

#' Download and reshape short-term interest rate expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term interest rate expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "interest_rate_ce_2014q4", ...).
#' @export
boc_get_interest_rate_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C6_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 94, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C6E_ONE_YEAR_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C6E_ONE_YEAR_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("interest_rate_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape short-term well-being expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term well-being expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "well_being_ce_2014q4", ...).
#' @export
boc_get_well_being_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C15_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 68, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C15E_FUTURE_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C15E_FUTURE_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("well_being_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  # Step 8: Final output
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape short-term well-being expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term well-being expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "access_credit_2014q4", ...).
#' @export
boc_get_access_credit_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C16_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 68, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C16E_FUTURE_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C16E_FUTURE_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("access_credit_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  # Step 8: Final output
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape short-term well-being expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term well-being expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "missing_payment_2014q4", ...).
#' @export
boc_get_missing_payment_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C14_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 42, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C14E_MISS_DEBT_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C14E_MISS_DEBT_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("missing_payment_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  # Step 8: Final output
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape CES_C5 components (e.g. INCOME, SPENDING)
#'
#' This function extracts selected components from CES_C5_DEMOGRAPHICS,
#' reshapes them to wide format with one row per province and columns like
#' `income_ce_2014q4`, `spending_ce_2015q1`, etc.
#' Atlantic values are duplicated to NS, NB, PEI, NL.
#'
#' @param component_keys Character vector. List of components to extract (e.g. c("INCOME", "SPENDING"))
#'
#' @return A named list of data.frames. One per component with `id` and wide-format columns.
#' @export
boc_income_spending_expectations <- function(component_keys) {
  
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C5_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 94, col_names = TRUE)
  
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province)) |>
    dplyr::select(id = GeoUID, province)
  
  pattern <- paste0("CES_C5E_(", paste(component_keys, collapse = "|"), ")_[A-Z]{2}")
  
  df_long <- df |>
    tidyr::pivot_longer(cols = -date, names_to = "col", values_to = "value") |>
    dplyr::filter(grepl(pattern, col)) |>
    dplyr::mutate(
      component = tolower(sub("CES_C5E_([A-Z]+)_[A-Z]{2}", "\\1", col)),
      prov_code = sub(".*_([A-Z]{2})$", "\\1", col),
      province = dplyr::case_when(
        prov_code == "AB" ~ "Alberta",
        prov_code == "BC" ~ "British Columbia",
        prov_code == "MB" ~ "Manitoba",
        prov_code == "ON" ~ "Ontario",
        prov_code == "QC" ~ "Québec",
        prov_code == "SK" ~ "Saskatchewan",
        prov_code == "AT" ~ "Atlantic",
        TRUE ~ prov_code
      ),
      quarter = tolower(gsub("-", "q", date)),
      col_name = paste0(component, "_ce_", quarter)
    ) |>
    dplyr::select(component, province, col_name, value)
  
  atl_expanded <- df_long |>
    dplyr::filter(province == "Atlantic") |>
    dplyr::mutate(province = NULL) |>
    tidyr::crossing(province = c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))
  
  df_clean <- df_long |>
    dplyr::filter(province != "Atlantic") |>
    dplyr::bind_rows(atl_expanded)
  
  df_split <- split(df_clean, df_clean$component)
  
  results <- lapply(df_split, function(df_sub) {
    df_wide <- df_sub |>
      dplyr::select(province, col_name, value) |>
      tidyr::pivot_wider(names_from = col_name, values_from = value) |>
      dplyr::left_join(provinces, by = "province") |>
      dplyr::select(id, dplyr::everything(), -province) |>
      dplyr::arrange(as.integer(id))
    return(df_wide)
  })
  
  return(results)
}

#' Download and reshape CES_C11 components (e.g. MOVING, SELLING)
#'
#' This function extracts selected components from CES_C11_DEMOGRAPHICS,
#' reshapes them to wide format with one row per province and columns like
#' `moving_ce_2014q4`, `selling_ce_2015q1`, etc.
#' Atlantic values are duplicated to NS, NB, PEI, NL.
#'
#' @param component_keys Character vector. List of components to extract (e.g. c("MOVING", "SELLING"))
#'
#' @return A named list of data.frames. One per component with `id` and wide-format columns.
#' @export
boc_housing_market_expectations <- function(component_keys) {
  
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C11_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 120, col_names = TRUE)
  
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province)) |>
    dplyr::select(id = GeoUID, province)
  
  pattern <- paste0("CES_C11E_(", paste(component_keys, collapse = "|"), ")_[A-Z]{2}")
  
  df_long <- df |>
    tidyr::pivot_longer(cols = -date, names_to = "col", values_to = "value") |>
    dplyr::filter(grepl(pattern, col)) |>
    dplyr::mutate(
      component = tolower(sub("CES_C11E_([A-Z]+)_[A-Z]{2}", "\\1", col)),
      prov_code = sub(".*_([A-Z]{2})$", "\\1", col),
      province = dplyr::case_when(
        prov_code == "AB" ~ "Alberta",
        prov_code == "BC" ~ "British Columbia",
        prov_code == "MB" ~ "Manitoba",
        prov_code == "ON" ~ "Ontario",
        prov_code == "QC" ~ "Québec",
        prov_code == "SK" ~ "Saskatchewan",
        prov_code == "AT" ~ "Atlantic",
        TRUE ~ prov_code
      ),
      quarter = tolower(gsub("-", "q", date)),  # e.g. 2014-Q4 → 2014q4
      col_name = paste0(component, "_ce_", quarter)
    ) |>
    dplyr::select(component, province, col_name, value)
  
  atl_expanded <- df_long |>
    dplyr::filter(province == "Atlantic") |>
    dplyr::mutate(province = NULL) |>
    tidyr::crossing(province = c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))
  
  df_clean <- df_long |>
    dplyr::filter(province != "Atlantic") |>
    dplyr::bind_rows(atl_expanded)
  
  df_split <- split(df_clean, df_clean$component)
  
  results <- lapply(df_split, function(df_sub) {
    df_wide <- df_sub |>
      dplyr::select(province, col_name, value) |>
      tidyr::pivot_wider(names_from = col_name, values_from = value) |>
      dplyr::left_join(provinces, by = "province") |>
      dplyr::select(id, dplyr::everything(), -province) |>
      dplyr::arrange(as.integer(id))
    return(df_wide)
  })
  
  return(results)
}
#' Download and reshape short-term well-being expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term well-being expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "house_price_2014q4", ...).
#' @export
boc_get_house_price_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C7/csv"
  df <- readr::read_csv(url, skip = 24, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C7_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C7_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("house_price_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  # Step 8: Final output
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

#' Download and reshape short-term well-being expectations by province
#'
#' This function downloads the Bank of Canada's Consumer Expectations Survey data
#' (short-term well-being expectations), filters relevant columns, reshapes the table
#' to wide format with one row per province and one column per quarter, and adds
#' the corresponding GeoUID from Statistics Canada's census geography.
#'
#' The value for the Atlantic region is duplicated across Nova Scotia, New Brunswick,
#' Prince Edward Island, and Newfoundland and Labrador.
#'
#' @return A data.frame with `id` (GeoUID) and one column per quarter (e.g., "income_2014q4", ...).
#' @export
boc_get_wage_expectations <- function() {
  
  # Step 1: Load Bank of Canada data
  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C3_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 68, col_names = TRUE)
  
  # Step 2: Keep only relevant columns
  df_filtered <- df |>
    dplyr::select(date, dplyr::starts_with("CES_C3E_NEXT_12_"))
  
  # Step 3: Reshape to long format and clean IDs
  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -date, names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id = gsub("CES_C3E_NEXT_12_", "", id_raw),
      quarter = tolower(gsub("-", "q", date)),
      var = paste0("wage_ce_", quarter)
    )
  
  # Step 4: Expand Atlantic region to all 4 provinces
  atl_values <- df_long |>
    dplyr::filter(id == "AT") |>
    dplyr::select(-id) |>
    dplyr::mutate(province = list(c("Nova Scotia", "New Brunswick", "Prince Edward Island", "Newfoundland and Labrador"))) |>
    tidyr::unnest(province)
  
  # Step 5: Add province names
  df_long <- df_long |>
    dplyr::filter(id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      id == "AB" ~ "Alberta",
      id == "BC" ~ "British Columbia",
      id == "MB" ~ "Manitoba",
      id == "ON" ~ "Ontario",
      id == "QC" ~ "Québec",
      id == "SK" ~ "Saskatchewan",
      TRUE ~ id
    )) |>
    dplyr::bind_rows(atl_values)
  
  # Step 6: Reshape to wide format
  df_wide <- df_long |>
    dplyr::select(province, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value) |>
    dplyr::arrange(province)
  
  # Step 7: Join with GeoUIDs
  provinces <- cancensus::get_census(
    dataset = "CA21",
    regions = list(C = "01"),
    level = "PR",
    geo_format = NA,
    labels = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(GeoUID, `Region Name`) |>
    dplyr::mutate(province = dplyr::case_when(
      `Region Name` == "Alberta (Alta.)" ~ "Alberta",
      `Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      `Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      `Region Name` == "Ontario (Ont.)" ~ "Ontario",
      `Region Name` == "Quebec (Que.)" ~ "Québec",
      `Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      `Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      `Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      `Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      `Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(province))
  
  # Step 8: Final output
  df_final <- df_wide |>
    dplyr::left_join(provinces, by = "province") |>
    dplyr::relocate(GeoUID, .before = province) |>
    dplyr::select(id = GeoUID, tidyselect::everything(), -province, -`Region Name`) |>
    dplyr::arrange(as.integer(id))
  
  return(df_final)
}

# Fonction CORRIGÉE pour être plus robuste
calculer_volatilite_annuelle_WIDE <- function(df, prefixe_colonne) {
  
  # CORRECTION : On utilise `matches` pour sélectionner UNIQUEMENT les colonnes
  # qui ont le format exact "prefix_YYYYqX". C'est ce qui corrige le bug _NA.
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = matches(paste0("^", prefixe_colonne, "\\d{4}q[1-4]$")),
      names_to = "trimestre_str",
      values_to = "valeur"
    ) %>%
    dplyr::mutate(
      trimestre_str = gsub(prefixe_colonne, "", trimestre_str),
      trimestre = as.yearqtr(trimestre_str)
    )
  
  # Calcul de la volatilité annuelle
  resultat_long <- df_long %>%
    dplyr::mutate(annee = year(trimestre)) %>%
    dplyr::group_by(id, annee) %>%
    dplyr::arrange(trimestre) %>%
    # CORRECTION : On s'assure qu'il y a assez de données pour un calcul stable
    dplyr::summarise(
      volatilite = if (n() >= 3) { # Il faut au moins 3 trimestres pour un écart-type de différences
        sd(diff(valeur), na.rm = TRUE) / sqrt(4)
      } else {
        NA_real_
      },
      .groups = 'drop'
    )
  
  # Pivoter le résultat en format large
  nom_variable <- gsub("_ce_?$", "", prefixe_colonne) # Enlève "_ce_" ou "_ce"
  
  resultat_wide <- resultat_long %>%
    filter(!is.na(volatilite)) %>% # On ignore les années avec un résultat NA
    tidyr::pivot_wider(
      names_from = annee,
      values_from = volatilite,
      names_prefix = paste0("volatilite_", nom_variable, "_")
    )
  
  return(resultat_wide)
}



