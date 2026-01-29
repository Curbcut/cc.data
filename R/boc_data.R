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

  url <- "https://www.bankofcanada.ca/valet/observations/group/CES_C16_DEMOGRAPHICS/csv"
  df <- readr::read_csv(url, skip = 68, col_names = TRUE)

  df_filtered <- df |>
    dplyr::select("date", dplyr::starts_with("CES_C16E_FUTURE_"))

  df_long <- df_filtered |>
    tidyr::pivot_longer(cols = -"date", names_to = "id_raw", values_to = "value") |>
    dplyr::mutate(
      id      = gsub("^CES_C16E_FUTURE_", "", .data$id_raw),
      quarter = tolower(gsub("-", "q", .data$date)),
      var     = paste0("access_credit_ce_", .data$quarter)
    )

  atl_values <- df_long |>
    dplyr::filter(.data$id == "AT") |>
    dplyr::select(dplyr::all_of(setdiff(names(df_long), "id"))) |>
    dplyr::mutate(province = list(c(
      "Nova Scotia", "New Brunswick",
      "Prince Edward Island", "Newfoundland and Labrador"
    ))) |>
    tidyr::unnest(cols = "province")   # <= ICI la correction

  df_long <- df_long |>
    dplyr::filter(.data$id != "AT") |>
    dplyr::mutate(province = dplyr::case_when(
      .data$id == "AB" ~ "Alberta",
      .data$id == "BC" ~ "British Columbia",
      .data$id == "MB" ~ "Manitoba",
      .data$id == "ON" ~ "Ontario",
      .data$id == "QC" ~ "Québec",
      .data$id == "SK" ~ "Saskatchewan",
      TRUE ~ .data$id
    )) |>
    dplyr::bind_rows(atl_values)

  df_wide <- df_long |>
    dplyr::select(dplyr::all_of(c("province", "var", "value"))) |>
    tidyr::pivot_wider(names_from = "var", values_from = "value") |>
    dplyr::arrange(.data$province)

  provinces <- cancensus::get_census(
    dataset   = "CA21",
    regions   = list(C = "01"),
    level     = "PR",
    geo_format = NA,
    labels    = "short",
    use_cache = TRUE
  ) |>
    dplyr::select(dplyr::all_of(c("GeoUID", "Region Name"))) |>
    dplyr::mutate(province = dplyr::case_when(
      .data$`Region Name` == "Alberta (Alta.)" ~ "Alberta",
      .data$`Region Name` == "British Columbia (B.C.)" ~ "British Columbia",
      .data$`Region Name` == "Manitoba (Man.)" ~ "Manitoba",
      .data$`Region Name` == "Ontario (Ont.)" ~ "Ontario",
      .data$`Region Name` == "Quebec (Que.)" ~ "Québec",
      .data$`Region Name` == "Saskatchewan (Sask.)" ~ "Saskatchewan",
      .data$`Region Name` == "Nova Scotia (N.S.)" ~ "Nova Scotia",
      .data$`Region Name` == "New Brunswick (N.B.)" ~ "New Brunswick",
      .data$`Region Name` == "Prince Edward Island (P.E.I.)" ~ "Prince Edward Island",
      .data$`Region Name` == "Newfoundland and Labrador (N.L.)" ~ "Newfoundland and Labrador",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(.data$province))

  out_join <- dplyr::left_join(df_wide, provinces, by = "province")

  keep_cols <- setdiff(names(df_wide), "province")

  dplyr::bind_cols(
    tibble::tibble(id = out_join$GeoUID),
    out_join[, keep_cols, drop = FALSE]
  ) |>
    dplyr::arrange(as.integer(.data$id))
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
  # 1) Liste des CMA
  cma_list <- cancensus::list_census_regions("CA21") |>
    dplyr::filter(.data$level == "CMA") |>
    dplyr::select(dplyr::all_of(c("region", "name")))
  
  # 2) Données census CMA (sans géométrie)
  cma_all <- cancensus::get_census(
    dataset   = "CA21",
    regions   = list(CMA = cma_list$region),
    geo_format = "sf",
    level     = "CMA"
  ) |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(c("name", "GeoUID"))) |>
    dplyr::mutate(name = gsub("\\s*\\(.*?\\)", "", .data$name))
  
  # 3) Excel des élasticités
  elasticities <- readxl::read_excel(file_path) |>
    dplyr::filter(.data$cma == 1) |>
    dplyr::select(dplyr::all_of(c("city", "elasticity"))) |>
    dplyr::rename(name = .data$city)
  
  # 4) Normalisation des noms
  name_mapping <- c(
    "Greater Sudbury/Grand Sudbury" = "Greater Sudbury / Grand Sudbury",
    "Kitchener-Cambridge-Waterloo"  = "Kitchener - Cambridge - Waterloo",
    "Ottawa-Gatineau"               = "Ottawa - Gatineau",
    "St. Catharines-Niagara"        = "St. Catharines - Niagara",
    "Trois-Rivieres"                = "Trois-Rivières",
    "Quebec"                        = "Québec",
    "Montreal"                      = "Montréal",
    "St. John's"                    = "St. John's",
    "Abbotsford-Mission"            = "Abbotsford - Mission"
  )
  elasticities <- elasticities |>
    dplyr::mutate(name = dplyr::recode(.data$name, !!!name_mapping))
  
  # 5) Jointure + sortie propre
  dplyr::inner_join(cma_all, elasticities, by = "name") |>
    dplyr::rename(
      id = .data$GeoUID,
      elasticity_estimates = .data$elasticity
    ) |>
    dplyr::select(dplyr::all_of(c("id", "elasticity_estimates"))) |>
    tibble::as_tibble()
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

