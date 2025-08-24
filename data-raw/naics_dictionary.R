## Scrape NAICS Classification Dictionary (EN/FR) from Statistics Canada
## This function scrapes the hierarchical NAICS classification structure (2-digit to 6-digit levels) 
## from the official Statistics Canada website, in English, French, or both.

naics_get_dictionary <- function(
  url_en = "https://www.statcan.gc.ca/en/statistical-programs/document/naics-scian-2022-structure-v1-eng.csv",
  url_fr = "https://www.statcan.gc.ca/fr/programmes-statistiques/document/naics-scian-2022-structure-v1-fra.csv"
) {
# Load required packages
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required. Please install it.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required. Please install it.")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  stop("Package 'stringr' is required. Please install it.")
}

# Helper function: drop columns starting with "super"
drop_super <- function(df) {
  super_cols <- names(df)[stringr::str_detect(tolower(names(df)), "^super")]
  if (length(super_cols) > 0) df <- dplyr::select(df, -dplyr::all_of(super_cols))
  df
}

# Read and clean English CSV
en <- readr::read_csv(url_en, show_col_types = FALSE) |>
  drop_super() |>
  dplyr::rename(
    level = "Level",
    hierarchical_structure = "Hierarchical structure",
    code = "Code",
    parent = "Parent",
    class_title = "Class title",
    class_definition = "Class definition"
  ) |>
  dplyr::select(level, hierarchical_structure, code, parent, class_title, class_definition) |>
  dplyr::mutate(code = as.character(code))

# Read and clean French CSV
fr <- readr::read_csv(url_fr, show_col_types = FALSE) |>
  drop_super() |>
  dplyr::rename(
    level = "Niveau",
    hierarchical_structure = "Structure hiérarchique",
    code = "Code",
    parent = "Parent",
    class_title = "Titres de classes",
    class_definition = "Définitions de la classe"
  ) |>
  dplyr::select(level, hierarchical_structure, code, parent, class_title, class_definition) |>
  dplyr::mutate(code = as.character(code))

# Return as list
list(en = en, fr = fr)
}

naics_dictionary <- naics_get_dictionary()

usethis::use_data(naics_dictionary, overwrite = TRUE)
