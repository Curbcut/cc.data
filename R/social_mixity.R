#' Build social mixity index
#'
#' This function builds the Social Mixity index dataset
#'
#' @param DB_table <`sf data.frame`> An \code{sf} object representing the
#' dissemination areas (DAs) to calculate the CanALE index for.
#'
#' @return A data.frame object representing the CanALE index for the given year
#' and DAs.
#' @export
build_social_mixity <- function(DB_table = bucket_read_object_zip_shp("DB_shp_carto.zip", "curbcut.rawdata")) {

  # Get travel time matrices by foot. Only 900 seconds (15 minutes walk)
  ttm_foot <- {
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    query <- "SELECT * FROM ttm_foot_DB WHERE travel_seconds < 900"
    DBI::dbGetQuery(conn, query)
  }
  # In the ttm_foot, add self
  ttm_foot <- rbind(ttm_foot, tibble::tibble(from = DB_table$DBUID,
                                             to = DB_table$DBUID,
                                             travel_seconds = 0))

  # We'll remove the three last digits of the DB IDs, so that it shows DA
  # IDs instead.
  ttm_foot_DA <- ttm_foot
  ttm_foot_DA$from <- gsub("\\d{3}$", "", ttm_foot_DA$from)
  ttm_foot_DA$to <- gsub("\\d{3}$", "", ttm_foot_DA$to)
  ttm_foot_DA <- ttm_foot_DA[c("from", "to")]
  ttm_foot_DA <- unique(ttm_foot_DA)

  # Load census variables ---------------------------------------------------

  # Socioprofessionnelles
  noc_total <- c(#"total" = "v_CA21_6561", "na" = "v_CA21_6564",
    "all" = "v_CA21_6567",
    "0" = "v_CA21_6570", "1" = "v_CA21_6573", "2" = "v_CA21_6576",
    "3" = "v_CA21_6579", "4" = "v_CA21_6582", "5" = "v_CA21_6585",
    "6" = "v_CA21_6588", "7" = "v_CA21_6591", "8" = "v_CA21_6594",
    "9" = "v_CA21_6597")

  # Cultures
  c21 <- cancensus::list_census_vectors("CA21")
  ethn_cult <- c21$vector[c21$parent_vector == "v_CA21_4917"]
  ethn_cult <- ethn_cult[!is.na(ethn_cult)]
  ethn_cult <- c("v_CA21_4917", ethn_cult)

  # Tranches d'âge
  age <- c21$vector[c21$parent_vector %in% c("v_CA21_11", "v_CA21_68", "v_CA21_251")]
  age <- age[!is.na(age)]
  age <- c("v_CA21_8", age)

  # Income
  income <- c21$vector[c21$parent_vector == "v_CA21_671"]
  income <- income[!is.na(income)]
  income <- c("v_CA21_671", income)

  # Education
  education <- c21$vector[c21$parent_vector == "v_CA21_5817"]
  education <- education[!is.na(education)]
  education <- c("v_CA21_5817", education)

  # Combine and name
  vectors <- c(noc_total, ethn_cult, age, income, education)
  names(vectors) <- sapply(c(noc_total, ethn_cult, age, income, education), \(x) {
    c21$label[c21$vector == x]
  }, simplify = TRUE)

  # Troubles with getting DA nation-wide. Get provinces and bind.
  pr_codes <- cancensus::list_census_regions("CA21", quiet = TRUE)
  pr_codes <- pr_codes$region[pr_codes$level == "PR"]
  pr_codes <- lapply(pr_codes, \(x) list(PR = x))
  all_pr_vecs <- lapply(pr_codes, \(reg) {
    get_census_data(census_dataset = "CA21",
                    var_codes = vectors,
                    region = reg, scale = "DA")
  })
  all_pr_vecs <- Reduce(rbind, all_pr_vecs)


  # Sum up, for every DA, the values of all the DAs accessible by foot ------

  require(data.table)
  # Convert data frames to data.tables for faster operations
  all_pr_vecs_dt <- as.data.table(all_pr_vecs)
  ttm_foot_DA_dt <- as.data.table(ttm_foot_DA)

  # Set the GeoUID as the key for faster join
  setkey(all_pr_vecs_dt, GeoUID)
  setkey(ttm_foot_DA_dt, from)

  # Perform join to create a mapping of from-to relationships
  ttm_foot_DA_joined <- ttm_foot_DA_dt[all_pr_vecs_dt, on = .(from = GeoUID), nomatch = 0]

  # Group by 'from' and compute the column sums across matching GeoUIDs
  aggregated_result <- ttm_foot_DA_joined[, lapply(.SD, sum), by = .(from),
                                          .SDcols = setdiff(names(all_pr_vecs_dt), "GeoUID")]

  # Update 'all_pr_vecs' with the aggregated result, joining by the GeoUID
  all_pr_vecs_dt[aggregated_result, on = .(GeoUID = from),
                 (names(aggregated_result)[-1]) := mget(paste0("i.", names(aggregated_result)[-1]))]

  # Convert back to data.frame if necessary
  all_pr_vecs <- as.data.frame(all_pr_vecs_dt)


  # Calculate in percentages ------------------------------------------------

  education_cols <- which(names(all_pr_vecs) %in% c("No certificate, diploma or degree",
                                                    "Postsecondary certificate, diploma or degree"))
  education_total <- which(names(all_pr_vecs) == "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households")
  education_col_nb <- education_cols[1]:education_cols[length(education_cols)]
  for (i in education_col_nb) {
    all_pr_vecs[[i]] <- all_pr_vecs[[i]] / all_pr_vecs[[education_total]]
  }

  income_cols <- which(names(all_pr_vecs) %in% c("Under $10,000 (including loss)",
                                                 "$100,000 and over"))
  income_total <- which(names(all_pr_vecs) == "With total income")
  income_col_nb <- income_cols[1]:income_cols[length(income_cols)]
  for (i in income_col_nb) {
    all_pr_vecs[[i]] <- all_pr_vecs[[i]] / all_pr_vecs[[income_total]]
  }

  age_cols <- which(names(all_pr_vecs) %in% c("0 to 4 years",
                                              "85 years and over"))
  age_total <- which(names(all_pr_vecs) == "Total - Age")
  age_col_nb <- age_cols[1]:age_cols[length(age_cols)]
  for (i in age_col_nb) {
    all_pr_vecs[[i]] <- all_pr_vecs[[i]] / all_pr_vecs[[age_total]]
  }

  ethn_cult_cols <- which(names(all_pr_vecs) %in% c("Canadian",
                                                    "Paraguayan"))
  ethn_cult_total <- which(names(all_pr_vecs) == "Total - Ethnic or cultural origin for the population in private households")
  ethn_cult_col_nb <- ethn_cult_cols[1]:ethn_cult_cols[length(ethn_cult_cols)]
  for (i in ethn_cult_col_nb) {
    all_pr_vecs[[i]] <- all_pr_vecs[[i]] / all_pr_vecs[[ethn_cult_total]]
  }

  sociopro_cols <- which(names(all_pr_vecs) %in% c("0 Legislative and senior management occupations",
                                                   "9 Occupations in manufacturing and utilities"))
  sociopro_total <- which(names(all_pr_vecs) == "All occupations")
  sociopro_col_nb <- sociopro_cols[1]:sociopro_cols[length(sociopro_cols)]
  for (i in sociopro_col_nb) {
    all_pr_vecs[[i]] <- all_pr_vecs[[i]] / all_pr_vecs[[sociopro_total]]
  }

  # Do entropy ---------------------------------------------------

  calculate_entropy <- function(percentages) {
    # Remplacer les zéros par des valeurs très petites pour éviter les problèmes de log(0)
    percentages[percentages == 0] <- .Machine$double.eps
    # Calculer l'entropie de Shannon
    entropy <- -sum(percentages * log(percentages), na.rm = TRUE)
    return(entropy)
  }

  # Calcul de l'entropie pour chaque catégorie
  sociopro <- apply(sf::st_drop_geometry(all_pr_vecs)[sociopro_col_nb], MARGIN = 1,
                    FUN = calculate_entropy, simplify = TRUE)
  ethn_cult <- apply(sf::st_drop_geometry(all_pr_vecs)[ethn_cult_col_nb], MARGIN = 1,
                     FUN = calculate_entropy, simplify = TRUE)
  age <- apply(sf::st_drop_geometry(all_pr_vecs)[age_col_nb], MARGIN = 1,
               FUN = calculate_entropy, simplify = TRUE)
  income <- apply(sf::st_drop_geometry(all_pr_vecs)[income_col_nb], MARGIN = 1,
                  FUN = calculate_entropy, simplify = TRUE)
  education <- apply(sf::st_drop_geometry(all_pr_vecs)[education_col_nb], MARGIN = 1,
                     FUN = calculate_entropy, simplify = TRUE)

  # Normalisation des entropies
  normalize_entropy <- function(entropy) {
    entropy_without_Inf <- entropy[!is.infinite(entropy)]
    (entropy - min(entropy_without_Inf, na.rm = TRUE)) / (
      max(entropy_without_Inf, na.rm = TRUE) - min(entropy_without_Inf, na.rm = TRUE))
  }

  sociopro_norm <- normalize_entropy(sociopro)
  ethn_cult_norm <- normalize_entropy(ethn_cult)
  age_norm <- normalize_entropy(age)
  income_norm <- normalize_entropy(income)
  education_norm <- normalize_entropy(education)


  # Make it a table ---------------------------------------------------------

  tb <- all_pr_vecs["GeoUID"]
  tb$composite <- sociopro_norm + ethn_cult_norm + age_norm + income_norm + education_norm
  tb$sociopro <- sociopro_norm
  tb$ethn_cult <- ethn_cult_norm
  tb$age <- age_norm
  tb$income <- income_norm
  tb$education <- education_norm
  tb[sapply(tb, is.infinite)] <- NA

  names(tb)[names(tb) != "GeoUID"] <- paste0(
    names(tb)[names(tb) != "GeoUID"], "_2021"
  )
  names(tb)[1] <- "DA_ID"

  return(tb)
}
