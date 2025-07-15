## IMPORT HOUSING CENSUS VECTORS ###############################################

verify_parents <- function(vectors_df, parents_df) {

  parents_type <- unique(unlist(parents_df$type))
  if (length(parents_type) > 1 | parents_type != "count") {
    stop("All vectors of `parents_df` must have `count` as type.")
  }

  if (sum(vectors_df$parent) > 0)
    stop("All vectors of `vectors_df` must be non-parent vectors. `parent` == F")

  if (sum(parents_df$parent)  != nrow(parents_df))
    stop("All vectors of `parents_df` must be parent vectors. `parent` == T")

  # Get all non-NA parent vectors and check if they're in the parents_df
  non_na_parents <- unique(vectors_df$parent_vec[!is.na(vectors_df$parent_vec)])
  if (!all(non_na_parents %in% parents_df$var_code)) {
    missing_parents <- non_na_parents[!non_na_parents %in% parents_df$var_code]
    stop(paste0("Parent vector `", missing_parents, "` is missing. \n"))
  }

  # Loop through each row in the vectors_df and check if there is a valid parent
  # vector for each year
  vec_cols <- names(vectors_df)[grepl("vec_", names(vectors_df))]
  row_indices <- seq_len(nrow(vectors_df))

  lapply(row_indices, function(row_index) {
    row_data <- vectors_df[row_index, ]
    if (is.na(row_data$parent_vec)) {
      stop("All non-parent entries need to have a parent vector for normalization.")
    }

    # Get all variables with non-NA vectors for the current row
    var_vectors <- sapply(vec_cols, function(col_name) {
      vectors <- unlist(row_data[[col_name]])
      if (all(is.na(vectors))) {
        return(NULL)
      }
      vectors
    }, simplify = FALSE, USE.NAMES = TRUE)
    existing_vars <- names(var_vectors[!sapply(var_vectors, is.null)])

    # Check if there is a parent vector for each non-NA variable vector
    parent_data <- parents_df[parents_df$var_code == row_data$parent_vec, ]
    parent_vectors <- sapply(vec_cols, function(col_name) {
      vectors <- unlist(parent_data[[col_name]])
      if (all(is.na(vectors))) {
        return(NULL)
      }
      vectors
    }, simplify = FALSE, USE.NAMES = TRUE)
    existing_parents <- names(parent_vectors[!sapply(parent_vectors, is.null)])

    # If there is a non-NA variable vector without a parent vector, throw an error
    if (!all(existing_vars %in% existing_parents)) {
      var_code <- row_data$var_code
      missing_parents <- existing_vars[!existing_vars %in% existing_parents]
      stop(paste0("`", var_code, "` is missing a parent variable for `",
                  missing_parents, "`. \n"))
    }
  })

  # Return NULL invisibly
  return(invisible(NULL))
}


census_vectors_housing <-
  tibble::tibble(
    var_code = "housing_tenant",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4239"),
    vec_2016 = list("v_CA16_4838"),
    vec_2011 = list("v_CA11N_2254"),
    vec_2006 = list("v_CA06_103"),
    vec_2001 = list("v_CA01_100"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = "Tenant-occupied (%)",
    var_short = "Tenant",
    explanation = "the percentage of private households occupied by tenants",
    exp_q5 = "are occupied by tenants",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_rent",
    type = list(c("dollar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4318"),
    vec_2016 = list("v_CA16_4901"),
    vec_2011 = list("v_CA11N_2292"),
    vec_2006 = list("v_CA06_2050"),
    vec_2001 = list("v_CA01_1667"),
    vec_1996 = list("v_CA1996_1701"),
    var_title = "Average rent ($)",
    var_short = "Avg. rent",
    explanation = "the average rent paid by tenants per month",
    exp_q5 = "the average rent is",
    rankings_chr = list(c("exceptionally inexpensive", "unusually inexpensive",
                          "just about average price", "unusually expensive",
                          "exceptionally expensive")),
    parent_vec = "tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_shelcost",
    type = list(c("dollar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4310"),
    vec_2016 = list("v_CA16_4894"),
    vec_2011 = list("v_CA11N_2285"),
    vec_2006 = list("v_CA06_2055"),
    vec_2001 = list("v_CA01_1671"),
    vec_1996 = list("v_CA1996_1704"),
    var_title = "Average shelter cost ($)",
    var_short = "Avg. shelter cost",
    explanation = "the average shelter cost paid by owners per month",
    exp_q5 = "the average shelter cost is",
    rankings_chr = list(c("exceptionally inexpensive", "unusually inexpensive",
                          "just about average price", "unusually expensive",
                          "exceptionally expensive")),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_repairs",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4274"),
    vec_2016 = list("v_CA16_4872"),
    vec_2011 = list("v_CA11N_2232"),
    vec_2006 = list("v_CA06_108"),
    vec_2001 = list("v_CA01_104"),
    vec_1996 = list("v_CA1996_1687"),
    var_title = "Housing requiring major repairs (%)",
    var_short = "Repairs",
    explanation = paste0(
      "the percentage of dwellings requiring major repairs"
    ),
    exp_q5 = "are requiring major repairs",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_value",
    type = list(c("dollar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4312"),
    vec_2016 = list("v_CA16_4896"),
    vec_2011 = list("v_CA11N_2287"),
    vec_2006 = list("v_CA06_2054"),
    vec_2001 = list("v_CA01_1674"),
    vec_1996 = list("v_CA1996_1681"),
    var_title = "Average property value ($)",
    var_short = "Avg. value",
    explanation = "the average value of owner-occupied dwellings",
    exp_q5 = "the average property value of owner-occupied dwellings is",
    rankings_chr = list(NULL),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_unafford",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4290"),
    vec_2016 = list("v_CA16_4888"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Unaffordable housing (%)",
    var_short = "Unaffordable",
    explanation = paste0(
      "the percentage of households which pay ",
      "more than 30% of income on housing costs"
    ),
    exp_q5 = "pay more than 30% of income on housing costs",
    rankings_chr = list(c("exceptionally affordable", "unusually affordable",
                          "just about average affordability",
                          "unusually unaffordable", "exceptionally unaffordable")),
    parent_vec = "owner_tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_unsuit",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4262"),
    vec_2016 = list("v_CA16_4861"),
    vec_2011 = list("v_CA11N_2276"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Unsuitable housing (%)",
    var_short = "Unsuitable",
    explanation = paste0(
      "the percentage of households living in ",
      "accommodations without enough bedrooms"
    ),
    exp_q5 = "are living in accommodations without enough bedrooms",
    rankings_chr = list(c("exceptionally suitable", "unusually suitable",
                          "just about average suitability", "unusually unsuitable",
                          "exceptionally unsuitable")),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_stress_renter",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4315"),
    vec_2016 = list("v_CA16_4899"),
    vec_2011 = list("v_CA11N_2290"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Renter housing stress (%)",
    var_short = "Renter stress",
    explanation = paste0(
      "the percentage of renter households that spend ",
      "more than 30% of their income on shelter costs"
    ),
    exp_q5 = "are under housing stress",
    rankings_chr = list(NULL),
    parent_vec = "tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_stress_owner",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4307"),
    vec_2016 = list("v_CA16_4892"),
    vec_2011 = list("v_CA11N_2283"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Owner housing stress (%)",
    var_short = "Owner stress",
    explanation = paste0(
      "the percentage of owner households that spend more ",
      "than 30% of their income on shelter costs"
    ),
    exp_q5 = "are under housing stress",
    rankings_chr = list(NULL),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_one",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_5751"),
    vec_2016 = list("v_CA16_6698"),
    vec_2011 = list("v_CA11N_1723"),
    vec_2006 = list("v_CA06_453"),
    vec_2001 = list("v_CA01_383"),
    vec_1996 = list("v_CA1996_1387"),
    var_title = "One-year housing mobility (%)",
    var_short = "1-year mob.",
    explanation = paste0("the percentage of residents that moved in ",
                         "the past year"),
    exp_q5 = "have moved in the past year",
    rankings_chr = list(NULL),
    parent_vec = "mobility_status_1",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_five",
    type = list("pct"),
    theme = "Housing",
    vec_2021 = list("v_CA21_5778"),
    vec_2016 = list("v_CA16_6725"),
    vec_2011 = list("v_CA11N_1750"),
    vec_2006 = list("v_CA06_462"),
    vec_2001 = list("v_CA01_392"),
    vec_1996 = list("v_CA1996_1396"),
    var_title = "Five-year housing mobility (%)",
    var_short = "5-year mob.",
    explanation = paste0("the percentage of residents that moved in ",
                         "the past 5 years"),
    exp_q5 = "have moved in the past five years",
    rankings_chr = list(NULL),
    parent_vec = "mobility_status_5",
    parent = FALSE
  )

census_vectors_housing_parent <-
  tibble::tibble(
    var_code = "private_households",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4237"),
    vec_2016 = list("v_CA16_4836"),
    vec_2011 = list("v_CA11N_2252"),
    vec_2006 = list("v_CA06_136"),
    vec_2001 = list("v_CA01_129"),
    vec_1996 = list("v_CA1996_1694"),
    var_title = "Households",
    var_short = "Households",
    explanation = "the total number of private households",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "private_dwellings",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4272"),
    vec_2016 = list("v_CA16_4870"),
    vec_2011 = list("v_CA11N_2230"),
    vec_2006 = list("v_CA06_105"),
    vec_2001 = list("v_CA01_96"),
    vec_1996 = list("v_CA1996_1678"),
    var_title = "Dwellings",
    var_short = "Dwellings",
    explanation = "the total number of private dwellings",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_tenant_households",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4288"),
    vec_2016 = list("v_CA16_4886"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Owner and tenant households",
    var_short = "Households",
    explanation = "the total number of owner and tenant households",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "tenant_households",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4313"),
    vec_2016 = list("v_CA16_4897"),
    vec_2011 = list("v_CA11N_2288"),
    vec_2006 = list("v_CA06_2049"),
    vec_2001 = list("v_CA01_1666"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = "Tenant households",
    var_short = "Tenant",
    explanation = "the total number of tenant households",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_households",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_4305"),
    vec_2016 = list("v_CA16_4890"),
    vec_2011 = list("v_CA11N_2281"),
    vec_2006 = list("v_CA06_2053"),
    vec_2001 = list("v_CA01_1670"),
    vec_1996 = list("v_CA1996_1682"),
    var_title = "Owner households",
    var_short = "Owner",
    explanation = "the total number of owner households",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_1",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_5745"),
    vec_2016 = list("v_CA16_6692"),
    vec_2011 = list("v_CA11N_1717"),
    vec_2006 = list("v_CA06_451"),
    vec_2001 = list("v_CA01_381"),
    vec_1996 = list("v_CA1996_1385"),
    var_title = "Residents",
    var_short = "Residents",
    explanation = "the total number of residents one year prior",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_5",
    type = list("count"),
    theme = "Housing",
    vec_2021 = list("v_CA21_5772"),
    vec_2016 = list("v_CA16_6719"),
    vec_2011 = list("v_CA11N_1744"),
    vec_2006 = list("v_CA06_460"),
    vec_2001 = list("v_CA01_390"),
    vec_1996 = list("v_CA1996_1394"),
    var_title = "Residents",
    var_short = "Residents",
    explanation = "the total number of residents five years prior",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(vectors_df = census_vectors_housing,
               parents_df = census_vectors_housing_parent)

census_vectors_housing <- rbind(census_vectors_housing,
                            census_vectors_housing_parent)

usethis::use_data(census_vectors_housing, overwrite = TRUE)


## IMPORT HOUSING TYPOLOGY #####################################################

census_vectors_typology <-
  tibble::tibble(
    var_code = "typology_single_detached",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_435"),
    vec_2016 = list("v_CA16_409"),
    vec_2011 = list("v_CA11F_200"),
    vec_2006 = list("v_CA06_120"),
    vec_2001 = list("v_CA01_113"),
    vec_1996 = list("v_CA1996_108"),
    var_title = "Single-detached (%)",
    var_short = "Single-detached",
    explanation = paste0(
      "the percentage of occupied private dwellings that ",
      "are single-detached houses"
    ),
    exp_q5 = "are single-detached houses",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_semi_detached",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_436"),
    vec_2016 = list("v_CA16_412"),
    vec_2011 = list("v_CA11F_204"),
    vec_2006 = list("v_CA06_121"),
    vec_2001 = list("v_CA01_114"),
    vec_1996 = list("v_CA1996_109"),
    var_title = "Semi-detached (%)",
    var_short = "Semi-detached",
    explanation = paste0(
      "the percentage of occupied private dwellings that are ",
      "semi-detached houses"
    ),
    exp_q5 = "are semi-detached houses",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_row_house",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_437"),
    vec_2016 = list("v_CA16_413"),
    vec_2011 = list("v_CA11F_205"),
    vec_2006 = list("v_CA06_122"),
    vec_2001 = list("v_CA01_115"),
    vec_1996 = list("v_CA1996_110"),
    var_title = "Row houses (%)",
    var_short = "Row houses",
    explanation = paste0(
      "the percentage of occupied private dwellings that are row houses"
    ),
    exp_q5 = "are row houses",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_duplex",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_438"),
    vec_2016 = list("v_CA16_414"),
    vec_2011 = list("v_CA11F_206"),
    vec_2006 = list("v_CA06_123"),
    vec_2001 = list("v_CA01_116"),
    vec_1996 = list("v_CA1996_111"),
    var_title = "Duplex (%)",
    var_short = "Duplex",
    explanation = paste0(
      "the percentage of occupied private dwellings that are duplexes"
    ),
    exp_q5 = "are duplexes",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_small",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_439"),
    vec_2016 = list("v_CA16_415"),
    vec_2011 = list("v_CA11F_207"),
    vec_2006 = list("v_CA06_125"),
    vec_2001 = list("v_CA01_118"),
    vec_1996 = list("v_CA1996_113"),
    var_title = "Apartments, less than 5 storeys (%)",
    var_short = "Apt. <5",
    explanation = paste0(
      "the percentage of occupied private dwellings that are apartments ",
      "with less than 5 storeys"
    ),
    exp_q5 = "are apartments with less than 5 storeys",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_large",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_440"),
    vec_2016 = list("v_CA16_410"),
    vec_2011 = list("v_CA11F_201"),
    vec_2006 = list("v_CA06_124"),
    vec_2001 = list("v_CA01_117"),
    vec_1996 = list("v_CA1996_112"),
    var_title = "Apartments, 5 or more storeys (%)",
    var_short = "Apt. >=5",
    explanation = paste0(
      "the percentage of occupied private dwellings that are apartments ",
      "with 5 or more storeys"
    ),
    exp_q5 = "are apartments with 5 or more storeys",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_other_single",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_441"),
    vec_2016 = list("v_CA16_416"),
    vec_2011 = list("v_CA11F_208"),
    vec_2006 = list("v_CA06_126"),
    vec_2001 = list("v_CA01_119"),
    vec_1996 = list("v_CA1996_114"),
    var_title = "Other single-attached (%)",
    var_short = "Other single-attached",
    explanation = paste0(
      "the percentage of occupied private dwellings that are other types of ",
      "single-attached dwellings"
    ),
    exp_q5 = "are other types of single-attached dwellings",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_movable",
    type = list("pct"),
    theme = "Building typology",
    vec_2021 = list("v_CA21_442"),
    vec_2016 = list("v_CA16_417"),
    vec_2011 = list("v_CA11F_202"),
    vec_2006 = list("v_CA06_127"),
    vec_2001 = list("v_CA01_120"),
    vec_1996 = list("v_CA1996_115"),
    var_title = "Movable dwellings (%)",
    var_short = "Movable dwellings",
    explanation = paste0(
      "the percentage of occupied private dwellings that are movable dwellings"
    ),
    exp_q5 = "are movable dwellings",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  )


verify_parents(vectors_df = census_vectors_typology,
               parents_df = census_vectors_housing_parent)

census_vectors_typology <- rbind(census_vectors_typology)

usethis::use_data(census_vectors_typology, overwrite = TRUE)

## IMPORT BEDROOM SIZE CENSUS VECTORS ##########################################

census_vectors_bedroomsize <-
  tibble::tibble(
    var_code = "bedroom_zero",
    type = list("pct"),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4245"),
    vec_2016 = list("v_CA16_4844"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "0 bedrooms (%)",
    var_short = "0 bedrooms",
    explanation = paste0(
      "the percentage of occupied private dwellings with zero bedrooms"
    ),
    exp_q5 = "have zero bedrooms",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_one",
    type = list("pct"),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4246"),
    vec_2016 = list("v_CA16_4845"),
    # BECAUSE 2011 LISTS 0 AND 1 TOGETHER !
    # vec_2011 = list("v_CA11N_2248"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "1 bedroom (%)",
    var_short = "1 bedroom",
    explanation = paste0(
      "the percentage of occupied private dwellings with one bedroom"
    ),
    exp_q5 = "have one bedroom",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_two",
    type = list("pct"),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4247"),
    vec_2016 = list("v_CA16_4846"),
    # vec_2011 = list("v_CA11N_2249"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "2 bedrooms (%)",
    var_short = "2 bedrooms",
    explanation = paste0(
      "the percentage of occupied private dwellings with two bedrooms"
    ),
    exp_q5 = "have two bedrooms",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_three",
    type = list("pct"),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4248"),
    vec_2016 = list("v_CA16_4847"),
    # vec_2011 = list("v_CA11N_2250"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "3 bedrooms (%)",
    var_short = "3 bedrooms",
    explanation = paste0(
      "the percentage of occupied private dwellings with three bedrooms"
    ),
    exp_q5 = "have three bedrooms",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_four",
    type = list("pct"),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4249"),
    vec_2016 = list("v_CA16_4848"),
    # vec_2011 = list("v_CA11N_2251"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "4 or more bedrooms (%)",
    var_short = "4+ bedrooms",
    explanation = paste0(
      "the percentage of occupied private dwellings with four or more bedrooms"
    ),
    exp_q5 = "have four or more bedrooms",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  )

verify_parents(vectors_df = census_vectors_bedroomsize,
               parents_df = census_vectors_housing_parent)

usethis::use_data(census_vectors_bedroomsize, overwrite = TRUE)

## IMPORT BEDROOM SIZE CENSUS VECTORS ##########################################

census_vectors_buildingage <-
  tibble::tibble(
    var_code = "buildingage_1960constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4264"),
    vec_2016 = list("v_CA16_4863"),
    vec_2011 = list("v_CA11N_2234"),
    vec_2006 = list(c("v_CA06_110", "v_CA06_111")),
    vec_2001 = list(c("v_CA01_105", "v_CA01_106")),
    vec_1996 = list(NA),
    var_title = "Built <1960 (%)",
    var_short = "Pre-1960",
    explanation = paste0(
      "the percentage of occupied private dwellings built before 1960"
    ),
    exp_q5 = "were built before 1960",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_1980constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4265"),
    vec_2016 = list("v_CA16_4864"),
    vec_2011 = list("v_CA11N_2235"),
    vec_2006 = list("v_CA06_113"),
    vec_2001 = list(c("v_CA01_107", "v_CA01_108")),
    vec_1996 = list(NA),
    var_title = "Built 1961-1980 (%)",
    var_short = "1961-1980",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 1961 and 1980"
    ),
    exp_q5 = "were built between 1961 and 1980",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_1990constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4266"),
    vec_2016 = list("v_CA16_4865"),
    vec_2011 = list("v_CA11N_2236"),
    vec_2006 = list(c("v_CA06_114", "v_CA06_115")),
    vec_2001 = list("v_CA01_109"),
    vec_1996 = list(NA),
    var_title = "Built 1981-1990 (%)",
    var_short = "1981-1990",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 1981 and 1990"
    ),
    exp_q5 = "were built between 1981 and 1990",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2000constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4267"),
    vec_2016 = list("v_CA16_4866"),
    vec_2011 = list("v_CA11N_2237"),
    vec_2006 = list(c("v_CA06_116", "v_CA06_117")),
    vec_2001 = list(c("v_CA01_110", "v_CA01_111")),
    vec_1996 = list(NA),
    var_title = "Built 1991-2000 (%)",
    var_short = "1991-2000",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 1991 and 2000"
    ),
    exp_q5 = "were built between 1991 and 2000",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2005constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4268"),
    vec_2016 = list("v_CA16_4867"),
    vec_2011 = list("v_CA11N_2238"),
    vec_2006 = list("v_CA06_118"),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Built 2001-2005 (%)",
    var_short = "2001-2005",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 2001 and 2005"
    ),
    exp_q5 = "were built between 2001 and 2005",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2010constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4269"),
    vec_2016 = list("v_CA16_4868"),
    vec_2011 = list("v_CA11N_2239"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Built 2006-2010 (%)",
    var_short = "2006-2010",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 2006 and 2010"
    ),
    exp_q5 = "were built between 2006 and 2010",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2015constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4270"),
    vec_2016 = list("v_CA16_4869"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Built 2011-2015 (%)",
    var_short = "2011-2015",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 2011 and 2015"
    ),
    exp_q5 = "were built between 2011 and 2015",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2020constr",
    type = list("pct"),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4271"),
    vec_2016 = list(NA),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Built 2016-2020 (%)",
    var_short = "2016-2020",
    explanation = paste0(
      "the percentage of occupied private dwellings built between 2016 and 2020"
    ),
    exp_q5 = "were built between 2016 and 2020",
    rankings_chr = list(NULL),
    parent_vec = "private_dwellings",
    parent = FALSE
  )

verify_parents(vectors_df = census_vectors_buildingage,
               parents_df = census_vectors_housing_parent)

usethis::use_data(census_vectors_buildingage, overwrite = TRUE)


## IMPORT HOUSEHOLD SIZE CENSUS VECTORS ########################################

census_vectors_householdsize <-
  tibble::tibble(
    var_code = "household_size_1",
    type = list("pct"),
    theme = "Households",
    vec_2021 = list("v_CA21_444"),
    vec_2016 = list("v_CA16_419"),
    vec_2011 = list("v_CA11F_210"),
    vec_2006 = list("v_CA06_129"),
    vec_2001 = list("v_CA01_122"),
    vec_1996 = list("v_CA1996_117"),
    var_title = "One-person (%)",
    var_short = "One",
    explanation = paste0(
      "the percentage of private households that ",
      "are occupied by a single individual"
    ),
    exp_q5 = "are occupied by a single individual",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_2",
    type = list("pct"),
    theme = "Households",
    vec_2021 = list("v_CA21_445"),
    vec_2016 = list("v_CA16_420"),
    vec_2011 = list("v_CA11F_211"),
    vec_2006 = list("v_CA06_130"),
    vec_2001 = list("v_CA01_123"),
    vec_1996 = list("v_CA1996_118"),
    var_title = "Two-person (%)",
    var_short = "Two",
    explanation = paste0(
      "the percentage of private households that ",
      "are occupied by two people"
    ),
    exp_q5 = "are occupied by two people",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_3",
    type = list("pct"),
    theme = "Households",
    vec_2021 = list("v_CA21_446"),
    vec_2016 = list("v_CA16_421"),
    vec_2011 = list("v_CA11F_212"),
    vec_2006 = list("v_CA06_131"),
    vec_2001 = list("v_CA01_124"),
    vec_1996 = list("v_CA1996_119"),
    var_title = "Three-person (%)",
    var_short = "Three",
    explanation = paste0(
      "the percentage of private households that ",
      "are occupied by three people"
    ),
    exp_q5 = "are occupied by three people",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_4",
    type = list("pct"),
    theme = "Households",
    vec_2021 = list(c("v_CA21_447", "v_CA21_448")),
    vec_2016 = list(c("v_CA16_422", "v_CA16_423")),
    vec_2011 = list(c("v_CA11F_213", "v_CA11F_214", "v_CA11F_215")),
    vec_2006 = list(c("v_CA06_132", "v_CA06_133")),
    vec_2001 = list(c("v_CA01_125", "v_CA01_126")),
    vec_1996 = list(c("v_CA1996_120", "v_CA1996_121")),
    var_title = "Four-or-more-person (%)",
    var_short = "Four+",
    explanation = paste0(
      "the percentage of private households that ",
      "are occupied by four or more people"
    ),
    exp_q5 = "are occupied by four or more people",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
    tibble::add_row(
      var_code = "average_household_size",
      type = list("avg"),
      theme = "Households",
      vec_2021 = list("v_CA21_452"),
      vec_2016 = list("v_CA16_425"),
      vec_2011 = list("v_CA11F_217"),
      vec_2006 = list("v_CA06_135"),
      vec_2001 = list("v_CA01_128"),
      vec_1996 = list("v_CA1996_1699"),
      var_title = "Average household size",
      var_short = "Avgsize",
      explanation = "the average number of people per private household",
      exp_q5 = "are occupied on average by a certain number of people",
      rankings_chr = list(NULL),
      parent_vec = "private_households",
      parent = FALSE
    )

# The parent is shared with housing
from_hou <- census_vectors_housing_parent[
  census_vectors_housing_parent$var_code %in% census_vectors_householdsize$parent_vec, ]

verify_parents(vectors_df = census_vectors_householdsize,
               parents_df = from_hou)

usethis::use_data(census_vectors_householdsize, overwrite = TRUE)


## IMPORT INCOME CENSUS VECTORS ################################################

census_vectors_income <-
  tibble::tibble(
    var_code = "inc_median_income",
    type = list(c("dollar","median")),
    theme = "Income",
    vec_2021 = list("v_CA21_906"),
    vec_2016 = list("v_CA16_2397"),
    vec_2011 = list("v_CA11N_2562"),
    vec_2006 = list("v_CA06_2000"),
    vec_2001 = list("v_CA01_1634"),
    vec_1996 = list("v_CA1996_1627"),
    var_title = "Median household income ($)",
    var_short = "Med. inc.",
    explanation = "the median before-tax household income",
    exp_q5 = "the median household income is",
    rankings_chr = list(NULL),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
    tibble::add_row(
      var_code = "inc_average_income",
      type = list("dollar","average"),
      theme = "Income",
      vec_2021 = list("v_CA21_915"),
      vec_2016 = list("v_CA16_4985"),
      vec_2011 = list("v_CA11N_2563"),
      vec_2006 = list("v_CA06_2001"),
      vec_2001 = list("v_CA01_1633"),
      vec_1996 = list("v_CA1996_1626"),
      var_title = "Average household income ($)",
      var_short = "Avg. inc.",
      explanation = "the average before-tax household income",
      exp_q5 = "the average household income is",
      rankings_chr = list(NULL),
      parent_vec = "private_households",
      parent = FALSE
    ) |>
  tibble::add_row(
    var_code = "inc_50",
    type = list("pct"),
    theme = "Income",
    vec_2021 = list(c("v_CA21_674", "v_CA21_677", "v_CA21_680", "v_CA21_683",
                      "v_CA21_686")),
    vec_2016 = list(c(
      "v_CA16_2406", "v_CA16_2407", "v_CA16_2408", "v_CA16_2409",
      paste0("v_CA16_24", 10:15)
    )),
    vec_2011 = list(paste0("v_CA11N_25", 34:40)),
    vec_2006 = list(paste0("v_CA06_19", 89:93)),
    vec_2001 = list(paste0("v_CA01_16", 22:26)),
    vec_1996 = list(paste0("v_CA1996_16", 15:19)),
    var_title = "Income under $50k (%)",
    var_short = "Inc. <$50k",
    explanation = paste0(
      "the percentage of labour force individuals with an income less ",
      "than $50,000"
    ),
    exp_q5 = "have an income of less than $50,000",
    rankings_chr = list(NULL),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_100",
    type = list("pct"),
    theme = "Income",
    vec_2021 = list(c("v_CA21_689", "v_CA21_692", "v_CA21_695", "v_CA21_698",
                    "v_CA21_701")),
    vec_2016 = list(paste0("v_CA16_24", 16:20)),
    vec_2011 = list(paste0("v_CA11N_25", 41:43)),
    vec_2006 = list(paste0("v_CA06_19", 94:98)),
    vec_2001 = list(paste0("v_CA01_16", 27:31)),
    vec_1996 = list(paste0("v_CA1996_16", 20:24)),
    var_title = "Income between $50k-$100k (%)",
    var_short = "Inc. $50-100k",
    explanation = paste0(
      "the percentage of labour force individuals with an income between ",
      "$50,000 and $100,000"
    ),
    exp_q5 = "have an income between $50,000 and $100,000",
    rankings_chr = list(NULL),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_high",
    type = list("pct"),
    theme = "Income",
    vec_2021 = list("v_CA21_704"),
    vec_2016 = list("v_CA16_2421"),
    vec_2011 = list(c("v_CA11N_2544", "v_CA11N_2545", "v_CA11N_2546")),
    vec_2006 = list("v_CA06_1999"),
    vec_2001 = list("v_CA01_1632"),
    vec_1996 = list("v_CA1996_1625"),
    var_title = "Income above $100k (%)",
    var_short = "Inc. >$100k",
    explanation = paste0(
      "the percentage of labour force individuals with an income higher ",
      "than $100,000"
    ),
    exp_q5 = "have an income higher than $100,000",
    rankings_chr = list(NULL),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_limat",
    type = list("pct"),
    theme = "Income",
    vec_2021 = list("v_CA21_1040"),
    vec_2016 = list("v_CA16_2540"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Prevalence of low income (after-tax) (%)",
    var_short = "Low income",
    explanation = paste0(
      "the prevalence of low income in private households ",
      "based on the Low-income measure, after-tax (LIM-AT)"
    ),
    exp_q5 = "are considered low income",
    parent_vec = "income_status",
    parent = FALSE
  )

census_vectors_income_parent <-
  tibble::tibble(
    var_code = "with_income",
    type = list("count"),
    theme = "Income",
    vec_2021 = list("v_CA21_671"),
    vec_2016 = list("v_CA16_2405"),
    vec_2011 = list("v_CA11N_2533"),
    vec_2006 = list("v_CA06_1988"),
    vec_2001 = list("v_CA01_1621"),
    vec_1996 = list("v_CA1996_1614"),
    var_title = "Labour force individuals",
    var_short = "With income",
    explanation = "the total population aged 15 years and over with an income",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "income_status",
    type = list("count"),
    theme = "Income",
    vec_2021 = list("v_CA21_1010"),
    vec_2016 = list("v_CA16_2510"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Households",
    var_short = "Households",
    explanation = paste0("the total population in private households to whom ",
                         "low-income concepts are applicable"),
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )


# addition of needed parent available from the housing vectors
from_hou <- census_vectors_housing_parent[
  census_vectors_housing_parent$var_code %in% census_vectors_income$parent_vec, ]

verify_parents(vectors_df = census_vectors_income,
               parents_df = rbind(census_vectors_income_parent, from_hou))

census_vectors_income <- rbind(census_vectors_income,
                               census_vectors_income_parent)

usethis::use_data(census_vectors_income, overwrite = TRUE)


## IMPORT IDENTITY CENSUS VECTORS ##############################################

census_vectors_identity <-
  tibble::tibble(
    var_code = "citizenship_imm",
    type = list("pct"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4410"),
    vec_2016 = list("v_CA16_3411"),
    vec_2011 = list("v_CA11N_22"),
    vec_2006 = list("v_CA06_478"),
    vec_2001 = list("v_CA01_406"),
    vec_1996 = list("v_CA1996_128"),
    var_title = "Immigrants (%)",
    var_short = "Immigrants",
    explanation = "the percentage of individuals who are foreign-born",
    exp_q5 = "are foreign-born",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_new",
    type = list("pct"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4431"),
    vec_2016 = list("v_CA16_3432"),
    vec_2011 = list("v_CA11N_43"),
    vec_2006 = list("v_CA06_553"),
    vec_2001 = list("v_CA01_507"),
    vec_1996 = list("v_CA1996_228"),
    var_title = "New immigrants (%)",
    var_short = "New immigrants",
    explanation = paste0(
      "the percentage of individuals who have immigrated in ",
      "the last five years"
    ),
    exp_q5 = "have immigrated in the last five years",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_vm",
    type = list("pct"),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4875"),
    vec_2016 = list("v_CA16_3957"),
    vec_2011 = list("v_CA11N_460"),
    vec_2006 = list("v_CA06_1303"),
    vec_2001 = list("v_CA01_703"),
    vec_1996 = list("v_CA1996_784"),
    var_title = "Visible minorities (%)",
    var_short = "Vis. minorities",
    explanation = paste0(
      "the percentage of individuals who identify as part of ",
      "one or more visible minority groups"
    ),
    exp_q5 = "identify as part of one or more visible minority groups",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_aboriginal",
    type = list("pct"),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4204"),
    vec_2016 = list("v_CA16_3855"),
    vec_2011 = list("v_CA11N_1354"),
    vec_2006 = list("v_CA06_565"),
    vec_2001 = list("v_CA01_718"),
    vec_1996 = list("v_CA1996_473"),
    var_title = "Indigenous (%)",
    var_short = "Indigenous",
    explanation = "the percentage of individuals who are of indigenous identity",
    exp_q5 = "are of indigenous identity",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonpr",
    type = list("pct"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4434"),
    vec_2016 = list("v_CA16_3435"),
    vec_2011 = list("v_CA11N_46"),
    vec_2006 = list("v_CA06_511"),
    vec_2001 = list("v_CA01_458"),
    vec_1996 = list("v_CA1996_180"),
    var_title = "Non-permanent residents (%)",
    var_short = "Non-permanent",
    explanation = "the percentage of individuals who are non-permanent residents",
    exp_q5 = "are non-permanent residents",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonimm",
    type = list("pct"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4407"),
    vec_2016 = list("v_CA16_3408"),
    vec_2011 = list("v_CA11N_19"),
    vec_2006 = list("v_CA06_475"),
    vec_2001 = list("v_CA01_403"),
    vec_1996 = list("v_CA1996_126"),
    var_title = "Non-immigrants (%)",
    var_short = "Non-immigrants",
    explanation = "the percentage of individuals who are non-immigrants",
    exp_q5 = "are non-immigrants",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_cit",
    type = list("pct"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4392"),
    vec_2016 = list("v_CA16_3393"),
    vec_2011 = list("v_CA11N_4"),
    vec_2006 = list("v_CA06_470"),
    vec_2001 = list("v_CA01_400"),
    vec_1996 = list("v_CA1996_123"),
    var_title = "Canadian citizens (%)",
    var_short = "Citizens",
    explanation = "the percentage of individuals who are Canadian citizens",
    exp_q5 = "are Canadian citizens",
    rankings_chr = list(NULL),
    parent_vec = "population_ph",
    parent = FALSE
  )

census_vectors_identity_parent <-
  tibble::tibble(
    var_code = "population_ph",
    type = list("count"),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4404"),
    vec_2016 = list("v_CA16_3405"),
    vec_2011 = list("v_CA11N_16"),
    vec_2006 = list("v_CA06_474"),
    vec_2001 = list("v_CA01_402"),
    vec_1996 = list("v_CA1996_125"),
    var_title = "Individuals",
    var_short = "Individuals",
    explanation = "the total count of individuals in private households",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(vectors_df = census_vectors_identity,
               parents_df = census_vectors_identity_parent)

census_vectors_identity <- rbind(census_vectors_identity,
                                 census_vectors_identity_parent)

usethis::use_data(census_vectors_identity, overwrite = TRUE)


## IMPORT TRANSPORT CENSUS VECTORS #############################################

census_vectors_transport <-
  tibble::tibble(
    var_code = "trans_car",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list("v_CA21_7635"),
    vec_2016 = list(c("v_CA16_5795", "v_CA16_5798")),
    vec_2011 = list(c("v_CA11N_2194", "v_CA11N_2197")),
    vec_2006 = list(c("v_CA06_1101", "v_CA06_1102")),
    vec_2001 = list(c("v_CA01_1255", "v_CA01_1256", "v_CA01_1264", "v_CA01_1265")),
    vec_1996 = list(c(
      "v_CA1996_1326", "v_CA1996_1327", "v_CA1996_1335",
      "v_CA1996_1336"
    )),
    var_title = "Drive to work (%)",
    var_short = "Drive",
    explanation = paste0(
      "the percentage of individuals who drive a privately ",
      "owned car or truck to work"
    ),
    exp_q5 = "drive a privately owned car or truck to work",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_walk_or_bike",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7647", "v_CA21_7650")),
    vec_2016 = list(c("v_CA16_5804", "v_CA16_5807")),
    vec_2011 = list(c("v_CA11N_2203", "v_CA11N_2206")),
    vec_2006 = list(c("v_CA06_1104", "v_CA06_1105")),
    vec_2001 = list(c("v_CA01_1258", "v_CA01_1259", "v_CA01_1267", "v_CA01_1268")),
    vec_1996 = list(c(
      "v_CA1996_1329", "v_CA1996_1330", "v_CA1996_1338",
      "v_CA1996_1339"
    )),
    var_title = "Walk or cycle to work (%)",
    var_short = "Walk or cycle",
    explanation = "the percentage of individuals who walk or cycle to work",
    exp_q5 = "walk or cycle to work",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_transit",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list("v_CA21_7644"),
    vec_2016 = list("v_CA16_5801"),
    vec_2011 = list("v_CA11N_2200"),
    vec_2006 = list("v_CA06_1103"),
    vec_2001 = list(c("v_CA01_1266", "v_CA01_1257")),
    vec_1996 = list(c("v_CA1996_1337", "v_CA1996_1328")),
    var_title = "Public transit to work (%)",
    var_short = "Transit",
    explanation = paste0(
      "the percentage of individuals who use public transit ",
      "to get to work"
    ),
    exp_q5 = "use public transit to get to work",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_15",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list("v_CA21_7659"),
    vec_2016 = list("v_CA16_5816"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Commute under 15 minutes (%)",
    var_short = "Commute <15m",
    explanation = paste0(
      "the percentage of individuals whose commute time is ",
      "less than 15 minutes"
    ),
    exp_q5 = "have a commute time of less than 15 minutes",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7662", "v_CA21_7665")),
    vec_2016 = list(c("v_CA16_5819", "v_CA16_5822")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Commute 15-45 minutes (%)",
    var_short = "Commute 14-45m",
    explanation = paste0(
      "the percentage of individuals whose commute time is ",
      "between 15 and 45 minutes"
    ),
    exp_q5 = "have a commute time between 15 and 45 minutes",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_plus",
    type = list("pct"),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7668", "v_CA21_7671")),
    vec_2016 = list(c("v_CA16_5825", "v_CA16_5828")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = "Commute more than 45 minutes (%)",
    var_short = "Commute >45m",
    explanation = paste0(
      "the percentage of individuals whose commute time is ",
      "longer than 45 minutes"
    ),
    exp_q5 = "have a commute time longer than 45 minutes",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  )

census_vectors_transport_parent <-
  tibble::tibble(
    var_code = "employment_lf",
    type = list("count"),
    theme = "Identity",
    vec_2021 = list("v_CA21_7632"),
    vec_2016 = list("v_CA16_5792"),
    vec_2011 = list("v_CA11N_2191"),
    vec_2006 = list("v_CA06_1100"),
    vec_2001 = list(c("v_CA01_1254", "v_CA01_1263")),
    vec_1996 = list("v_CA1996_1324"),
    var_title = "Employed individuals",
    var_short = "Individuals",
    explanation = paste0("the total count of employed labour force aged 15 ",
                         "years and over with a usual place of work or no ",
                         "fixed workplace address"),
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(vectors_df = census_vectors_transport,
               parents_df = census_vectors_transport_parent)

census_vectors_transport <- rbind(census_vectors_transport,
                                 census_vectors_transport_parent)

usethis::use_data(census_vectors_transport, overwrite = TRUE)


## IMPORT EMPLOYMENT CENSUS VECTORS ############################################

# census_vectors_employment <-
#   tibble::tibble(
#     var_code = "emp_professional",
#     type = list("pct"),
#     theme = "Employment",
#     vec_2021 = list(c("v_CA21_6642", "v_CA21_6645")),
#     vec_2016 = list(c("v_CA16_5735", "v_CA16_5738")),
#     vec_2011 = list(c("v_CA11N_2107", "v_CA11N_2110")),
#     vec_2006 = list(c("v_CA06_1021", "v_CA06_1022")),
#     vec_2001 = list(c("v_CA01_1181", "v_CA01_1182")),
#     vec_1996 = list(NA),
#     var_title = "Managerial and professional occupations (%)",
#     var_short = "Professional",
#     explanation = paste0(
#       "the percentage of the workforce in professional and ",
#       "managerial occupations, based on the North American ",
#       "Industry Classification System"
#     )
#   ) |>
#   tibble::add_row(
#     var_code = "emp_creative",
#     type = list("pct"),
#     theme = "Employment",
#     vec_2021 = list(c("v_CA21_6633", "v_CA21_6657")),
#     vec_2016 = list(c("v_CA16_5726", "v_CA16_5750")),
#     vec_2011 = list(c("v_CA11N_2098", "v_CA11N_2122")),
#     vec_2006 = list(c("v_CA06_1018", "v_CA06_1026")),
#     vec_2001 = list(c("v_CA01_1178", "v_CA01_1186")),
#     vec_1996 = list(NA),
#     var_title = "Creative occupations (%)",
#     var_short = "Creative",
#     explanation = paste0(
#       "the percentage of the workforce in artistic and ",
#       "cultural occupations, based on the North American ",
#       "Industry Classification System"
#     )
#   )
#
# usethis::use_data(census_vectors_employment, overwrite = TRUE)

census_vectors_employment <-
  tibble::tibble(
    var_code = "employment_er",
    type = list("pct"),
    theme = "Employment",
    vec_2021 = list("v_CA21_6498"),
    vec_2016 = list("v_CA16_5603"),
    vec_2011 = list("v_CA11N_1993"),
    vec_2006 = list("v_CA06_577"),
    vec_2001 = list("v_CA01_737"),
    vec_1996 = list("v_CA1996_799"),
    var_title = "Employment Rate (%)",
    var_short = "Employed",
    explanation = paste0(
      "the percentage of individuals who are 15 years or older ",
      "and are employed"
    ),
    exp_q5 = "are employed",
    rankings_chr = list(NULL),
    parent_vec = "employment_15older",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_uer",
    type = list("pct"),
    theme = "Employment",
    vec_2021 = list("v_CA21_6501"),
    vec_2016 = list("v_CA16_5606"),
    vec_2011 = list("v_CA11N_1996"),
    vec_2006 = list("v_CA06_578"),
    vec_2001 = list("v_CA01_738"),
    vec_1996 = list("v_CA1996_800"),
    var_title = "Unemployment Rate (%)",
    var_short = "Unemployed",
    explanation = paste0(
      "the percentage of individuals who are in the labour force ",
      "and are unemployed"
    ),
    exp_q5 = "are unemployed",
    rankings_chr = list(NULL),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powoutmun",
    type = list("pct"),
    theme = "Employment",
    vec_2021 = list(c("v_CA21_7623", "v_CA21_7626", "v_CA21_7629")),
    vec_2016 = list(c("v_CA16_5783", "v_CA16_5786", "v_CA16_5789")),
    vec_2011 = list(NA),
    vec_2006 = list(c("v_CA06_1079", "v_CA06_1082")),
    vec_2001 = list(c("v_CA01_1240", "v_CA01_1243", "v_CA01_1248", "v_CA01_1251")),
    vec_1996 = list(c("v_CA1996_1311", "v_CA1996_1314", "v_CA1996_1319", "v_CA1996_1322")),
    var_title = "Work outside municipality of residence (%)",
    var_short = "Outside",
    explanation = paste0(
      "the percentage of individuals who work outside ",
      "their municipality of residence"
    ),
    exp_q5 = "work outside their municipality of residence",
    rankings_chr = list(NULL),
    parent_vec = "employment_lfupow",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powinmun",
    type = list("pct"),
    theme = "Employment",
    vec_2021 = list("v_CA21_7620"),
    vec_2016 = list("v_CA16_5780"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1078"),
    vec_2001 = list(c("v_CA01_1239", "v_CA01_1247")),
    vec_1996 = list(c("v_CA1996_1310", "v_CA1996_1318")),
    var_title = "Work in municipality of residence (%)",
    var_short = "Within",
    explanation = paste0(
      "the percentage of individuals who work in ",
      "their municipality of residence"
    ),
    exp_q5 = "work in their municipality of residence",
    rankings_chr = list(NULL),
    parent_vec = "employment_lfupow",
    parent = FALSE
  ) |>
  # TKTK VECTORS SEEM OFF
  # tibble::add_row(
  #   var_code = "employment_powphys",
  #   type = list("pct"),
  #   theme = "Employment",
  #   vec_2021 = list(c("v_CA21_7614", "v_CA21_7611", "v_CA21_7608")),
  #   vec_2016 = list(c("v_CA16_5768", "v_CA16_5771", "v_CA16_5774")),
  #   vec_2011 = list(c("v_CA11N_2182", "v_CA11N_2185", "v_CA11N_2188")),
  #   vec_2006 = list(c("v_CA06_1077", "v_CA06_1082", "v_CA06_1083")),
  #   vec_2001 = list(c("v_CA06_1077", "v_CA01_1243", "v_CA01_1244", "v_CA01_1246",
  #                     "v_CA01_1251", "v_CA01_1252")),
  #   vec_1996 = list(c("v_CA1996_1309", "v_CA1996_1317", "v_CA1996_1314",
  #                     "v_CA1996_1322", "v_CA1996_1315", "v_CA1996_1323")),
  #   var_title = "Work at a physical work location (%)",
  #   var_short = "Work at a physical work location",
  #   explanation = paste0(
  #     "the percentage of employed individuals who work ",
  #     "at a physical work location"
  #   ),
  #   exp_q5 = "work at a physical work location",
  #   rankings_chr = list(NULL),
  #   parent_vec = "employment_em",
  #   parent = FALSE
  # ) |>
  tibble::add_row(
    var_code = "employment_powhome",
    type = list("pct"),
    theme = "Employment",
    vec_2021 = list("v_CA21_7605"),
    vec_2016 = list("v_CA16_5765"),
    vec_2011 = list("v_CA11N_2179"),
    vec_2006 = list("v_CA06_1081"),
    vec_2001 = list(c("v_CA01_1242", "v_CA01_1250")),
    vec_1996 = list(c("v_CA1996_1313", "v_CA1996_1321")),
    var_title = "Work at home (%)",
    var_short = "Home",
    explanation = paste0(
      "the percentage of employed individuals who work ",
      "at home"
    ),
    exp_q5 = "work at home",
    rankings_chr = list(NULL),
    parent_vec = "employment_em",
    parent = FALSE
  )

census_vectors_employment_parent <-
  tibble::tibble(
    var_code = "employment_15older",
    type = list("count"),
    theme = "Employment",
    vec_2021 = list("v_CA21_6492"),
    vec_2016 = list("v_CA16_5597"),
    vec_2011 = list("v_CA11N_1987"),
    vec_2006 = list("v_CA06_575"),
    vec_2001 = list("v_CA01_735"),
    vec_1996 = list("v_CA1996_797"),
    var_title = "Individuals aged 15 years or older",
    var_short = "15 years or older",
    explanation = "the total population aged 15 years and over",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lfupow",
    type = list("count"),
    theme = "Employment",
    vec_2021 = list("v_CA21_7617"),
    vec_2016 = list("v_CA16_5777"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1077"),
    vec_2001 = list(c("v_CA01_1238", "v_CA01_1246")),
    vec_1996 = list(c("v_CA1996_1309", "v_CA1996_1317")),
    var_title = "Individuals employed in the labour force with a usual place of work",
    var_short = "Usual place of work",
    explanation = "the number of employed individuals with a usual place of work",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_em",
    type = list("count"),
    theme = "Employment",
    vec_2021 = list("v_CA21_6498"),
    vec_2016 = list("v_CA16_5603"),
    vec_2011 = list("v_CA11N_1993"),
    vec_2006 = list("v_CA06_577"),
    vec_2001 = list("v_CA01_737"),
    vec_1996 = list("v_CA1996_799"),
    var_title = "Employed individuals in the labour force",
    var_short = "Employed",
    explanation = "the number of individuals in the labour force who are employed",
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )


# addition of needed parent available from the transport vectors
from_transport <- census_vectors_transport_parent[
  census_vectors_transport_parent$var_code %in% census_vectors_employment$parent_vec, ]

verify_parents(vectors_df = census_vectors_employment,
               parents_df = rbind(census_vectors_employment_parent, from_transport))

census_vectors_employment <- rbind(census_vectors_employment,
                                   census_vectors_employment_parent)

usethis::use_data(census_vectors_employment, overwrite = TRUE)


## IMPORT FAMILY CENSUS VECTORS ############################################

census_vectors_family <-
  tibble::tibble(
    var_code = "family_children",
    type = list("pct"),
    theme = "Household",
    vec_2021 = list(c("v_CA21_502", "v_CA21_505")),
    vec_2016 = list(c("v_CA16_494", "v_CA16_495", "v_CA16_496", "v_CA16_498",
                      "v_CA16_499", "v_CA16_500")),
    vec_2011 = list(c("v_CA11F_129", "v_CA11F_119", "v_CA11F_125")),
    vec_2006 = list(c("v_CA06_65", "v_CA06_59", "v_CA06_69")),
    vec_2001 = list(c("v_CA01_63", "v_CA01_57", "v_CA01_67")),
    vec_1996 = list(c("v_CA1996_68", "v_CA1996_74", "v_CA1996_78")),
    var_title = "Families with children (%)",
    var_short = "With child",
    explanation = "the percentage of census families with children out of total households",
    exp_q5 = "live with children",
    rankings_chr = list(NULL),
    parent_vec = "census_families",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "family_one_person",
    type = list("pct"),
    theme = "Household",
    vec_2021 = list("v_CA21_444"),
    vec_2016 = list("v_CA16_419"),
    vec_2011 = list("v_CA11F_210"),
    vec_2006 = list("v_CA06_129"),
    vec_2001 = list("v_CA01_122"),
    vec_1996 = list("v_CA1996_117"),
    var_title = "Living alone (%)",
    var_short = "Living alone",
    explanation = "the percentage of one person households out of total households",
    exp_q5 = "are one-person households",
    rankings_chr = list(NULL),
    parent_vec = "census_families",
    parent = FALSE
  )

census_vectors_family_parent <-
  tibble::tibble(
    var_code = "census_families",
    type = list("count"),
    theme = "Household",
    vec_2021 = list("v_CA21_443"),
    vec_2016 = list("v_CA16_418"),
    vec_2011 = list("v_CA11F_209"),
    vec_2006 = list("v_CA06_128"),
    vec_2001 = list("v_CA01_121"),
    vec_1996 = list("v_CA1996_116"),
    var_title = "Census families",
    var_short = "Families",
    explanation = paste0("the total count of census families"),
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(vectors_df = census_vectors_family,
               parents_df = census_vectors_family_parent)

census_vectors_family <- rbind(census_vectors_family,
                                  census_vectors_family_parent)

usethis::use_data(census_vectors_family, overwrite = TRUE)


## IMPORT LANGUAGE CENSUS VECTORS ##############################################

census_vectors_language <-
  tibble::tibble(
    var_code = "lang_french_only",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_1150",
    vec_2016 = "v_CA16_518",
    vec_2011 = "v_CA11F_557",
    vec_2006 = "v_CA06_245",
    vec_2001 = "v_CA01_215",
    vec_1996 = "v_CA1996_312",
    var_title = "Knows French only (%)",
    var_short = "Fr. only",
    explanation = paste0(
      "the percentage of individuals that only know French ",
      "as an official language"
    ),
    exp_q5 = "only know French out of the two official languages",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_eng_only",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_1147",
    vec_2016 = "v_CA16_515",
    vec_2011 = "v_CA11F_554",
    vec_2006 = "v_CA06_244",
    vec_2001 = "v_CA01_214",
    vec_1996 = "v_CA1996_311",
    var_title = "Knows English only (%)",
    var_short = "Eng. only",
    explanation = paste0(
      "the percentage of individuals that only know English ",
      "as an official language"
    ),
    exp_q5 = "only know English out of the two official languages",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_eng",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_1153",
    vec_2016 = "v_CA16_521",
    vec_2011 = "v_CA11F_560",
    vec_2006 = "v_CA06_246",
    vec_2001 = "v_CA01_216",
    vec_1996 = "v_CA1996_313",
    var_title = "Knows French and English (%)",
    var_short = "Fr. and Eng.",
    explanation = paste0(
      "the percentage of individuals that know both official ",
      "languages (French and English)"
    ),
    exp_q5 = "know both official languages (French and English)",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_no_official",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_1156",
    vec_2016 = "v_CA16_524",
    vec_2011 = "v_CA11F_563",
    vec_2006 = "v_CA06_247",
    vec_2001 = "v_CA01_217",
    vec_1996 = "v_CA1996_314",
    var_title = "Knows neither French nor English (%)",
    var_short = "Non-official",
    explanation = paste0(
      "the percentage of individuals that do not know either ",
      "of the official languages (French or English)"
    ),
    exp_q5 = "do not know either of the official languages (French or English)",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_eng",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_2209",
    vec_2016 = "v_CA16_1364",
    vec_2011 = "v_CA11F_593",
    vec_2006 = "v_CA06_257",
    vec_2001 = "v_CA01_227",
    vec_1996 = "v_CA1996_324",
    var_title = "Most often speak English at home (%)",
    var_short = "English",
    explanation = paste0(
      "the percentage of individuals that most often speak ",
      "English at home"
    ),
    exp_q5 = "most often speak English at home",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_fr",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_2212",
    vec_2016 = "v_CA16_1367",
    vec_2011 = "v_CA11F_596",
    vec_2006 = "v_CA06_258",
    vec_2001 = "v_CA01_228",
    vec_1996 = "v_CA1996_325",
    var_title = "Most often speak French at home (%)",
    var_short = "French",
    explanation = paste0(
      "the percentage of individuals that most often speak ",
      "French at home"
    ),
    exp_q5 = "most often speak French at home",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_nonof",
    type = list("pct"),
    theme = "Language",
    vec_2021 = "v_CA21_2215",
    vec_2016 = "v_CA16_1370",
    vec_2011 = "v_CA11F_599",
    vec_2006 = "v_CA06_259",
    vec_2001 = "v_CA01_229",
    vec_1996 = "v_CA1996_326",
    var_title = "Most often speak a non-official language at home (%)",
    var_short = "Non-official",
    explanation = paste0(
      "the percentage of individuals that most often speak ",
      "a non-official language at home"
    ),
    exp_q5 = "most often speak a non-official language at home",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  )

census_vectors_language_parent <-
  tibble::tibble(
    var_code = "c_population",
    type = list("count"),
    theme = "Identity",
    vec_2021 = list("v_CA21_1"),
    vec_2016 = list("v_CA16_401"),
    vec_2011 = list("v_CA11F_1"),
    vec_2006 = list("v_CA06_1"),
    vec_2001 = list("v_CA01_2"),
    vec_1996 = list("v_CA1996_2"),
    var_title = "Individuals",
    var_short = "Individuals",
    explanation = paste0("the total count of individuals"),
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(vectors_df = census_vectors_language,
               parents_df = census_vectors_language_parent)

census_vectors_language <- rbind(census_vectors_language,
                                 census_vectors_language_parent)

usethis::use_data(census_vectors_language, overwrite = TRUE)


## IMPORT AGE CENSUS VECTORS ###################################################

census_vectors_age <-
  tibble::tibble(
    var_code = "age_0_14",
    type = list("pct"),
    theme = "Age",
    vec_2021 = list("v_CA21_11"),
    vec_2016 = list("v_CA16_4"),
    vec_2011 = list(c("v_CA11F_8", "v_CA11F_11", "v_CA11F_14")),
    vec_2006 = list(c(
      "v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_23",
      "v_CA06_24", "v_CA06_25"
    )),
    vec_2001 = list(c(
      "v_CA01_7", "v_CA01_8", "v_CA01_9", "v_CA01_26",
      "v_CA01_27", "v_CA01_28"
    )),
    vec_1996 = list(c(
      "v_CA1996_7", "v_CA1996_31", "v_CA1996_8", "v_CA1996_32",
      "v_CA1996_9", "v_CA1996_33"
    )),
    var_title = "Aged between 0 and 14 (%)",
    var_short = "0-14 yo",
    explanation = "the percentage of the population aged between 0 and 14 years old",
    exp_q5 = "are aged between 0 and 14 years old",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "age_15_64",
    type = list("pct"),
    theme = "Age",
    vec_2021 = list("v_CA21_68"),
    vec_2016 = list("v_CA16_61"),
    vec_2011 = list(paste0("v_CA11F_", c(17, 35, 38, 41, 44, 47, 50, 53, 56, 59))),
    vec_2006 = list(c(paste0("v_CA06_", 7:16), paste0("v_CA06_", 26:35))),
    vec_2001 = list(c(paste0("v_CA01_", 10:19), paste0("v_CA01_", 29:38))),
    vec_1996 = list(c(paste0("v_CA1996_", 15:24), paste0("v_CA1996_", 39:48))),
    var_title = "Aged between 15 and 64 (%)",
    var_short = "15-64 yo",
    explanation = "the percentage of the population aged between 15 and 64 years old",
    exp_q5 = "are aged between 15 and 64 years old",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "age_65_plus",
    type = list("pct"),
    theme = "Age",
    vec_2021 = list("v_CA21_251"),
    vec_2016 = list("v_CA16_244"),
    vec_2011 = list(c(paste0("v_CA11F_", c(62, 65, 68, 71, 74)))),
    vec_2006 = list(c(paste0("v_CA06_", 17:21), paste0("v_CA06_", 36:40))),
    vec_2001 = list(c(paste0("v_CA01_", 20:24), paste0("v_CA01_", 39:43))),
    vec_1996 = list(c(paste0("v_CA1996_", 25:29), paste0("v_CA1996_", 49:53))),
    var_title = "Aged 65 and above (%)",
    var_short = "65+ yo",
    explanation = "the percentage of the population aged 65 and above",
    exp_q5 = "are aged 65 and above",
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  )


# Function to always subset the vectors which match the parent
get_rows_from_parent <- function(vecs, parent_vec) {
  df <- vecs[vecs$parent_vector == parent_vec, ]
  df[!is.na(df$vector), ]
}

# Function to get the age vectors by year from the census dataset
recent_census <- function(census_dataset) {

  vecs <- cancensus::list_census_vectors(census_dataset)
  vec_total <- vecs[grepl("Age", vecs$label), ]$vector[1]

  # Extracting rows related to total age
  total_age <- get_rows_from_parent(vecs, vec_total)

  # Iterate through each category and sub-category of ages
  all_ages <- lapply(total_age$vector, \(bcat) {

    mid_cat <- get_rows_from_parent(vecs, bcat)
    out <- lapply(mid_cat$vector, \(mcat) {
      get_rows_from_parent(vecs, mcat)
    })

    # Additional level of depth in some cases
    out <- lapply(out, \(t) {
      if (sum(grepl(" to ", t$label)) > 0) {
        w <- which(grepl(" to ", t$label))
        for (i in w) {
          outt <- lapply(t$vector[w], \(lcat) {
            get_rows_from_parent(vecs, lcat)
          })
        }
        Reduce(rbind, c(outt, list(t[-w,])))
      } else {
        t
      }
    })

    Reduce(rbind, out)

  })

  Reduce(rbind, all_ages)
}

older_census <- function(census_dataset, vec) {
  vecs <- cancensus::list_census_vectors(census_dataset)

  # Using the vectors of Total - Age, get its children
  total_age <- get_rows_from_parent(vecs, vec)

  total_age_2006 <- lapply(total_age$vector, \(ta) {
    get_rows_from_parent(vecs, ta)
  })

  # MALE AND FEMALE
  total_age_2006
}

total_age_2021 <- recent_census("CA21")
total_age_2016 <- recent_census("CA16")
total_age_2011 <- get_rows_from_parent(cancensus::list_census_vectors("CA11"), "v_CA11F_5")
total_age_2006 <- older_census("CA06", "v_CA06_2")
total_age_2001 <- older_census("CA01", "v_CA01_5")
total_age_1996 <- older_census("CA1996", "v_CA1996_5")


# Function to clean up the dataframe
cleanup <- function(df, year) {
  t <- df[c("vector", "label")]
  t$label <- gsub(" to |-", ":", t$label)
  t$label <- gsub(" years| years and over|\\+", "", t$label)
  t$year <- year
  t
}

# Function to handle and clean older data
handle_older_data <- function(data) {
  mapply(function(z, year) {
    out <- if (!is.data.frame(z)) lapply(z, cleanup, year) else cleanup(z, year)
    binded <- Reduce(rbind, out)
    listed <- if (!is.data.frame(z)) lapply(unique(binded$label), function(x) binded$vector[binded$label == x]) else z$vector
    if (!is.data.frame(z)) out <- out[[1]]
    out$vector <- listed
    out
  }, data, c("2011", "2006", "2001", "1996"), SIMPLIFY = FALSE)
}

older <- list(total_age_2011, total_age_2006, total_age_2001, total_age_1996)
names(older) <- c("2011", "2006", "2001", "1996")
cleaned_older <- handle_older_data(older)

# Processing recent data and structuring into the desired format
recent <- mapply(\(x, year) {

  categories <- lapply(cleaned_older$`2011`$label, \(z) eval(parse(text = z)))
  second_penultimate_vecs <- lapply(categories[2:(length(categories)-1)], \(c) x$vector[x$label %in% c])
  first <- list(x$vector[1:5])
  last <- list(x$vector[which(x$label == 85):nrow(x)])
  tibble::tibble(vector = c(first, second_penultimate_vecs, last),
                 label = cleaned_older$`2011`$label,
                 year = year)

}, list(total_age_2021, total_age_2016), c("2021", "2016"), SIMPLIFY = FALSE)

names(recent) <- c("2021", "2016")

# Bind
final <- c(recent, cleaned_older)

# Create age page tibble
categories <- unique(final[[1]]$label)

vectors_table <- lapply(categories, \(cat) {

  var_code <- gsub(":", "_", cat)
  var_code <- sprintf("age_%s", var_code)

  vec_2021 <- final$`2021`$vector[final$`2021`$label == cat]
  vec_2016 <- final$`2016`$vector[final$`2016`$label == cat]
  vec_2011 <- final$`2011`$vector[final$`2011`$label == cat]
  vec_2006 <- final$`2006`$vector[final$`2006`$label == cat]
  vec_2001 <- final$`2001`$vector[final$`2001`$label == cat]
  vec_1996 <- final$`1996`$vector[final$`1996`$label == cat]


  if (grepl(":", cat)) {
    two_digts <- strsplit(cat, ":")[[1]]
    var_title <- sprintf("Aged between %s and %s (%%)", two_digts[1], two_digts[2])
    var_short <-  sprintf("%s yo", gsub(":", "-",  cat))
    explanation <-  sprintf("the percentage of the population aged between %s and %s years old", two_digts[1], two_digts[2])
    exp_q5 <-  sprintf("are aged between %s and %s years old", two_digts[1], two_digts[2])
  } else {
    var_title <-  "Aged 85 and above (%)"
    var_short  <-  "85+ yo"
    explanation <-  "the percentage of the population aged 85 and above"
    exp_q5 <-  "are aged 85 and above"
  }

  tibble::tibble(
    var_code = var_code,
    type = list("pct"),
    theme = "Age",
    vec_2021 = vec_2021,
    vec_2016 = vec_2016,
    vec_2011 = vec_2011,
    vec_2006 = vec_2006,
    vec_2001 = vec_2001,
    vec_1996 = vec_1996,
    var_title = var_title,
    var_short = var_short,
    explanation = explanation,
    exp_q5 = exp_q5,
    rankings_chr = list(NULL),
    parent_vec = "c_population",
    parent = FALSE
  )

})

census_vectors_age_page <- Reduce(rbind, vectors_table)

census_vectors_age <- rbind(census_vectors_age, census_vectors_age_page)


#### PARENT VECTOR `POPULATION` ALREADY PART OF `LANGUAGE`

# verify_parents(vectors_df = census_vectors_age,
#                parents_df = census_vectors_age_parent)
#
# census_vectors_age <- rbind(census_vectors_age,
#                             census_vectors_age_parent)

usethis::use_data(census_vectors_age, overwrite = TRUE)


## IMPORT EDUCATION CENSUS VECTORS #############################################

census_vectors_education <-
  tibble::tibble(
    var_code = "edu_no_degree",
    type = list("pct"),
    theme = "Education",
    vec_2021 = list("v_CA21_5820"),
    vec_2016 = list("v_CA16_5054"),
    vec_2011 = list("v_CA11N_1774"),
    vec_2006 = list(c("v_CA06_1235", "v_CA06_1249", "v_CA06_1263")),
    vec_2001 = list("v_CA01_1387"),
    vec_1996 = list("v_CA1996_1350"),
    var_title = "No certificate, diploma or degree (%)",
    var_short = "No degree",
    explanation = paste0(
      "the percentage of the population aged 15 and over ",
      "with no certificate, diploma or degree"
    ),
    exp_q5 = "hold no certificate, diploma or degree",
    rankings_chr = list(NULL),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_secondary",
    type = list("pct"),
    theme = "Education",
    vec_2021 = list("v_CA21_5823"),
    vec_2016 = list("v_CA16_5057"),
    vec_2011 = list("v_CA11N_1777"),
    vec_2006 = list(c("v_CA06_1237", "v_CA06_1251", "v_CA06_1265")),
    vec_2001 = list("v_CA01_1387"),
    vec_1996 = list("v_CA1996_1351"),
    var_title = "Secondary school diploma or equivalent (%)",
    var_short = "Secondary",
    explanation = paste0(
      "the percentage of the population aged 15 and over ",
      "holding a secondary (high) school diploma or equivalency certificate"
    ),
    exp_q5 = "hold a secondary (high) school diploma or equivalency certificate",
    rankings_chr = list(NULL),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_nonuni",
    type = list("pct"),
    theme = "Education",
    vec_2021 = list("v_CA21_5841"),
    vec_2016 = list("v_CA16_5072"),
    vec_2011 = list("v_CA11N_1786"),
    vec_2006 = list(c("v_CA06_1239", "v_CA06_1253", "v_CA06_1267")),
    vec_2001 = list("v_CA01_1392"),
    vec_1996 = list("v_CA1996_1355"),
    var_title = "College, CEGEP or other non-university certificate or diploma (%)",
    var_short = "College",
    explanation = paste0(
      "the percentage of the population aged 15 and over ",
      "holding a college, CEGEP or other non-university certificate or diploma"
    ),
    exp_q5 = "hold a college, CEGEP or other non-university certificate or diploma",
    rankings_chr = list(NULL),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_uni_below",
    type = list("pct"),
    theme = "Education",
    vec_2021 = list("v_CA21_5844"),
    vec_2016 = list("v_CA16_5075"),
    vec_2011 = list("v_CA11N_1789"),
    vec_2006 = list(c("v_CA06_1241", "v_CA06_1255", "v_CA06_1269")),
    vec_2001 = list("v_CA01_1396"),
    vec_1996 = list("v_CA1996_1359"),
    var_title = "University certificate or diploma below bachelor level (%)",
    var_short = "Uni. below bachelor",
    explanation = paste0(
      "the percentage of the population aged 15 and over ",
      "holding a university certificate or diploma below bachelor level"
    ),
    exp_q5 = "hold a university certificate or diploma below bachelor level",
    rankings_chr = list(NULL),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_bachelor_above",
    type = list("pct"),
    theme = "Education",
    vec_2021 = list("v_CA21_5847"),
    vec_2016 = list("v_CA16_5078"),
    vec_2011 = list("v_CA11N_1792"),
    vec_2006 = list(c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268")),
    vec_2001 = list("v_CA01_1397"),
    vec_1996 = list("v_CA1996_1360"),
    var_title = "Bachelor and above (%)",
    var_short = "Bachelor+",
    explanation = paste0(
      "the percentage of the population aged 15 and over ",
      "holding a University certificate, diploma or degree at bachelor level ",
      "or above"
    ),
    exp_q5 = "hold a University certificate, diploma or degree at bachelor level or above",
    rankings_chr = list(NULL),
    parent_vec = "population_15plus",
    parent = FALSE
  )


census_vectors_education_parent <-
  tibble::tibble(
    var_code = "population_15plus",
    type = list("count"),
    theme = "Identity",
    vec_2021 = list("v_CA21_5817"),
    vec_2016 = list("v_CA16_5051"),
    vec_2011 = list("v_CA11N_1771"),
    vec_2006 = list(c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262")),
    vec_2001 = list("v_CA01_1384"),
    vec_1996 = list("v_CA1996_1347"),
    var_title = "Individuals aged 15 and over",
    var_short = "Individuals",
    explanation = paste0("the total count of individuals aged 15 years and ",
                         "over in private households"),
    exp_q5 = NA,
    rankings_chr = list(NULL),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(vectors_df = census_vectors_education,
               parents_df = census_vectors_education_parent)

census_vectors_education <- rbind(census_vectors_education,
                                  census_vectors_education_parent)



usethis::use_data(census_vectors_education, overwrite = TRUE)


## COMBINE ALL CENSUS VECTORS ##################################################

census_vectors_table <- rbind(
  census_vectors_housing,
  census_vectors_householdsize,
  census_vectors_income,
  census_vectors_identity,
  census_vectors_transport,
  census_vectors_employment,
  census_vectors_family,
  census_vectors_language,
  census_vectors_age,
  census_vectors_education,
  census_vectors_bedroomsize,
  census_vectors_buildingage,
  census_vectors_typology
)

usethis::use_data(census_vectors_table, overwrite = TRUE)
