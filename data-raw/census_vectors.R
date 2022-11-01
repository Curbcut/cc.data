## IMPORT HOUSING CENSUS VECTORS ###############################################

census_vectors_housing <-
  tibble::tibble(
    var_code = "housing_tenant",
    type = list("pct"),
    vec_2016 = list("v_CA16_4838"),
    vec_2011 = list("v_CA11N_2254"),
    vec_2006 = list("v_CA06_103"),
    vec_2001 = list("v_CA01_100"),
    vec_1996 = list("v_CA1996_1683"),
    parent_vectors = list(NA),
    var_title = "Tenant-occupied (%)",
    var_short = "Tenant",
    explanation = "the percentage of private dwellings occupied by tenants") |>
  tibble::add_row(
    var_code = "housing_rent",
    type = list(c("avg", "dollar")),
    vec_2016 = list("v_CA16_4901"),
    vec_2011 = list("v_CA11N_2292"),
    vec_2006 = list("v_CA06_2050"),
    vec_2001 = list("v_CA01_1667"),
    vec_1996 = list("v_CA1996_1701"),
    parent_vectors = list(NA),
    var_title = "Average rent ($)",
    var_short = "Avg. rent",
    explanation = "the average rent paid by tenants per month") |>
  tibble::add_row(
    var_code = "housing_repairs",
    type = list("pct"),
    vec_2016 = list("v_CA16_4872"),
    vec_2011 = list("v_CA11N_2232"),
    vec_2006 = list("v_CA06_108"),
    vec_2001 = list("v_CA01_104"),
    vec_1996 = list("v_CA1996_1687"),
    parent_vectors = list(NA),
    var_title = "Housing requiring major repairs (%)",
    var_short = "Repairs",
    explanation = paste0("the percentage of households living in dwellings ",
                         "requiring major repairs")) |>
  tibble::add_row(
    var_code = "housing_value",
    type = list(c("avg", "dollar")),
    vec_2016 = list("v_CA16_4896"),
    vec_2011 = list("v_CA11N_2287"),
    vec_2006 = list("v_CA06_2054"),
    vec_2001 = list("v_CA01_1674"),
    vec_1996 = list("v_CA1996_1681"),
    parent_vectors = list("v_CA01_1670"),
    var_title = "Average property value ($)",
    var_short = "Avg. value",
    explanation = "the average value of owner-occupied dwellings") |>
  tibble::add_row(
    var_code = "housing_unafford",
    type = list("pct"),
    vec_2016 = list("v_CA16_4888"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Unaffordable housing (%)",
    var_short = "Unaffordable",
    explanation = paste0("the percentage of dwellings for which residents pay ",
                         "more than 30% of income on housing costs")) |>
  tibble::add_row(
    var_code = "housing_unsuit",
    type = list("pct"),
    vec_2016 = list("v_CA16_4861"),
    vec_2011 = list("v_CA11N_2276"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Unsuitable housing (%)",
    var_short = "Unsuitable",
    explanation = paste0("the percentage of households living in ",
                         "accommodations without enough bedrooms")) |>
  tibble::add_row(
    var_code = "housing_stress_renter",
    type = list("pct"),
    vec_2016 = list("v_CA16_4899"),
    vec_2011 = list("v_CA11N_2290"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Renter housing stress (%)",
    var_short = "Renter stress",
    explanation = paste0("the percentage of renter households that spend ",
                         "more than 30% of their income on shelter costs")) |>
  tibble::add_row(
    var_code = "housing_stress_owner",
    type = list("pct"),
    vec_2016 = list("v_CA16_4892"),
    vec_2011 = list("v_CA11N_2283"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Owner housing stress (%)",
    var_short = "Owner stress",
    explanation = paste0("the percentage of owner households that spend more ",
                         "than 30% of their income on shelter costs")) |>
  tibble::add_row(
    var_code = "housing_mobility_one",
    type = list("pct"),
    vec_2016 = list("v_CA16_6698"),
    vec_2011 = list("v_CA11N_1723"),
    vec_2006 = list("v_CA06_453"),
    vec_2001 = list("v_CA01_383"),
    vec_1996 = list("v_CA1996_1387"),
    parent_vectors = list(NA),
    var_title = "One-year housing mobility (%)",
    var_short = "1-year mob.",
    explanation = paste0("the percentage of households that have moved in ",
                         "the past year")) |>
  tibble::add_row(
    var_code = "housing_mobility_five",
    type = list("pct"),
    vec_2016 = list("v_CA16_6725"),
    vec_2011 = list("v_CA11N_1750"),
    vec_2006 = list("v_CA06_462"),
    vec_2001 = list("v_CA01_392"),
    vec_1996 = list("v_CA1996_1396"),
    parent_vectors = list("v_CA1996_1394"),
    var_title = "Five-year housing mobility (%)",
    var_short = "5-year mob.",
    explanation = paste0("the percentage of households that have moved in the ",
                         "past five years")) |>
  tibble::add_row(
    var_code = "housing_single_detached",
    type = list("pct"),
    vec_2016 = list("v_CA16_409"),
    vec_2011 = list("v_CA11F_200"),
    vec_2006 = list("v_CA06_120"),
    vec_2001 = list("v_CA01_113"),
    vec_1996 = list("v_CA1996_108"),
    parent_vectors = list(NA),
    var_title = "Single-detached (%)",
    var_short = "Single-detached",
    explanation = paste0("the percentage of occupied private dwellings that ",
                         "are single-detached houses"))

usethis::use_data(census_vectors_housing, overwrite = TRUE)


## IMPORT INCOME CENSUS VECTORS ################################################

census_vectors_income <-
  tibble::tibble(
    var_code = "inc_median_income",
    type = list(c("median", "dollar")),
    vec_2016 = list("v_CA16_2397"),
    vec_2011 = list("v_CA11N_2562"),
    vec_2006 = list("v_CA06_2000"),
    vec_2001 = list("v_CA01_1634"),
    vec_1996 = list("v_CA1996_1627"),
    parent_vectors = list(NA),
    var_title = "Median household income ($)",
    var_short = "Med. inc.",
    explanation = "the median before-tax household income") |>
  tibble::add_row(
    var_code = "inc_50",
    type = list("pct"),
    vec_2016 = list(c("v_CA16_2406", "v_CA16_2407", "v_CA16_2408", "v_CA16_2409",
                      paste0("v_CA16_24", 10:15))),
    vec_2011 = list(paste0("v_CA11N_25", 34:40)),
    vec_2006 = list(paste0("v_CA06_19", 89:93)),
    vec_2001 = list(paste0("v_CA01_16", 22:26)),
    vec_1996 = list(paste0("v_CA1996_16", 15:19)),
    parent_vectors = list(NA),
    var_title = "Income under $50k (%)",
    var_short = "Inc. <$50k",
    explanation = paste0("the percentage of households with an income less ",
                         "then $50,000")) |>
  tibble::add_row(
    var_code = "inc_100",
    type = list("pct"),
    vec_2016 = list(paste0("v_CA16_24", 16:20)),
    vec_2011 = list(paste0("v_CA11N_25", 41:43)),
    vec_2006 = list(paste0("v_CA06_19", 94:98)),
    vec_2001 = list(paste0("v_CA01_16", 27:31)),
    vec_1996 = list(paste0("v_CA1996_16", 20:24)),
    parent_vectors = list(NA),
    var_title = "Income between $50k-$100k (%)",
    var_short = "Inc. $50-100k",
    explanation = paste0("the percentage of households with an income between ",
                         "$50,000 and $100,000")) |>
  tibble::add_row(
    var_code = "inc_high",
    type = list("pct"),
    vec_2016 = list("v_CA16_2421"),
    vec_2011 = list(c("v_CA11N_2544", "v_CA11N_2545", "v_CA11N_2546")),
    vec_2006 = list("v_CA06_1999"),
    vec_2001 = list("v_CA01_1632"),
    vec_1996 = list("v_CA1996_1625"),
    parent_vectors = list(NA),
    var_title = "Income above $100k (%)",
    var_short = "Inc. >$100k",
    explanation = paste0("the percentage of households with an income higher ",
                         "than $100,000")) |>
  tibble::add_row(
    var_code = "inc_limat",
    type = list("pct"),
    vec_2016 = list("v_CA16_2540"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Prevalence of low income (after-tax) (%)",
    var_short = "Low income",
    explanation = paste0("the prevalence of low income in private households ",
                         "based on the Low-income measure, after-tax (LIM-AT)"))

usethis::use_data(census_vectors_income, overwrite = TRUE)

## COMBINE ALL CENSUS VECTORS ##################################################

census_vectors <- rbind(census_vectors_housing, census_vectors_income)

usethis::use_data(census_vectors, overwrite = TRUE)
