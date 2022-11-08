## IMPORT HOUSING CENSUS VECTORS ###############################################

census_vectors_housing <-
  tibble::tibble(
    var_code = "housing_tenant",
    type = list("pct"),
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Housing",
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
    theme = "Income",
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
    theme = "Income",
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
    theme = "Income",
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
    theme = "Income",
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
    theme = "Income",
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


## IMPORT IDENTITY CENSUS VECTORS ##############################################

census_vectors_identity <-
  tibble::tibble(
    var_code = "iden_imm",
    type = list("pct"),
    theme = "Identity",
    vec_2016 = list("v_CA16_3411"),
    vec_2011 = list("v_CA11N_22"),
    vec_2006 = list("v_CA06_478"),
    vec_2001 = list("v_CA01_406"),
    vec_1996 = list("v_CA1996_128"),
    parent_vectors = list(NA),
    var_title = "Immigrants (%)",
    var_short = "Immigrants",
    explanation = "the percentage of residents who are foreign-born") |>
  tibble::add_row(
    var_code = "iden_imm_new",
    type = list("pct"),
    theme = "Identity",
    vec_2016 = list("v_CA16_3432"),
    vec_2011 = list("v_CA11N_43"),
    vec_2006 = list("v_CA06_553"),
    vec_2001 = list("v_CA01_507"),
    vec_1996 = list("v_CA1996_228"),
    parent_vectors = list(c("v_CA1996_125", "v_CA01_402", "v_CA06_474",
                          "v_CA11N_16", "v_CA16_3405")),
    var_title = "New immigrants (%)",
    var_short = "New immigrants",
    explanation = paste0("the percentage of people who have immigrated in ",
                         "the last five years")) |>
  tibble::add_row(
    var_code = "iden_vm",
    type = list("pct"),
    theme = "Identity",
    vec_2016 = list("v_CA16_3957"),
    vec_2011 = list("v_CA11N_460"),
    vec_2006 = list("v_CA06_1303"),
    vec_2001 = list("v_CA01_703"),
    vec_1996 = list("v_CA1996_784"),
    parent_vectors = list(NA),
    var_title = "Visible minorities (%)",
    var_short = "Vis. minorities",
    explanation = paste0("the percentage of people who identify as part of ",
                         "one or more visible minority groups")) |>
  tibble::add_row(
    var_code = "iden_aboriginal",
    type = list("pct"),
    theme = "Identity",
    vec_2016 = list("v_CA16_3855"),
    vec_2011 = list("v_CA11N_1354"),
    vec_2006 = list("v_CA06_565"),
    vec_2001 = list("v_CA01_718"),
    vec_1996 = list("v_CA1996_473"),
    parent_vectors = list(NA),
    var_title = "Aboriginal (%)",
    var_short = "Aboriginal",
    explanation = "the percentage of people who are of aboriginal identity")

usethis::use_data(census_vectors_identity, overwrite = TRUE)


## IMPORT TRANSPORT CENSUS VECTORS #############################################

census_vectors_transport <-
  tibble::tibble(
    var_code = "trans_car",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list(c("v_CA16_5795", "v_CA16_5798")),
    vec_2011 = list(c("v_CA11N_2194", "v_CA11N_2197")),
    vec_2006 = list(c("v_CA06_1101", "v_CA06_1102")),
    vec_2001 = list(c("v_CA01_1255", "v_CA01_1256", "v_CA01_1264", "v_CA01_1265")),
    vec_1996 = list(c("v_CA1996_1326", "v_CA1996_1327", "v_CA1996_1335",
                 "v_CA1996_1336")),
    parent_vectors = list(c("v_CA01_1253", "v_CA1996_1324")),
    var_title = "Drive to work (%)",
    var_short = "Drive",
    explanation = paste0("the percentage of people who drive a privately ",
                         "owned car or truck to work")) |>
  tibble::add_row(
    var_code = "trans_walk_or_bike",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list(c("v_CA16_5804", "v_CA16_5807")),
    vec_2011 = list(c("v_CA11N_2203", "v_CA11N_2206")),
    vec_2006 = list(c("v_CA06_1104", "v_CA06_1105")),
    vec_2001 = list(c("v_CA01_1258", "v_CA01_1259", "v_CA01_1267", "v_CA01_1268")),
    vec_1996 = list(c("v_CA1996_1329", "v_CA1996_1330", "v_CA1996_1338",
                 "v_CA1996_1339")),
    parent_vectors = list(c("v_CA01_1253", "v_CA1996_1324")),
    var_title = "Walk or cycle to work (%)",
    var_short = "Walk or cycle",
    explanation = "the percentage of people who walk or cycle to work") |>
  tibble::add_row(
    var_code = "trans_transit",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list("v_CA16_5801"),
    vec_2011 = list("v_CA11N_2200"),
    vec_2006 = list("v_CA06_1103"),
    vec_2001 = list(c("v_CA01_1266", "v_CA01_1257")),
    vec_1996 = list(c("v_CA1996_1337", "v_CA1996_1328")),
    parent_vectors = list(c("v_CA01_1253", "v_CA1996_1324")),
    var_title = "Public transit to work (%)",
    var_short = "Transit",
    explanation = paste0("the percentage of people who use public transit ",
                         "to get to work")) |>
  tibble::add_row(
    var_code = "trans_t_15",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list("v_CA16_5816"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Commute under 15 minutes (%)",
    var_short = "Commute <15m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "less than 15 minutes")) |>
  tibble::add_row(
    var_code = "trans_t_45",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list(c("v_CA16_5819", "v_CA16_5822")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Commute 15-45 minutes (%)",
    var_short = "Commute 14-45m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "between 15 and 45 minutes")) |>
  tibble::add_row(
    var_code = "trans_t_45_plus",
    type = list("pct"),
    theme = "Transport",
    vec_2016 = list(c("v_CA16_5825", "v_CA16_5828")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Commute more than 45 minutes (%)",
    var_short = "Commute >45m",
    explanation = paste0("the percentage of people whose commute time is ",
                         "longer than 45 minutes"))

usethis::use_data(census_vectors_transport, overwrite = TRUE)


## IMPORT EMPLOYMENT CENSUS VECTORS ############################################

census_vectors_employment <-
  tibble::tibble(
    var_code = "emp_professional",
    type = list("pct"),
    theme = "Employment",
    vec_2016 = list(c("v_CA16_5735", "v_CA16_5738")),
    vec_2011 = list(c("v_CA11N_2107", "v_CA11N_2110")),
    vec_2006 = list(c("v_CA06_1021", "v_CA06_1022")),
    vec_2001 = list(c("v_CA01_1181", "v_CA01_1182")),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Managerial and professional occupations (%)",
    var_short = "Professional",
    explanation = paste0("the percentage of the workforce in professional and ",
                         "managerial occupations, based on the North American ",
                         "Industry Classification System")) |>
  tibble::add_row(
    var_code = "emp_creative",
    type = list("pct"),
    theme = "Employment",
    vec_2016 = list(c("v_CA16_5726", "v_CA16_5750")),
    vec_2011 = list(c("v_CA11N_2098", "v_CA11N_2122")),
    vec_2006 = list(c("v_CA06_1018", "v_CA06_1026")),
    vec_2001 = list(c("v_CA01_1178", "v_CA01_1186")),
    vec_1996 = list(NA),
    parent_vectors = list(NA),
    var_title = "Creative occupations (%)",
    var_short = "Creative",
    explanation = paste0("the percentage of the workforce in artistic and ",
                         "cultural occupations, based on the North American ",
                         "Industry Classification System"))

usethis::use_data(census_vectors_employment, overwrite = TRUE)


## IMPORT FAMILY CENSUS VECTORS ############################################

census_vectors_family <-
  tibble::tibble(
    var_code = "family_children",
    type = list("pct"),
    theme = "Family",
    vec_2016 = list("v_CA16_507"),
    vec_2011 = list(c("v_CA11F_129", "v_CA11F_119", "v_CA11F_125")),
    vec_2006 = list(c("v_CA06_65", "v_CA06_59", "v_CA06_69")),
    vec_2001 = list(c("v_CA01_63", "v_CA01_57", "v_CA01_67")),
    vec_1996 = list(NA),
    parent_vectors = list(c("v_CA11F_115", "v_CA06_55", "v_CA01_53",
                          "v_CA16_504")),
    var_title = "Families with children (%)",
    var_short = "With child",
    explanation = "the percentage of census families with children out of total households") |>
  tibble::add_row(
    var_code = "family_one_person",
    type = list("pct"),
    theme = "Family",
    vec_2016 = list("v_CA16_510"),
    vec_2011 = list("v_CA11F_157"),
    vec_2006 = list("v_CA06_89"),
    vec_2001 = list("v_CA01_87"),
    vec_1996 = list("v_CA1996_98"),
    parent_vectors = list("v_CA16_504"),
    var_title = "Living alone (%)",
    var_short = "Living alone",
    explanation = "the percentage of one person households out of total households")

usethis::use_data(census_vectors_family, overwrite = TRUE)


## IMPORT LANGUAGE CENSUS VECTORS ##############################################

census_vectors_language <-
  tibble::tibble(
    var_code = "lang_french_only",
    type = list("pct"),
    theme = "Language",
    vec_2016 = "v_CA16_518",
    vec_2011 = "v_CA11F_557",
    vec_2006 = "v_CA06_245",
    vec_2001 = "v_CA01_215",
    vec_1996 = "v_CA1996_312",
    parent_vectors = list(NA),
    var_title = "French only (%)",
    var_short = "Fr. only",
    explanation = paste0("the percentage of individuals that only know French ",
                         "as an official language")) |>
  tibble::add_row(
    var_code = "lang_eng_only",
    type = list("pct"),
    theme = "Language",
    vec_2016 = "v_CA16_515",
    vec_2011 = "v_CA11F_554",
    vec_2006 = "v_CA06_244",
    vec_2001 = "v_CA01_214",
    vec_1996 = "v_CA1996_311",
    parent_vectors = list(NA),
    var_title = "English only (%)",
    var_short = "Eng. only",
    explanation = paste0("the percentage of individuals that only know English ",
                         "as an official language")) |>
  tibble::add_row(
    var_code = "lang_french_eng",
    type = list("pct"),
    theme = "Language",
    vec_2016 = "v_CA16_521",
    vec_2011 = "v_CA11F_560",
    vec_2006 = "v_CA06_246",
    vec_2001 = "v_CA01_216",
    vec_1996 = "v_CA1996_313",
    parent_vectors = list(NA),
    var_title = "French and English (%)",
    var_short = "Fr. and Eng.",
    explanation = paste0("the percentage of individuals that know both official ",
                         "languages (French and English)")) |>
  tibble::add_row(
    var_code = "lang_no_official",
    type = list("pct"),
    theme = "Language",
    vec_2016 = "v_CA16_524",
    vec_2011 = "v_CA11F_563",
    vec_2006 = "v_CA06_247",
    vec_2001 = "v_CA01_217",
    vec_1996 = "v_CA1996_314",
    parent_vectors = list(NA),
    var_title = "Neither French nor English (%)",
    var_short = "Non-official",
    explanation = paste0("the percentage of individuals that do not know either ",
                         "of the official languages (French or English)"))

usethis::use_data(census_vectors_language, overwrite = TRUE)


## IMPORT AGE CENSUS VECTORS ###################################################

census_vectors_age <-
  tibble::tibble(
    var_code = "age_0_14",
    type = list("pct"),
    theme = "Age",
    vec_2016 = list("v_CA16_4"),
    vec_2011 = list(c("v_CA11F_8", "v_CA11F_11", "v_CA11F_14")),
    vec_2006 = list(c("v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_23",
                      "v_CA06_24", "v_CA06_25")),
    vec_2001 = list(c("v_CA01_7", "v_CA01_8", "v_CA01_9", "v_CA01_26",
                      "v_CA01_27", "v_CA01_28")),
    vec_1996 = list(c("v_CA1996_7", "v_CA1996_31", "v_CA1996_8", "v_CA1996_32",
                      "v_CA1996_9", "v_CA1996_33")),
    parent_vectors = list(c("v_CA06_3", "v_CA06_22", "v_CA01_6", "v_CA01_25",
                            "v_CA1996_6", "v_CA1996_30")),
    var_title = "Aged between 0 and 14 (%)",
    var_short = "0-14 yo",
    explanation = "the percentage of the population aged between 0 and 14 years old") |>
  tibble::add_row(
    var_code = "age_15_64",
    type = list("pct"),
    theme = "Age",
    vec_2016 = list("v_CA16_61"),
    vec_2011 = list(paste0("v_CA11F_", c(17,35,38,41,44,47,50,53,56,59))),
    vec_2006 = list(c(paste0("v_CA06_", 7:16), paste0("v_CA06_", 26:35))),
    vec_2001 = list(c(paste0("v_CA01_", 10:19), paste0("v_CA01_", 29:38))),
    vec_1996 = list(c(paste0("v_CA1996_", 15:24), paste0("v_CA1996_", 39:48))),
    parent_vectors = list(c("v_CA06_3", "v_CA06_22", "v_CA01_6", "v_CA01_25",
                            "v_CA1996_6", "v_CA1996_30")),
    var_title = "Aged between 15 and 64 (%)",
    var_short = "15-64 yo",
    explanation = "the percentage of the population aged between 15 and 64 years old") |>
  tibble::add_row(
    var_code = "age_65_plus",
    type = list("pct"),
    theme = "Age",
    vec_2016 = list("v_CA16_244"),
    vec_2011 = list(c(paste0("v_CA11F_", c(62,65,68,71,74)))),
    vec_2006 = list(c(paste0("v_CA06_", 17:21), paste0("v_CA06_", 36:40))),
    vec_2001 = list(c(paste0("v_CA01_", 20:24), paste0("v_CA01_", 39:43))),
    vec_1996 = list(c(paste0("v_CA1996_", 25:29), paste0("v_CA1996_", 49:53))),
    parent_vectors = list(c("v_CA06_3", "v_CA06_22", "v_CA01_6", "v_CA01_25",
                            "v_CA1996_6", "v_CA1996_30")),
    var_title = "Aged 65 and above (%)",
    var_short = "65+ yo",
    explanation = "the percentage of the population aged 65 and above")

usethis::use_data(census_vectors_age, overwrite = TRUE)


## IMPORT EDUCATION CENSUS VECTORS #############################################

census_vectors_education <-
  tibble::tibble(
    var_code = "edu_bachelor_above",
    type = list("pct"),
    theme = "Education",
    vec_2016 = list("v_CA16_5078"),
    vec_2011 = list("v_CA11N_1792"),
    vec_2006 = list(c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268")),
    vec_2001 = list("v_CA01_1397"),
    vec_1996 = list("v_CA1996_1360"),
    parent_vectors = list(c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262",
                            "v_CA1996_1347", "v_CA01_1384", "v_CA11N_1771",
                            "v_CA16_5051")),
    var_title = "Bachelor and above (%)",
    var_short = "Bachelor+",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "holding a degree at bachelor level or above")) |>
  tibble::add_row(
    var_code = "edu_no_degree",
    type = list("pct"),
    theme = "Education",
    vec_2016 = list("v_CA16_5054"),
    vec_2011 = list("v_CA11N_1774"),
    vec_2006 = list(c("v_CA06_1235", "v_CA06_1249", "v_CA06_1263")),
    vec_2001 = list("v_CA01_1387"),
    vec_1996 = list("v_CA1996_1350"),
    parent_vectors = list(c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262",
                            "v_CA1996_1347", "v_CA01_1384", "v_CA11N_1771",
                            "v_CA16_5051")),
    var_title = "No certificate, diploma or degree (%)",
    var_short = "No degree",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "with no certificate, diploma or degree"))

usethis::use_data(census_vectors_education, overwrite = TRUE)


## COMBINE ALL CENSUS VECTORS ##################################################

census_vectors <- rbind(census_vectors_housing, census_vectors_income,
                        census_vectors_identity, census_vectors_transport,
                        census_vectors_employment, census_vectors_family,
                        census_vectors_language, census_vectors_age,
                        census_vectors_education)

usethis::use_data(census_vectors, overwrite = TRUE)
