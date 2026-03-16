verify_parents <- function(vectors_df, parents_df) {
  parents_type <- unique(vapply(
    parents_df$type,
    function(x) x$unit,
    character(1)
  ))
  if (length(parents_type) > 1 | parents_type != "count") {
    stop("All vectors of `parents_df` must have `count` as type.")
  }

  if (sum(vectors_df$parent) > 0) {
    stop(
      "All vectors of `vectors_df` must be non-parent vectors. `parent` == F"
    )
  }

  if (sum(parents_df$parent) != nrow(parents_df)) {
    stop("All vectors of `parents_df` must be parent vectors. `parent` == T")
  }

  # Get all non-NA parent vectors and check if they're in the parents_df
  # MODIFIÉ: unlist pour supporter multi-parents
  non_na_parents <- unique(unlist(vectors_df$parent_vec))
  non_na_parents <- non_na_parents[!is.na(non_na_parents)]
  if (!all(non_na_parents %in% parents_df$var_code)) {
    missing_parents <- non_na_parents[!non_na_parents %in% parents_df$var_code]
    stop(paste0("Parent vector `", missing_parents, "` is missing. \n"))
  }

  vec_cols <- names(vectors_df)[grepl("vec_", names(vectors_df))]
  row_indices <- seq_len(nrow(vectors_df))

  lapply(row_indices, function(row_index) {
    row_data <- vectors_df[row_index, ]

    # MODIFIÉ: unlist pour supporter multi-parents
    parent_vecs <- unlist(row_data$parent_vec)
    parent_vecs <- parent_vecs[!is.na(parent_vecs)]
    if (length(parent_vecs) == 0) {
      stop(
        "All non-parent entries need to have a parent vector for normalization."
      )
    }

    var_vectors <- sapply(
      vec_cols,
      function(col_name) {
        vectors <- unlist(row_data[[col_name]])
        if (all(is.na(vectors))) {
          return(NULL)
        }
        vectors
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    existing_vars <- names(var_vectors[!sapply(var_vectors, is.null)])

    # MODIFIÉ: union de tous les parents disponibles
    existing_parents <- Reduce(
      union,
      lapply(parent_vecs, function(pv) {
        parent_data <- parents_df[parents_df$var_code == pv, ]
        parent_vectors <- sapply(
          vec_cols,
          function(col_name) {
            vectors <- unlist(parent_data[[col_name]])
            if (all(is.na(vectors))) {
              return(NULL)
            }
            vectors
          },
          simplify = FALSE,
          USE.NAMES = TRUE
        )
        names(parent_vectors[!sapply(parent_vectors, is.null)])
      })
    )

    if (!all(existing_vars %in% existing_parents)) {
      var_code <- row_data$var_code
      missing_parents <- existing_vars[!existing_vars %in% existing_parents]
      stop(paste0(
        "`",
        var_code,
        "` is missing a parent variable for `",
        missing_parents,
        "`. \n"
      ))
    }
  })

  return(invisible(NULL))
}

census_vectors_housing <-
  tibble::tibble(
    var_code = "housing_tenant",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4239"),
    vec_2016 = list("v_CA16_4838"),
    vec_2011 = list("v_CA11N_2254"),
    vec_2006 = list("v_CA06_103"),
    vec_2001 = list("v_CA01_100"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = list(list(
      en = "Tenant-occupied",
      fr = "Occupés par des locataires"
    )),
    var_short = list(list(
      en = "Tenant",
      fr = "Locataire"
    )),
    description = list(list(
      en = "The number of private households where no member of the household owns their dwelling. The dwelling is considered to be rented even if no cash rent is paid",
      fr = "Le nombre des ménages privés dont aucun membre n'est propriétaire du logement. Le logement est considéré comme étant loué même si aucun loyer en argent n'est versé"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_owner",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4238"),
    vec_2016 = list("v_CA16_4837"),
    vec_2011 = list("v_CA11N_2253"),
    vec_2006 = list("v_CA06_102"),
    vec_2001 = list("v_CA01_99"),
    vec_1996 = list("v_CA1996_1682"),
    var_title = list(list(
      en = "Owner-occupied",
      fr = "Occupés par des propriétaires"
    )),
    var_short = list(list(
      en = "Owner",
      fr = "Propriétaire"
    )),
    description = list(list(
      en = "The number of private households where one or more members of the household own their dwelling.",
      fr = "Le nombre de ménages privés dont un ou plusieurs membres sont propriétaires du logement."
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_shelcost_tenant",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4318"),
    vec_2016 = list("v_CA16_4901"),
    vec_2011 = list("v_CA11N_2292"),
    vec_2006 = list("v_CA06_2050"),
    vec_2001 = list("v_CA01_1667"),
    vec_1996 = list("v_CA1996_1701"),
    var_title = list(list(
      en = "Average shelter cost for tenant households",
      fr = "Frais de logement moyens des ménages locataires"
    )),
    var_short = list(list(
      en = "Shelter tenant",
      fr = "Frais log. loc"
    )),
    description = list(list(
      en = "The average monthly shelter expenses paid by tenant households, including, where applicable, rent plus costs of electricity, heat, water and municipal services",
      fr = "Les frais mensuels moyens de logement payés par les ménages locataires, incluant, s'il y a lieu, le loyer ainsi que les coûts d'électricité, de chauffage, d'eau et des services municipaux"
    )),
    parent_vec = "tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_shelcost_owner",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4310"),
    vec_2016 = list("v_CA16_4894"),
    vec_2011 = list("v_CA11N_2285"),
    vec_2006 = list("v_CA06_2055"),
    vec_2001 = list("v_CA01_1671"),
    vec_1996 = list("v_CA1996_1704"),
    var_title = list(list(
      en = "Average shelter cost for owner households",
      fr = "Frais de logement moyens des ménages propriétaires"
    )),
    var_short = list(list(
      en = "Shelter owner",
      fr = "Frais log. prop"
    )),
    description = list(list(
      en = "The average monthly shelter expenses paid by owner households, including, where applicable, mortgage payments, property taxes, condominium fees, and costs of electricity, heat and water",
      fr = "Les frais mensuels moyens de logement payés par les ménages propriétaires, incluant, s'il y a lieu, les versements hypothécaires, les taxes foncières, les frais de copropriété ainsi que les coûts d'électricité, de chauffage et d'eau"
    )),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_repairs",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4274"),
    vec_2016 = list("v_CA16_4872"),
    vec_2011 = list("v_CA11N_2232"),
    vec_2006 = list("v_CA06_108"),
    vec_2001 = list("v_CA01_104"),
    vec_1996 = list("v_CA1996_1687"),
    var_title = list(list(
      en = "Housing requiring major repairs",
      fr = "Logements nécessitant des réparations importantes"
    )),
    var_short = list(list(
      en = "Repairs",
      fr = "Réparations"
    )),
    description = list(list(
      en = "The number of occupied private dwellings that require major repairs, including dwellings with defective plumbing or electrical wiring, or structural repairs to walls, floors or ceilings",
      fr = "Le nombre de logements privés occupés nécessitant des réparations majeures, y compris les logements où la plomberie ou l'installation électrique est défectueuse, ou qui ont besoin de réparations à la charpente des murs, des planchers ou des plafonds"
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_value",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4312"),
    vec_2016 = list("v_CA16_4896"),
    vec_2011 = list("v_CA11N_2287"),
    vec_2006 = list("v_CA06_2054"),
    vec_2001 = list("v_CA01_1674"),
    vec_1996 = list("v_CA1996_1681"),
    var_title = list(list(
      en = "Average property value",
      fr = "Valeur moyenne des propriétés"
    )),
    var_short = list(list(
      en = "Avg. value",
      fr = "Valeur moyenne"
    )),
    description = list(list(
      en = "The average owner-estimated market value of owner-occupied private dwellings, including the value of the entire dwelling, the land it is on, and any other structures on the property (such as a garage)",
      fr = "Valeur marchande moyenne estimée par le propriétaire pour les logements privés occupés par leur propriétaire, incluant la valeur de l'ensemble du logement, du terrain sur lequel il se trouve et de toute autre construction sur la propriété"
    )),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_unafford",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4290"),
    vec_2016 = list("v_CA16_4888"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Unaffordable housing",
      fr = "Logement inabordable"
    )),
    var_short = list(list(
      en = "Unaffordable",
      fr = "Inabordable"
    )),
    description = list(list(
      en = "The number of private households whose shelter costs are equal to 30% or more of their total before-tax household income",
      fr = "Le nombre de ménages privés dont les frais de logement correspondent à 30 % ou plus de leur revenu total avant impôt"
    )),
    parent_vec = "owner_tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_unsuit",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4262"),
    vec_2016 = list("v_CA16_4861"),
    vec_2011 = list("v_CA11N_2276"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Unsuitable housing",
      fr = "Logement inadapté"
    )),
    var_short = list(list(
      en = "Unsuitable",
      fr = "Inadéquat"
    )),
    description = list(list(
      en = "The number of private households living in unsuitable housing, that is, in dwellings that do not have enough bedrooms for the size and composition of the household according to the National Occupancy Standard (NOS)",
      fr = "Nombre de ménages privés vivant dans un logement de taille non convenable, c'est-à-dire dans des logements qui ne comptent pas suffisamment de chambres pour la taille et la composition du ménage, selon la Norme nationale d'occupation (NNO)"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_stress_renter",
    type = list(list(
      unit = NULL,
      aggregation_field = "pct",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4315"),
    vec_2016 = list("v_CA16_4899"),
    vec_2011 = list("v_CA11N_2290"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Renter housing stress",
      fr = "Difficultés financières lié au logement des locataires"
    )),
    var_short = list(list(
      en = "Renter stress",
      fr = "Stress loc."
    )),
    description = list(list(
      en = "The number of tenant households whose shelter costs are equal to 30% or more of their total before-tax household income",
      fr = "Le nombre de ménages locataires dont les frais de logement correspondent à 30 % ou plus de leur revenu total avant impôt"
    )),
    parent_vec = "tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_stress_owner",
    type = list(list(
      unit = NULL,
      aggregation_field = "pct",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4307"),
    vec_2016 = list("v_CA16_4892"),
    vec_2011 = list("v_CA11N_2283"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Owner housing stress",
      fr = "Difficultés financières lié au logement des propriétaires"
    )),
    var_short = list(list(
      en = "Owner stress",
      fr = "Stress prop."
    )),
    description = list(list(
      en = "The number of owner households whose shelter costs are equal to 30% or more of their total before-tax household income",
      fr = "Le nombre de ménages propriétaires dont les frais de logement correspondent à 30 % ou plus de leur revenu total avant impôt"
    )),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_one",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5751"),
    vec_2016 = list("v_CA16_6698"),
    vec_2011 = list("v_CA11N_1723"),
    vec_2006 = list("v_CA06_453"),
    vec_2001 = list("v_CA01_383"),
    vec_1996 = list("v_CA1996_1387"),
    var_title = list(list(
      en = "One-year housing mobility",
      fr = "Mobilité du logement sur un an"
    )),
    var_short = list(list(
      en = "1-year mob.",
      fr = "Mob. 1 an"
    )),
    description = list(list(
      en = "The number of persons who moved in the past year, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours de l'année précédente, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_1",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_one_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5752"),
    vec_2016 = list("v_CA16_6699"),
    vec_2011 = list("v_CA11N_1724"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "One-year housing mobility - Male",
      fr = "Mobilité du logement sur un an - Homme"
    )),
    var_short = list(list(
      en = "1-year mob. M",
      fr = "Mob. 1 an H"
    )),
    description = list(list(
      en = "The number of males who moved in the past year, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre d'hommes ayant déménagé au cours de l'année précédente, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_1_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_one_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5753"),
    vec_2016 = list("v_CA16_6700"),
    vec_2011 = list("v_CA11N_1725"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "One-year housing mobility - Female",
      fr = "Mobilité du logement sur un an - Femmes"
    )),
    var_short = list(list(
      en = "1-year mob. F",
      fr = "Mob. 1 an F"
    )),
    description = list(list(
      en = "The number of females who moved in the past year, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de femmes ayant déménagé au cours de l'année précédente, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_1_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_five",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5778"),
    vec_2016 = list("v_CA16_6725"),
    vec_2011 = list("v_CA11N_1750"),
    vec_2006 = list("v_CA06_462"),
    vec_2001 = list("v_CA01_392"),
    vec_1996 = list("v_CA1996_1396"),
    var_title = list(list(
      en = "Five-year housing mobility",
      fr = "Mobilité du logement sur cinq ans"
    )),
    var_short = list(list(
      en = "5-year mob.",
      fr = "Mob. 5 ans"
    )),
    description = list(list(
      en = "The number of persons who moved in the past 5 years, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours des 5 dernières années, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_5",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_five_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5779"),
    vec_2016 = list("v_CA16_6726"),
    vec_2011 = list("v_CA11N_1751"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Five-year housing mobility",
      fr = "Mobilité du logement sur cinq ans"
    )),
    var_short = list(list(
      en = "5-year mob.",
      fr = "Mob. 5 ans"
    )),
    description = list(list(
      en = "The number of persons who moved in the past 5 years, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours des 5 dernières années, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_5_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_five_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_5780"),
    vec_2016 = list("v_CA16_6727"),
    vec_2011 = list("v_CA11N_1752"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Five-year housing mobility",
      fr = "Mobilité du logement sur cinq ans"
    )),
    var_short = list(list(
      en = "5-year mob.",
      fr = "Mob. 5 ans"
    )),
    description = list(list(
      en = "The number of persons who moved in the past 5 years, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours des 5 dernières années, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_5_f",
    parent = FALSE
  )

census_vectors_housing_parent <-
  tibble::tibble(
    var_code = "private_households",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4237"),
    vec_2016 = list("v_CA16_4836"),
    vec_2011 = list("v_CA11N_2252"),
    vec_2006 = list("v_CA06_136"),
    vec_2001 = list("v_CA01_129"),
    vec_1996 = list("v_CA1996_1694"),
    var_title = list(list(
      en = "Households",
      fr = "Ménages"
    )),
    var_short = list(list(
      en = "Households",
      fr = "Ménages"
    )),
    description = list(list(
      en = "The number of households, defined as a person or group of persons who occupy the same dwelling and do not have a usual place of residence elsewhere in Canada or abroad",
      fr = "Le nombre de ménages, définis comme une personne ou un groupe de personnes qui occupent le même logement et n'ont pas de domicile habituel ailleurs au Canada ou à l'étranger"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "private_dwellings",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4272"),
    vec_2016 = list("v_CA16_4870"),
    vec_2011 = list("v_CA11N_2230"),
    vec_2006 = list("v_CA06_105"),
    vec_2001 = list("v_CA01_96"),
    vec_1996 = list("v_CA1996_1678"),
    var_title = list(list(
      en = "Dwellings",
      fr = "Logements"
    )),
    var_short = list(list(
      en = "Dwellings",
      fr = "Logements"
    )),
    description = list(list(
      en = "The number of private dwellings, defined as a separate set of living quarters with a private entrance from outside the building or from a common area inside, that meet the conditions for year-round occupancy (a source of heat or power and an enclosed space providing shelter from the elements)",
      fr = "Le nombre de logements privés, définis comme un ensemble séparé de pièces d'habitation possédant une entrée privée depuis l'extérieur de l'immeuble ou à partir d'un espace commun intérieur, qui répondent aux conditions les rendant propres à l'habitation durant toute l'année (source de chauffage ou d'énergie et espace clos permettant de s'abriter des intempéries)"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_tenant_households",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4288"),
    vec_2016 = list("v_CA16_4886"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Owner and tenant households",
      fr = "Ménages propriétaires et locataires"
    )),
    var_short = list(list(
      en = "Households",
      fr = "Ménages"
    )),
    description = list(list(
      en = "The number of owner and tenant households, defined as a person or group of persons who occupy the same dwelling and do not have a usual place of residence elsewhere in Canada or abroad",
      fr = "Le nombre de ménages propriétaires et locataires, définis comme une personne ou un groupe de personnes qui occupent le même logement et n'ont pas de domicile habituel ailleurs au Canada ou à l'étranger"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "tenant_households",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing",
    vec_2021 = list("v_CA21_4313"),
    vec_2016 = list("v_CA16_4897"),
    vec_2011 = list("v_CA11N_2288"),
    vec_2006 = list("v_CA06_2049"),
    vec_2001 = list("v_CA01_1666"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = list(list(
      en = "Tenant households",
      fr = "Ménages locataires"
    )),
    var_short = list(list(
      en = "Tenant",
      fr = "Locataire"
    )),
    description = list(list(
      en = "The number of private households where no member of the household owns their dwelling. The dwelling is considered to be rented even if no cash rent is paid",
      fr = "Le nombre des ménages privés dont aucun membre n'est propriétaire du logement. Le logement est considéré comme étant loué même si aucun loyer en argent n'est versé"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_households",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_4305"),
    vec_2016 = list("v_CA16_4890"),
    vec_2011 = list("v_CA11N_2281"),
    vec_2006 = list("v_CA06_2053"),
    vec_2001 = list("v_CA01_1670"),
    vec_1996 = list("v_CA1996_1682"),
    var_title = list(list(
      en = "Owner households",
      fr = "Ménages propriétaires"
    )),
    var_short = list(list(
      en = "Owner",
      fr = "Propriétaire"
    )),
    description = list(list(
      en = "The number of owner households, defined as private households where at least one member of the household owns the dwelling, even if it is still being paid for",
      fr = "Le nombre de ménages propriétaires, définis comme des ménages privés au sein desquels au moins un des membres du ménage est propriétaire du logement, même s'il est encore en train de le payer"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_1",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5745"),
    vec_2016 = list("v_CA16_6692"),
    vec_2011 = list("v_CA11N_1717"),
    vec_2006 = list("v_CA06_451"),
    vec_2001 = list("v_CA01_381"),
    vec_1996 = list("v_CA1996_1385"),
    var_title = list(list(
      en = "Residents",
      fr = "Résidents"
    )),
    var_short = list(list(
      en = "Residents",
      fr = "Résidents"
    )),
    description = list(list(
      en = "The total number of residents one year prior",
      fr = "Le nombre total de résidents un an auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_1_m",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5746"),
    vec_2016 = list("v_CA16_6693"),
    vec_2011 = list("v_CA11N_1718"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Residents - Male",
      fr = "Résidents - Hommes"
    )),
    var_short = list(list(
      en = "Residents M",
      fr = "Résidents H"
    )),
    description = list(list(
      en = "The total number of male residents one year prior",
      fr = "Le nombre total de résidents hommes un an auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_1_f",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5747"),
    vec_2016 = list("v_CA16_6694"),
    vec_2011 = list("v_CA11N_1719"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Residents - Female",
      fr = "Résidents - Femmes"
    )),
    var_short = list(list(
      en = "Residents F",
      fr = "Résidents F"
    )),
    description = list(list(
      en = "The total number of female residents one year prior",
      fr = "Le nombre total de résidents femmes un an auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_5",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5772"),
    vec_2016 = list("v_CA16_6719"),
    vec_2011 = list("v_CA11N_1744"),
    vec_2006 = list("v_CA06_460"),
    vec_2001 = list("v_CA01_390"),
    vec_1996 = list("v_CA1996_1394"),
    var_title = list(list(
      en = "Residents",
      fr = "Résidents"
    )),
    var_short = list(list(
      en = "Residents",
      fr = "Résidents"
    )),
    description = list(list(
      en = "The total number of residents five years prior",
      fr = "Le nombre total de résidents cinq ans auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_5_m",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5773"),
    vec_2016 = list("v_CA16_6720"),
    vec_2011 = list("v_CA11N_1745"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Residents - Male",
      fr = "Résidents - Hommes"
    )),
    var_short = list(list(
      en = "Residents M",
      fr = "Résidents H"
    )),
    description = list(list(
      en = "The total number of male residents five years prior",
      fr = "Le nombre total de résidents hommes cinq ans auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_5_f",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Housing",
    vec_2021 = list("v_CA21_5774"),
    vec_2016 = list("v_CA16_6721"),
    vec_2011 = list("v_CA11N_1746"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Residents - Female",
      fr = "Résidents - Femmes"
    )),
    var_short = list(list(
      en = "Residents F",
      fr = "Résidents F"
    )),
    description = list(list(
      en = "The total number of female residents five years prior",
      fr = "Le nombre total de résidents femmes cinq ans auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(
  vectors_df = census_vectors_housing,
  parents_df = census_vectors_housing_parent
)

census_vectors_housing <- rbind(
  census_vectors_housing,
  census_vectors_housing_parent
)

usethis::use_data(census_vectors_housing, overwrite = TRUE)


## IMPORT HOUSING TYPOLOGY #####################################################

census_vectors_typology <-
  tibble::tibble(
    var_code = "typology_single_detached",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_435"),
    vec_2016 = list("v_CA16_409"),
    vec_2011 = list("v_CA11F_200"),
    vec_2006 = list("v_CA06_120"),
    vec_2001 = list("v_CA01_113"),
    vec_1996 = list("v_CA1996_108"),
    var_title = list(list(
      en = "Single-detached",
      fr = "Maisons individuelles"
    )),
    var_short = list(list(
      en = "Single-detached",
      fr = "Maison ind."
    )),
    description = list(list(
      en = "The number of private dwellings that are single-detached houses, defined as single dwellings not attached to any other dwelling or structure (except their own garage or shed), surrounded by open space on all sides and with no dwellings above or below. Mobile homes permanently fixed to a foundation are also included in this category.",
      fr = "Le nombre de logements privés qui sont des maisons individuelles non attenantes, définies comme des logements individuels non joints à un autre logement ou à une autre construction (sauf à leur propre garage ou hangar), entourés d'espaces libres sur tous les côtés et sans logement au-dessus ni au-dessous. Les habitations mobiles installées de façon permanente sur des fondations sont également incluses dans cette catégorie."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_semi_detached",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_436"),
    vec_2016 = list("v_CA16_412"),
    vec_2011 = list("v_CA11F_204"),
    vec_2006 = list("v_CA06_121"),
    vec_2001 = list("v_CA01_114"),
    vec_1996 = list("v_CA1996_109"),
    var_title = list(list(
      en = "Semi-detached",
      fr = "Semi-détaché"
    )),
    var_short = list(list(
      en = "Semi-detached",
      fr = "Semi-détaché"
    )),
    description = list(list(
      en = "The number of private dwellings that are semi-detached houses, defined as one of two dwellings attached side by side (or back to back) to each other, but not attached to any other dwelling or structure (except their own garage or shed), with no dwellings above or below and open space around the pair.",
      fr = "Le nombre de logements privés qui sont des maisons jumelées, définies comme l'un de deux logements réunis côte à côte (ou de l'arrière à l'arrière) par un mur commun, mais non joints à aucun autre logement ou construction (sauf à leur propre garage ou hangar), sans logement au-dessus ou au-dessous et entourés d'espaces libres."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_row_house",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_437"),
    vec_2016 = list("v_CA16_413"),
    vec_2011 = list("v_CA11F_205"),
    vec_2006 = list("v_CA06_122"),
    vec_2001 = list("v_CA01_115"),
    vec_1996 = list("v_CA1996_110"),
    var_title = list(list(
      en = "Row houses",
      fr = "Maisons en rangée"
    )),
    var_short = list(list(
      en = "Row houses",
      fr = "Rangées"
    )),
    description = list(list(
      en = "The number of private dwellings that are row houses, defined as dwellings in a row of at least three dwellings joined side by side (or occasionally side to back), such as townhouses or garden homes, with no dwellings above or below; townhouses attached to a high-rise building are also included in this category.",
      fr = "Le nombre de logements privés qui sont des maisons en rangée, définies comme des logements dans une rangée d'au moins trois logements réunis côte à côte (ou parfois réunis par un des côtés d'un logement et l'arrière d'un autre logement), comme une maison en bande ou une maison-jardin, sans logement au-dessus ni au-dessous; les maisons en bande jointes à une tour d'habitation sont également incluses dans cette catégorie."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_duplex",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_438"),
    vec_2016 = list("v_CA16_414"),
    vec_2011 = list("v_CA11F_206"),
    vec_2006 = list("v_CA06_123"),
    vec_2001 = list("v_CA01_116"),
    vec_1996 = list("v_CA1996_111"),
    var_title = list(list(
      en = "Duplex",
      fr = "Duplex"
    )),
    var_short = list(list(
      en = "Duplex",
      fr = "Duplex"
    )),
    description = list(list(
      en = "The number of private dwellings that are apartments or flats in a duplex, defined as one of two dwellings located one above the other, which may or may not be attached to other dwellings or buildings.",
      fr = "Le nombre de logements privés qui sont des appartements ou plain-pieds dans un duplex, définis comme l'un de deux logements superposés qui peuvent être ou ne pas être joints à d'autres logements ou immeubles."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_small",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_439"),
    vec_2016 = list("v_CA16_415"),
    vec_2011 = list("v_CA11F_207"),
    vec_2006 = list("v_CA06_125"),
    vec_2001 = list("v_CA01_118"),
    vec_1996 = list("v_CA1996_113"),
    var_title = list(list(
      en = "Apartments, less than 5 storeys",
      fr = "Appartements de moins de 5 étages"
    )),
    var_short = list(list(
      en = "Apt. <5",
      fr = "Apt. <5"
    )),
    description = list(list(
      en = "The number of private dwellings that are apartments in a building that has fewer than five storeys, defined as dwelling units attached to other dwelling units, commercial units or other non-residential space in a building with fewer than five storeys.",
      fr = "Le nombre de logements privés qui sont des appartements dans un immeuble de moins de cinq étages, définis comme des logements joints à d'autres logements ou à d'autres locaux commerciaux ou non résidentiels dans un immeuble de moins de cinq étages."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_large",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_440"),
    vec_2016 = list("v_CA16_410"),
    vec_2011 = list("v_CA11F_201"),
    vec_2006 = list("v_CA06_124"),
    vec_2001 = list("v_CA01_117"),
    vec_1996 = list("v_CA1996_112"),
    var_title = list(list(
      en = "Apartments, 5 or more storeys",
      fr = "Appartements de 5 étages ou plus"
    )),
    var_short = list(list(
      en = "Apt. >=5",
      fr = "Apt. >=5"
    )),
    description = list(list(
      en = "The number of private dwellings that are apartments in a building that has five or more storeys, that is, dwelling units in high-rise apartment buildings with five or more storeys.",
      fr = "Le nombre de logements privés qui sont des appartements dans un immeuble de cinq étages ou plus, c'est-à-dire des logements situés dans une tour d'habitation comportant cinq étages ou plus."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_other_single",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_441"),
    vec_2016 = list("v_CA16_416"),
    vec_2011 = list("v_CA11F_208"),
    vec_2006 = list("v_CA06_126"),
    vec_2001 = list("v_CA01_119"),
    vec_1996 = list("v_CA1996_114"),
    var_title = list(list(
      en = "Other single-attached",
      fr = "Autres maisons individuelles attenantes"
    )),
    var_short = list(list(
      en = "Other single",
      fr = "Autres"
    )),
    description = list(list(
      en = "The number of private dwellings that are other single-attached houses, defined as single dwellings attached to another building that do not fall into the other dwelling type categories, such as a single dwelling attached to a non-residential structure (e.g., a store or a church) or occasionally to another residential structure (e.g., an apartment building).",
      fr = "Le nombre de logements privés qui sont d'autres maisons individuelles attenantes, définis comme des logements individuels joints à un autre immeuble et ne se classant dans aucune des autres catégories de type de logement, comme un logement individuel réuni à une construction non résidentielle (p. ex., un magasin ou une église) ou, parfois, à une autre construction résidentielle (p. ex., un immeuble d'appartements)."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_movable",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Building typology",
    vec_2021 = list("v_CA21_442"),
    vec_2016 = list("v_CA16_417"),
    vec_2011 = list("v_CA11F_202"),
    vec_2006 = list("v_CA06_127"),
    vec_2001 = list("v_CA01_120"),
    vec_1996 = list("v_CA1996_115"),
    var_title = list(list(
      en = "Movable dwellings",
      fr = "Logements mobiles"
    )),
    var_short = list(list(
      en = "Movable",
      fr = "Mobiles"
    )),
    description = list(list(
      en = "The number of private dwellings that are mobile homes or other movable dwellings, defined as single dwellings designed and constructed to be transported on their own chassis and capable of being moved to a new location on short notice, or other single dwellings used as places of residence that can be moved on short notice, such as tents, recreational vehicles, travel trailers, houseboats or floating homes.",
      fr = "Le nombre de logements privés qui sont des habitations mobiles ou d'autres logements mobiles, définis comme des logements individuels conçus et construits pour être transportés sur leur propre châssis et pouvant être déplacés sans grand délai, ou d'autres logements individuels utilisés comme résidence et pouvant être déplacés sans grand délai, tels qu'une tente, un véhicule récréatif, une roulotte de voyage, un bateau-maison ou une maison flottante."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  )


verify_parents(
  vectors_df = census_vectors_typology,
  parents_df = census_vectors_housing_parent
)

census_vectors_typology <- rbind(census_vectors_typology)

usethis::use_data(census_vectors_typology, overwrite = TRUE)

## IMPORT BEDROOM SIZE CENSUS VECTORS ##########################################

census_vectors_bedroomsize <-
  tibble::tibble(
    var_code = "bedroom_zero",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4245"),
    vec_2016 = list("v_CA16_4844"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "0 bedrooms",
      fr = "0 chambres à coucher"
    )),
    var_short = list(list(
      en = "0 bedrooms",
      fr = "0 CC"
    )),
    description = list(list(
      en = "The number of occupied private dwellings with zero bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes and exclude rooms designed for another use during the day, such as living rooms and dining rooms. One-room private dwellings, such as bachelor or studio apartments, are classified as having zero bedrooms.",
      fr = "Le nombre de logements privés occupés ne comptant aucune chambre à coucher. Les chambres à coucher désignent des pièces utilisées principalement pour dormir et excluent les pièces conçues pour un autre usage pendant la journée, comme les salons et les salles à manger. Les logements privés d'une pièce, comme les studios, sont classés comme n'ayant aucune chambre à coucher."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_one",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4246"),
    vec_2016 = list("v_CA16_4845"),
    # BECAUSE 2011 LISTS 0 AND 1 TOGETHER !
    # vec_2011 = list("v_CA11N_2248"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "1 bedroom",
      fr = "1 chambre à coucher"
    )),
    var_short = list(list(
      en = "1 bedroom",
      fr = "1 CC"
    )),
    description = list(list(
      en = "The number of occupied private dwellings with one bedroom. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding rooms such as living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant une chambre à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d'autres fins (comme une chambre d'ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_two",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4247"),
    vec_2016 = list("v_CA16_4846"),
    vec_2011 = list("v_CA11N_2249"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "2 bedrooms",
      fr = "2 chambres à coucher"
    )),
    var_short = list(list(
      en = "2 bedrooms",
      fr = "2 CC"
    )),
    description = list(list(
      en = "The number of occupied private dwellings with two bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant deux chambres à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d'autres fins (comme une chambre d'ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_three",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4248"),
    vec_2016 = list("v_CA16_4847"),
    vec_2011 = list("v_CA11N_2250"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "3 bedrooms",
      fr = "3 chambres à coucher"
    )),
    var_short = list(list(
      en = "3 bedrooms",
      fr = "3 CC"
    )),
    description = list(list(
      en = "The number of occupied private dwellings with three bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant trois chambres à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d'autres fins (comme une chambre d'ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_four",
    type = list(
      (list(
        unit = "count",
        aggregation_field = "sum",
        measurement_scale = "scalar"
      ))
    ),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4249"),
    vec_2016 = list("v_CA16_4848"),
    vec_2011 = list("v_CA11N_2251"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "4 or more bedrooms",
      fr = "4 chambres à coucher ou plus"
    )),
    var_short = list(list(
      en = "4+ bedrooms",
      fr = "4+ CC"
    )),
    description = list(list(
      en = "The number of occupied private dwellings with four or more bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant quatre chambres à coucher ou plus. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d'autres fins (comme une chambre d'ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  )

verify_parents(
  vectors_df = census_vectors_bedroomsize,
  parents_df = census_vectors_housing_parent
)

usethis::use_data(census_vectors_bedroomsize, overwrite = TRUE)

## IMPORT BEDROOM SIZE CENSUS VECTORS ##########################################

census_vectors_buildingage <-
  tibble::tibble(
    var_code = "buildingage_1960constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4264"),
    vec_2016 = list("v_CA16_4863"),
    vec_2011 = list("v_CA11N_2234"),
    vec_2006 = list(c("v_CA06_110", "v_CA06_111")),
    vec_2001 = list(c("v_CA01_105", "v_CA01_106")),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built <1960",
      fr = "Construits <1960"
    )),
    var_short = list(list(
      en = "Pre-1960",
      fr = "<1960"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built before 1960. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits avant 1960. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_1980constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4265"),
    vec_2016 = list("v_CA16_4864"),
    vec_2011 = list("v_CA11N_2235"),
    vec_2006 = list("v_CA06_113"),
    vec_2001 = list(c("v_CA01_107", "v_CA01_108")),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 1961–1980",
      fr = "Construits entre 1961-1980"
    )),
    var_short = list(list(
      en = "1961–1980",
      fr = "1961–1980"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 1961 and 1980. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 1961 et 1980. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_1990constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4266"),
    vec_2016 = list("v_CA16_4865"),
    vec_2011 = list("v_CA11N_2236"),
    vec_2006 = list(c("v_CA06_114", "v_CA06_115")),
    vec_2001 = list("v_CA01_109"),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 1981–1990",
      fr = "Construits entre 1981-1990"
    )),
    var_short = list(list(
      en = "1981–1990",
      fr = "1981–1990"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 1981 and 1990. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 1981 et 1990. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2000constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4267"),
    vec_2016 = list("v_CA16_4866"),
    vec_2011 = list("v_CA11N_2237"),
    vec_2006 = list(c("v_CA06_116", "v_CA06_117")),
    vec_2001 = list(c("v_CA01_110", "v_CA01_111")),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 1991–2000",
      fr = "Construits entre 1991-2000"
    )),
    var_short = list(list(
      en = "1991–2000",
      fr = "1991–2000"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 1991 and 2000. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 1991 et 2000. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2005constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4268"),
    vec_2016 = list("v_CA16_4867"),
    vec_2011 = list("v_CA11N_2238"),
    vec_2006 = list("v_CA06_118"),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 2001–2005",
      fr = "Construits entre 2001-2005"
    )),
    var_short = list(list(
      en = "2001–2005",
      fr = "2001–2005"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 2001 and 2005. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 2001 et 2005. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2010constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4269"),
    vec_2016 = list("v_CA16_4868"),
    vec_2011 = list("v_CA11N_2239"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 2006–2010",
      fr = "Construits entre 2006-2010"
    )),
    var_short = list(list(
      en = "2006–2010",
      fr = "2006–2010"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 2006 and 2010. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 2006 et 2010. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2015constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4270"),
    vec_2016 = list("v_CA16_4869"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 2011–2015",
      fr = "Construits entre 2011-2015"
    )),
    var_short = list(list(
      en = "2011–2015",
      fr = "2011–2015"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 2011 and 2015. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 2011 et 2015. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "buildingage_2020constr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Housing period of construction",
    vec_2021 = list("v_CA21_4271"),
    vec_2016 = list(NA),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Built 2016–2020",
      fr = "Construits entre 2016-2020"
    )),
    var_short = list(list(
      en = "2016–2020",
      fr = "2016–2020"
    )),
    description = list(list(
      en = "The number of occupied private dwellings built between 2016 and 2020. Period of construction refers to the period in time during which the dwelling was originally constructed. It refers to the period when construction was completed, not the time of later remodelling, additions or conversions. For properties with multiple residential structures, it refers to the period in which the most recent structure was completed.",
      fr = "Le nombre de logements privés occupés construits entre 2016 et 2020. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s'agit de la période d'achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  )


verify_parents(
  vectors_df = census_vectors_buildingage,
  parents_df = census_vectors_housing_parent
)

usethis::use_data(census_vectors_buildingage, overwrite = TRUE)

## IMPORT HOUSEHOLD SIZE CENSUS VECTORS ########################################

census_vectors_householdsize <-
  tibble::tibble(
    var_code = "household_size_1",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_444"),
    vec_2016 = list("v_CA16_419"),
    vec_2011 = list("v_CA11F_210"),
    vec_2006 = list("v_CA06_129"),
    vec_2001 = list("v_CA01_122"),
    vec_1996 = list("v_CA1996_117"),
    var_title = list(list(
      en = "One-person",
      fr = "Une personne"
    )),
    var_short = list(list(
      en = "One",
      fr = "Un."
    )),
    description = list(list(
      en = "The number of private households consisting of one person",
      fr = "Le nombre de ménages privés composés d'une seule personne"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_1_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_535"),
    vec_2016 = list(NA),
    vec_2011 = list("v_CA11F_158"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "One-person - Male",
      fr = "Une personne - Homme"
    )),
    var_short = list(list(
      en = "One M",
      fr = "Un. H"
    )),
    description = list(list(
      en = "The number of private households consisting of one male person",
      fr = "Le nombre de ménages privés composés d'un seul homme"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_1_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_536"),
    vec_2016 = list(NA),
    vec_2011 = list("v_CA11F_159"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "One-person - Female",
      fr = "Une personne - Femme"
    )),
    var_short = list(list(
      en = "One F",
      fr = "Un. F"
    )),
    description = list(list(
      en = "The number of private households consisting of one female person",
      fr = "Le nombre de ménages privés composés d'une seule femme"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_2",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_445"),
    vec_2016 = list("v_CA16_420"),
    vec_2011 = list("v_CA11F_211"),
    vec_2006 = list("v_CA06_130"),
    vec_2001 = list("v_CA01_123"),
    vec_1996 = list("v_CA1996_118"),
    var_title = list(list(
      en = "Two-person",
      fr = "Deux personnes"
    )),
    var_short = list(list(
      en = "Two",
      fr = "Deux"
    )),
    description = list(list(
      en = "The number of private households consisting of two persons",
      fr = "Le nombre de ménages privés composés de deux personnes"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_3",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_446"),
    vec_2016 = list("v_CA16_421"),
    vec_2011 = list("v_CA11F_212"),
    vec_2006 = list("v_CA06_131"),
    vec_2001 = list("v_CA01_124"),
    vec_1996 = list("v_CA1996_119"),
    var_title = list(list(
      en = "Three-person",
      fr = "Trois personnes"
    )),
    var_short = list(list(
      en = "Three",
      fr = "Trois"
    )),
    description = list(list(
      en = "The number of private households consisting of three persons",
      fr = "Le nombre de ménages privés composés de trois personnes"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "household_size_4",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list(c("v_CA21_447", "v_CA21_448")),
    vec_2016 = list(c("v_CA16_422", "v_CA16_423")),
    vec_2011 = list(c("v_CA11F_213", "v_CA11F_214", "v_CA11F_215")),
    vec_2006 = list(c("v_CA06_132", "v_CA06_133")),
    vec_2001 = list(c("v_CA01_125", "v_CA01_126")),
    vec_1996 = list(c("v_CA1996_120", "v_CA1996_121")),
    var_title = list(list(
      en = "Four-or-more-person",
      fr = "Quatre personnes ou pluss"
    )),
    var_short = list(list(
      en = "Four+",
      fr = "Quatre+"
    )),
    description = list(list(
      en = "The number of private households consisting of four or more persons",
      fr = "Le nombre de ménages privés composés de quatre personnes ou plus"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "average_household_size",
    type = list(list(
      unit = "ratio",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Households",
    vec_2021 = list("v_CA21_452"),
    vec_2016 = list("v_CA16_425"),
    vec_2011 = list("v_CA11F_217"),
    vec_2006 = list("v_CA06_135"),
    vec_2001 = list("v_CA01_128"),
    vec_1996 = list("v_CA1996_1699"),
    var_title = list(list(
      en = "Average household size",
      fr = "Taille moyenne des ménages"
    )),
    var_short = list(list(
      en = "Avg size",
      fr = "Taille moy."
    )),
    description = list(list(
      en = "The average number of persons per private household",
      fr = "Le nombre moyen de personnes par ménage privé"
    )),
    parent_vec = "private_households",
    parent = FALSE
  )

# The parent is shared with housing
from_hou <- census_vectors_housing_parent[
  census_vectors_housing_parent$var_code %in%
    census_vectors_householdsize$parent_vec,
]

verify_parents(vectors_df = census_vectors_householdsize, parents_df = from_hou)

usethis::use_data(census_vectors_householdsize, overwrite = TRUE)

## IMPORT INCOME CENSUS VECTORS ################################################

census_vectors_income <-
  # Household (private households) ----
  tibble::tibble(
    var_code = "median_income_household",
    type = list(list(
      unit = "dollar",
      aggregation_field = "med",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_906"),
    vec_2016 = list("v_CA16_2397"),
    vec_2011 = list("v_CA11N_2562"),
    vec_2006 = list("v_CA06_2000"),
    vec_2001 = list("v_CA01_1634"),
    vec_1996 = list("v_CA1996_1627"),
    var_title = list(list(
      en = "Median household income",
      fr = "Revenu médian des ménages"
    )),
    var_short = list(list(
      en = "Med. inc.",
      fr = "Rev. Méd."
    )),
    description = list(list(
      en = "The median income of households before tax",
      fr = "Le revenu médian des ménages avant impôt"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "average_income_household",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_915"),
    vec_2016 = list("v_CA16_4985"),
    vec_2011 = list("v_CA11N_2563"),
    vec_2006 = list("v_CA06_2001"),
    vec_2001 = list("v_CA01_1633"),
    vec_1996 = list("v_CA1996_1626"),
    var_title = list(list(
      en = "Average household income",
      fr = "Revenu moyen des ménages"
    )),
    var_short = list(list(
      en = "Avg. inc.",
      fr = "Revenu moy."
    )),
    description = list(list(
      en = "The average income of households before tax",
      fr = "Le revenu moyen des ménages avant impôt"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_household_50",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(paste0("v_CA21_9", 24:33)),
    vec_2016 = list(c(
      "v_CA16_2406",
      "v_CA16_2407",
      "v_CA16_2408",
      "v_CA16_2409",
      "v_CA16_2410",
      "v_CA16_2411",
      "v_CA16_2412",
      "v_CA16_2413",
      "v_CA16_2414",
      "v_CA16_2415"
    )),
    vec_2011 = list(paste0("v_CA11N_25", 34:40)),
    vec_2006 = list(paste0("v_CA06_19", 89:93)),
    vec_2001 = list(paste0("v_CA01_16", 22:26)),
    vec_1996 = list(paste0("v_CA1996_16", 15:19)),
    var_title = list(list(
      en = "Income under $50k",
      fr = "Revenu inférieur à 50 000 $"
    )),
    var_short = list(list(
      en = "Inc. <$50k",
      fr = "Rev. <$50k"
    )),
    description = list(list(
      en = "The number of households whose total income is under $50,000",
      fr = "Le nombre de ménages dont le revenu total est inférieur à 50 000 $"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_household_100",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(paste0("v_CA21_9", 34:38)),
    vec_2016 = list(paste0("v_CA16_24", 16:20)),
    vec_2011 = list(paste0("v_CA11N_25", 41:43)),
    vec_2006 = list(paste0("v_CA06_19", 94:98)),
    vec_2001 = list(paste0("v_CA01_16", 27:31)),
    vec_1996 = list(paste0("v_CA1996_16", 20:24)),
    var_title = list(list(
      en = "Income between $50k-$100k",
      fr = "Revenu compris entre 50 000 et 100 000 dollars"
    )),
    var_short = list(list(
      en = "Inc. $50-100k",
      fr = "Rev. $50-100k"
    )),
    description = list(list(
      en = "The number of households whose total income is between $50,000 and $99,999",
      fr = "Le nombre de ménages dont le revenu total se situe entre 50 000 $ et 99 999 $"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_household_high",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_939"),
    vec_2016 = list("v_CA16_2421"),
    vec_2011 = list(paste0("v_CA11N_25", 44:46)),
    vec_2006 = list(NA),
    vec_2001 = list("v_CA01_1632"),
    vec_1996 = list("v_CA1996_1625"),
    var_title = list(list(
      en = "Income above $100k",
      fr = "Revenu supérieur à 100 000 $"
    )),
    var_short = list(list(
      en = "Inc. >$100k",
      fr = "Rev. >$100k"
    )),
    description = list(list(
      en = "The number of households whose total income is $100,000 or more",
      fr = "Le nombre de ménages dont le revenu total est de 100 000 $ ou plus"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_limat",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1025"),
    vec_2016 = list("v_CA16_2525"),
    vec_2011 = list("v_CA11N_2591"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Prevalence of low income (after-tax)",
      fr = "Prévalence des faibles revenus (après impôts)"
    )),
    var_short = list(list(
      en = "Low income",
      fr = "Faible revenu"
    )),
    description = list(list(
      en = "The prevalence of low income in private households based on the Low-income measure, after-tax (LIM-AT), where a household is considered low income if its after-tax income, adjusted for household size, is below 50% of the national median adjusted after-tax income",
      fr = "La prévalence des faibles revenus dans les ménages privés sur la base de la mesure de faible revenu après impôt (MFR-AT), selon laquelle un ménage est considéré à faible revenu si son revenu après impôt ajusté en fonction de la taille du ménage est inférieur à 50 % du revenu médian après impôt ajusté à l'échelle nationale"
    )),
    parent_vec = "income_status",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_limat_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1026"),
    vec_2016 = list("v_CA16_2526"),
    vec_2011 = list("v_CA11N_2592"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Prevalence of low income (after-tax) - Male",
      fr = "Prévalence des faibles revenus (après impôts) - Homme"
    )),
    var_short = list(list(
      en = "Low income M",
      fr = "Faible rev. H"
    )),
    description = list(list(
      en = "The number of males in low income based on the Low-income measure, after-tax (LIM-AT)",
      fr = "Le nombre d'hommes à faible revenu selon la mesure de faible revenu après impôt (MFR-AT)"
    )),
    parent_vec = "income_status_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_limat_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1027"),
    vec_2016 = list("v_CA16_2527"),
    vec_2011 = list("v_CA11N_2593"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Prevalence of low income (after-tax) - Female",
      fr = "Prévalence des faibles revenus (après impôts) - Femme"
    )),
    var_short = list(list(
      en = "Low income F",
      fr = "Faible rev. F"
    )),
    description = list(list(
      en = "The number of females in low income based on the Low-income measure, after-tax (LIM-AT)",
      fr = "Le nombre de femmes à faible revenu selon la mesure de faible revenu après impôt (MFR-AT)"
    )),
    parent_vec = "income_status_f",
    parent = FALSE
  ) |>
  # Individual (persons with income) ----
  tibble::add_row(
    var_code = "median_income_individual",
    type = list(list(
      unit = "dollar",
      aggregation_field = "med",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_560"),
    vec_2016 = list("v_CA16_2207"),
    vec_2011 = list("v_CA11N_2341"),
    vec_2006 = list("v_CA06_1583"),
    vec_2001 = list("v_CA01_1449"),
    vec_1996 = list("v_CA1996_1454"),
    var_title = list(list(
      en = "Median individual income",
      fr = "Revenu médian individuel"
    )),
    var_short = list(list(
      en = "Med. inc.",
      fr = "Rev. Méd."
    )),
    description = list(list(
      en = "The median total income of individuals before tax",
      fr = "Le revenu total médian des individus avant impôt"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "median_income_individual_m",
    type = list(list(
      unit = "dollar",
      aggregation_field = "med",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_561"),
    vec_2016 = list("v_CA16_2208"),
    vec_2011 = list("v_CA11N_2342"),
    vec_2006 = list("v_CA06_1605"),
    vec_2001 = list("v_CA01_1471"),
    vec_1996 = list("v_CA1996_1476"),
    var_title = list(list(
      en = "Median individual income - Male",
      fr = "Revenu médian individuel - Homme"
    )),
    var_short = list(list(
      en = "Med. inc. M",
      fr = "Rev. Méd. H"
    )),
    description = list(list(
      en = "The median total income of male individuals before tax",
      fr = "Le revenu total médian des hommes avant impôt"
    )),
    parent_vec = "with_income_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "median_income_individual_f",
    type = list(list(
      unit = "dollar",
      aggregation_field = "med",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_562"),
    vec_2016 = list("v_CA16_2209"),
    vec_2011 = list("v_CA11N_2343"),
    vec_2006 = list("v_CA06_1627"),
    vec_2001 = list("v_CA01_1493"),
    vec_1996 = list("v_CA1996_1498"),
    var_title = list(list(
      en = "Median individual income - Female",
      fr = "Revenu médian individuel - Femme"
    )),
    var_short = list(list(
      en = "Med. inc. F",
      fr = "Rev. Méd. F"
    )),
    description = list(list(
      en = "The median total income of female individuals before tax",
      fr = "Le revenu total médian des femmes avant impôt"
    )),
    parent_vec = "with_income_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "average_income_individual",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_605"),
    vec_2016 = list("v_CA16_4957"),
    vec_2011 = list("v_CA11N_2344"),
    vec_2006 = list("v_CA06_1584"),
    vec_2001 = list("v_CA01_1448"),
    vec_1996 = list("v_CA1996_1453"),
    var_title = list(list(
      en = "Average individual income",
      fr = "Revenu moyen individuel"
    )),
    var_short = list(list(
      en = "Avg. inc.",
      fr = "Revenu moy."
    )),
    description = list(list(
      en = "The average total income of individuals before tax",
      fr = "Le revenu total moyen des individus avant impôt"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "average_income_individual_m",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_606"),
    vec_2016 = list("v_CA16_4958"),
    vec_2011 = list("v_CA11N_2345"),
    vec_2006 = list("v_CA06_1606"),
    vec_2001 = list("v_CA01_1470"),
    vec_1996 = list("v_CA1996_1475"),
    var_title = list(list(
      en = "Average individual income - Male",
      fr = "Revenu moyen individuel - Homme"
    )),
    var_short = list(list(
      en = "Avg. inc. M",
      fr = "Rev. moy. H"
    )),
    description = list(list(
      en = "The average total income of male individuals before tax",
      fr = "Le revenu total moyen des hommes avant impôt"
    )),
    parent_vec = "with_income_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "average_income_individual_f",
    type = list(list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_607"),
    vec_2016 = list("v_CA16_4959"),
    vec_2011 = list("v_CA11N_2346"),
    vec_2006 = list("v_CA06_1628"),
    vec_2001 = list("v_CA01_1492"),
    vec_1996 = list("v_CA1996_1497"),
    var_title = list(list(
      en = "Average individual income - Female",
      fr = "Revenu moyen individuel - Femme"
    )),
    var_short = list(list(
      en = "Avg. inc. F",
      fr = "Rev. moy. F"
    )),
    description = list(list(
      en = "The average total income of female individuals before tax",
      fr = "Le revenu total moyen des femmes avant impôt"
    )),
    parent_vec = "with_income_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_50",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_674",
      "v_CA21_677",
      "v_CA21_680",
      "v_CA21_683",
      "v_CA21_686"
    )),
    vec_2016 = list(c(
      "v_CA16_2258",
      "v_CA16_2261",
      "v_CA16_2264",
      "v_CA16_2267",
      "v_CA16_2270"
    )),
    vec_2011 = list(c(
      "v_CA11N_2302",
      "v_CA11N_2305",
      "v_CA11N_2308",
      "v_CA11N_2311",
      "v_CA11N_2314",
      "v_CA11N_2317",
      "v_CA11N_2320"
    )),
    vec_2006 = list(paste0("v_CA06_15", 67:80)),
    vec_2001 = list(paste0("v_CA01_14", 32:45)),
    vec_1996 = list(paste0("v_CA1996_14", 37:50)),
    var_title = list(list(
      en = "Income under $50k",
      fr = "Revenu inférieur à 50 000 $"
    )),
    var_short = list(list(
      en = "Inc. <$50k",
      fr = "Rev. <$50k"
    )),
    description = list(list(
      en = "The number of persons whose total income is under $50,000",
      fr = "Le nombre de personnes dont le revenu total est inférieur à 50 000 $"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_50_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_675",
      "v_CA21_678",
      "v_CA21_681",
      "v_CA21_684",
      "v_CA21_687"
    )),
    vec_2016 = list(c(
      "v_CA16_2259",
      "v_CA16_2262",
      "v_CA16_2265",
      "v_CA16_2268",
      "v_CA16_2271"
    )),
    vec_2011 = list(c(
      "v_CA11N_2303",
      "v_CA11N_2306",
      "v_CA11N_2309",
      "v_CA11N_2312",
      "v_CA11N_2315",
      "v_CA11N_2318",
      "v_CA11N_2321"
    )),
    vec_2006 = list(paste0("v_CA06_", 1589:1602)),
    vec_2001 = list(paste0("v_CA01_", 1454:1467)),
    vec_1996 = list(paste0("v_CA1996_", 1459:1472)),
    var_title = list(list(
      en = "Income under $50k - Male",
      fr = "Revenu inférieur à 50 000 $ - Homme"
    )),
    var_short = list(list(
      en = "Inc. <$50k M",
      fr = "Rev. <$50k H"
    )),
    description = list(list(
      en = "The number of males whose total income is under $50,000",
      fr = "Le nombre d'hommes dont le revenu total est inférieur à 50 000 $"
    )),
    parent_vec = "with_income_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_50_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_676",
      "v_CA21_679",
      "v_CA21_682",
      "v_CA21_685",
      "v_CA21_688"
    )),
    vec_2016 = list(c(
      "v_CA16_2260",
      "v_CA16_2263",
      "v_CA16_2266",
      "v_CA16_2269",
      "v_CA16_2272"
    )),
    vec_2011 = list(c(
      "v_CA11N_2304",
      "v_CA11N_2307",
      "v_CA11N_2310",
      "v_CA11N_2313",
      "v_CA11N_2316",
      "v_CA11N_2319",
      "v_CA11N_2322"
    )),
    vec_2006 = list(paste0("v_CA06_", 1611:1624)),
    vec_2001 = list(paste0("v_CA01_", 1476:1489)),
    vec_1996 = list(paste0("v_CA1996_", 1481:1494)),
    var_title = list(list(
      en = "Income under $50k - Female",
      fr = "Revenu inférieur à 50 000 $ - Femme"
    )),
    var_short = list(list(
      en = "Inc. <$50k F",
      fr = "Rev. <$50k F"
    )),
    description = list(list(
      en = "The number of females whose total income is under $50,000",
      fr = "Le nombre de femmes dont le revenu total est inférieur à 50 000 $"
    )),
    parent_vec = "with_income_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_100",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_689",
      "v_CA21_692",
      "v_CA21_695",
      "v_CA21_698",
      "v_CA21_701"
    )),
    vec_2016 = list(c(
      "v_CA16_2273",
      "v_CA16_2276",
      "v_CA16_2279",
      "v_CA16_2282",
      "v_CA16_2285"
    )),
    vec_2011 = list(c(
      "v_CA11N_2323",
      "v_CA11N_2326",
      "v_CA11N_2329"
    )),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income between $50k-$100k",
      fr = "Revenu compris entre 50 000 et 100 000 dollars"
    )),
    var_short = list(list(
      en = "Inc. $50-100k",
      fr = "Rev. $50-100k"
    )),
    description = list(list(
      en = "The number of persons whose total income is between $50,000 and $99,999",
      fr = "Le nombre de personnes dont le revenu total se situe entre 50 000 $ et 99 999 $"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_100_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_690",
      "v_CA21_693",
      "v_CA21_696",
      "v_CA21_699",
      "v_CA21_702"
    )),
    vec_2016 = list(c(
      "v_CA16_2274",
      "v_CA16_2277",
      "v_CA16_2280",
      "v_CA16_2283",
      "v_CA16_2286"
    )),
    vec_2011 = list(c(
      "v_CA11N_2324",
      "v_CA11N_2327",
      "v_CA11N_2330"
    )),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income between $50k-$100k - Male",
      fr = "Revenu compris entre 50 000 et 100 000 dollars - Homme"
    )),
    var_short = list(list(
      en = "Inc. $50-100k M",
      fr = "Rev. $50-100k H"
    )),
    description = list(list(
      en = "The number of males whose total income is between $50,000 and $99,999",
      fr = "Le nombre d'hommes dont le revenu total se situe entre 50 000 $ et 99 999 $"
    )),
    parent_vec = "with_income_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_100_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c(
      "v_CA21_691",
      "v_CA21_694",
      "v_CA21_697",
      "v_CA21_700",
      "v_CA21_703"
    )),
    vec_2016 = list(c(
      "v_CA16_2275",
      "v_CA16_2278",
      "v_CA16_2281",
      "v_CA16_2284",
      "v_CA16_2287"
    )),
    vec_2011 = list(c(
      "v_CA11N_2325",
      "v_CA11N_2328",
      "v_CA11N_2331"
    )),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income between $50k-$100k - Female",
      fr = "Revenu compris entre 50 000 et 100 000 dollars - Femme"
    )),
    var_short = list(list(
      en = "Inc. $50-100k F",
      fr = "Rev. $50-100k F"
    )),
    description = list(list(
      en = "The number of females whose total income is between $50,000 and $99,999",
      fr = "Le nombre de femmes dont le revenu total se situe entre 50 000 $ et 99 999 $"
    )),
    parent_vec = "with_income_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_high",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_704"),
    vec_2016 = list("v_CA16_2288"),
    vec_2011 = list("v_CA11N_2332"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income above $100k",
      fr = "Revenu supérieur à 100 000 $"
    )),
    var_short = list(list(
      en = "Inc. >$100k",
      fr = "Rev. >$100k"
    )),
    description = list(list(
      en = "The number of persons whose total income is $100,000 or more",
      fr = "Le nombre de personnes dont le revenu total est de 100 000 $ ou plus"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_high_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_705"),
    vec_2016 = list("v_CA16_2289"),
    vec_2011 = list("v_CA11N_2333"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income above $100k - Male",
      fr = "Revenu supérieur à 100 000 $ - Homme"
    )),
    var_short = list(list(
      en = "Inc. >$100k M",
      fr = "Rev. >$100k H"
    )),
    description = list(list(
      en = "The number of males whose total income is $100,000 or more",
      fr = "Le nombre d'hommes dont le revenu total est de 100 000 $ ou plus"
    )),
    parent_vec = "with_income_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_individual_high_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_706"),
    vec_2016 = list("v_CA16_2290"),
    vec_2011 = list("v_CA11N_2334"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Income above $100k - Female",
      fr = "Revenu supérieur à 100 000 $ - Femme"
    )),
    var_short = list(list(
      en = "Inc. >$100k F",
      fr = "Rev. >$100k F"
    )),
    description = list(list(
      en = "The number of females whose total income is $100,000 or more",
      fr = "Le nombre de femmes dont le revenu total est de 100 000 $ ou plus"
    )),
    parent_vec = "with_income_f",
    parent = FALSE
  )

census_vectors_income_parent <-
  # Individuals with income
  tibble::tibble(
    var_code = "with_income",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_671"),
    vec_2016 = list("v_CA16_2252"),
    vec_2011 = list("v_CA11N_2299"),
    vec_2006 = list("v_CA06_1566"),
    vec_2001 = list("v_CA01_1431"),
    vec_1996 = list("v_CA1996_1436"),
    var_title = list(list(
      en = "Individuals with income",
      fr = "Personnes avec revenu"
    )),
    var_short = list(list(
      en = "With income",
      fr = "Avec revenu"
    )),
    description = list(list(
      en = "The total population aged 15 years and over with an income",
      fr = "La population totale âgée de 15 ans et plus disposant d'un revenu"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "with_income_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_672"),
    vec_2016 = list("v_CA16_2253"),
    vec_2011 = list("v_CA11N_2300"),
    vec_2006 = list("v_CA06_1588"),
    vec_2001 = list("v_CA01_1453"),
    vec_1996 = list("v_CA1996_1458"),
    var_title = list(list(
      en = "Individuals with income - Male",
      fr = "Personnes avec revenu - Homme"
    )),
    var_short = list(list(
      en = "With income M",
      fr = "Avec revenu H"
    )),
    description = list(list(
      en = "The total male population aged 15 years and over with an income",
      fr = "La population masculine totale âgée de 15 ans et plus disposant d'un revenu"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "with_income_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_673"),
    vec_2016 = list("v_CA16_2254"),
    vec_2011 = list("v_CA11N_2301"),
    vec_2006 = list("v_CA06_1610"),
    vec_2001 = list("v_CA01_1475"),
    vec_1996 = list("v_CA1996_1480"),
    var_title = list(list(
      en = "Individuals with income - Female",
      fr = "Personnes avec revenu - Femme"
    )),
    var_short = list(list(
      en = "With income F",
      fr = "Avec revenu F"
    )),
    description = list(list(
      en = "The total female population aged 15 years and over with an income",
      fr = "La population féminine totale âgée de 15 ans et plus disposant d'un revenu"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  # Population in private households (LIM-AT)
  tibble::add_row(
    var_code = "income_status",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1010"),
    vec_2016 = list("v_CA16_2510"),
    vec_2011 = list("v_CA11N_2576"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Population in private households (LIM-AT applicable)",
      fr = "Population en ménages privés (MFR-AT applicable)"
    )),
    var_short = list(list(
      en = "Households",
      fr = "Ménages"
    )),
    description = list(list(
      en = "The total population in private households to whom low-income concepts are applicable",
      fr = "La population totale des ménages privés à laquelle s'appliquent les concepts de bas revenus"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "income_status_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1011"),
    vec_2016 = list("v_CA16_2511"),
    vec_2011 = list("v_CA11N_2577"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Population in private households (LIM-AT applicable) - Male",
      fr = "Population en ménages privés (MFR-AT applicable) - Homme"
    )),
    var_short = list(list(
      en = "Households M",
      fr = "Ménages H"
    )),
    description = list(list(
      en = "The total male population in private households to whom low-income concepts are applicable",
      fr = "La population masculine totale des ménages privés à laquelle s'appliquent les concepts de bas revenus"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "income_status_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1012"),
    vec_2016 = list("v_CA16_2512"),
    vec_2011 = list("v_CA11N_2578"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Population in private households (LIM-AT applicable) - Female",
      fr = "Population en ménages privés (MFR-AT applicable) - Femme"
    )),
    var_short = list(list(
      en = "Households F",
      fr = "Ménages F"
    )),
    description = list(list(
      en = "The total female population in private households to whom low-income concepts are applicable",
      fr = "La population féminine totale des ménages privés à laquelle s'appliquent les concepts de bas revenus"
    )),
    parent_vec = NA,
    parent = TRUE
  )


# addition of needed parent available from the housing vectors
from_hou <- census_vectors_housing_parent[
  census_vectors_housing_parent$var_code %in% census_vectors_income$parent_vec,
]

verify_parents(
  vectors_df = census_vectors_income,
  parents_df = rbind(census_vectors_income_parent, from_hou)
)

census_vectors_income <- rbind(
  census_vectors_income,
  census_vectors_income_parent
)

usethis::use_data(census_vectors_income, overwrite = TRUE)

## IMPORT IDENTITY CENSUS VECTORS ##############################################

census_vectors_identity_parent <-
  tibble::tibble(
    var_code = "population_ph",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4404"),
    vec_2016 = list("v_CA16_3405"),
    vec_2011 = list("v_CA11N_16"),
    vec_2006 = list("v_CA06_474"),
    vec_2001 = list("v_CA01_402"),
    vec_1996 = list("v_CA1996_125"),
    var_title = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    var_short = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    description = list(list(
      en = "The total number of persons living in private households",
      fr = "Le nombre total de personnes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_ph_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4405"),
    vec_2016 = list("v_CA16_3406"),
    vec_2011 = list("v_CA11N_17"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Individuals - Male",
      fr = "Individus - Homme"
    )),
    var_short = list(list(
      en = "Individuals M",
      fr = "Individus H"
    )),
    description = list(list(
      en = "The total number of males living in private households",
      fr = "Le nombre total d'hommes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_ph_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4406"),
    vec_2016 = list("v_CA16_3407"),
    vec_2011 = list("v_CA11N_18"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Individuals - Female",
      fr = "Individus - Femme"
    )),
    var_short = list(list(
      en = "Individuals F",
      fr = "Individus F"
    )),
    description = list(list(
      en = "The total number of females living in private households",
      fr = "Le nombre total de femmes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  )

census_vectors_identity <-
  tibble::tibble(
    var_code = "citizenship_cit",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4392"),
    vec_2016 = list("v_CA16_3393"),
    vec_2011 = list("v_CA11N_4"),
    vec_2006 = list("v_CA06_470"),
    vec_2001 = list("v_CA01_400"),
    vec_1996 = list("v_CA1996_123"),
    var_title = list(list(
      en = "Canadian citizens",
      fr = "Citoyens canadiens"
    )),
    var_short = list(list(
      en = "Citizens",
      fr = "Citoyens"
    )),
    description = list(list(
      en = "The number of persons who hold Canadian citizenship, whether by birth in Canada or by naturalization",
      fr = "Le nombre de personnes qui possèdent la citoyenneté canadienne, soit par la naissance au Canada, soit par naturalisation"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_cit_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4393"),
    vec_2016 = list("v_CA16_3394"),
    vec_2011 = list("v_CA11N_5"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Canadian citizens - Male",
      fr = "Citoyens canadiens - Homme"
    )),
    var_short = list(list(
      en = "Citizens M",
      fr = "Citoyens H"
    )),
    description = list(list(
      en = "The number of males who hold Canadian citizenship",
      fr = "Le nombre d'hommes qui possèdent la citoyenneté canadienne"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_cit_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4394"),
    vec_2016 = list("v_CA16_3395"),
    vec_2011 = list("v_CA11N_6"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Canadian citizens - Female",
      fr = "Citoyens canadiens - Femme"
    )),
    var_short = list(list(
      en = "Citizens F",
      fr = "Citoyens F"
    )),
    description = list(list(
      en = "The number of females who hold Canadian citizenship",
      fr = "Le nombre de femmes qui possèdent la citoyenneté canadienne"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonimm",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4407"),
    vec_2016 = list("v_CA16_3408"),
    vec_2011 = list("v_CA11N_19"),
    vec_2006 = list("v_CA06_475"),
    vec_2001 = list("v_CA01_403"),
    vec_1996 = list("v_CA1996_126"),
    var_title = list(list(
      en = "Non-immigrants",
      fr = "Non-immigrants"
    )),
    var_short = list(list(
      en = "Non-immigrants",
      fr = "Non-immigrants"
    )),
    description = list(list(
      en = "The number of persons who are Canadian citizens by birth and have never been landed immigrants or permanent residents of Canada",
      fr = "Le nombre de personnes qui sont des citoyens canadiens de naissance et qui n'ont jamais été des immigrants reçus ou des résidents permanents du Canada"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonimm_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4408"),
    vec_2016 = list("v_CA16_3409"),
    vec_2011 = list("v_CA11N_20"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Non-immigrants - Male",
      fr = "Non-immigrants - Homme"
    )),
    var_short = list(list(
      en = "Non-imm. M",
      fr = "Non-imm. H"
    )),
    description = list(list(
      en = "The number of males who are Canadian citizens by birth and have never been landed immigrants",
      fr = "Le nombre d'hommes qui sont des citoyens canadiens de naissance et qui n'ont jamais été des immigrants reçus"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonimm_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4409"),
    vec_2016 = list("v_CA16_3410"),
    vec_2011 = list("v_CA11N_21"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Non-immigrants - Female",
      fr = "Non-immigrants - Femme"
    )),
    var_short = list(list(
      en = "Non-imm. F",
      fr = "Non-imm. F"
    )),
    description = list(list(
      en = "The number of females who are Canadian citizens by birth and have never been landed immigrants",
      fr = "Le nombre de femmes qui sont des citoyens canadiens de naissance et qui n'ont jamais été des immigrants reçus"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4410"),
    vec_2016 = list("v_CA16_3411"),
    vec_2011 = list("v_CA11N_22"),
    vec_2006 = list("v_CA06_478"),
    vec_2001 = list("v_CA01_406"),
    vec_1996 = list("v_CA1996_128"),
    var_title = list(list(
      en = "Immigrants",
      fr = "Immigrants"
    )),
    var_short = list(list(
      en = "Immigrants",
      fr = "Immigrants"
    )),
    description = list(list(
      en = "The number of persons who are, or have ever been, landed immigrants or permanent residents of Canada",
      fr = "Le nombre de personnes qui sont ou ont déjà été des immigrants reçus ou des résidents permanents du Canada"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4411"),
    vec_2016 = list("v_CA16_3412"),
    vec_2011 = list("v_CA11N_23"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Immigrants - Male",
      fr = "Immigrants - Homme"
    )),
    var_short = list(list(
      en = "Immigrants M",
      fr = "Immigrants H"
    )),
    description = list(list(
      en = "The number of males who are, or have ever been, landed immigrants or permanent residents of Canada",
      fr = "Le nombre d'hommes qui sont ou ont déjà été des immigrants reçus ou des résidents permanents du Canada"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4412"),
    vec_2016 = list("v_CA16_3413"),
    vec_2011 = list("v_CA11N_24"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Immigrants - Female",
      fr = "Immigrants - Femme"
    )),
    var_short = list(list(
      en = "Immigrants F",
      fr = "Immigrants F"
    )),
    description = list(list(
      en = "The number of females who are, or have ever been, landed immigrants or permanent residents of Canada",
      fr = "Le nombre de femmes qui sont ou ont déjà été des immigrants reçus ou des résidents permanents du Canada"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_new",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4431"),
    vec_2016 = list("v_CA16_3432"),
    vec_2011 = list("v_CA11N_43"),
    vec_2006 = list("v_CA06_553"),
    vec_2001 = list("v_CA01_507"),
    vec_1996 = list("v_CA1996_228"),
    var_title = list(list(
      en = "New immigrants",
      fr = "Nouveaux immigrants"
    )),
    var_short = list(list(
      en = "New immigrants",
      fr = "Nouveaux imm."
    )),
    description = list(list(
      en = "The number of persons who are landed immigrants or permanent residents whose first date of landed immigrant status was within the five years preceding the census reference day",
      fr = "Le nombre de personnes qui sont des immigrants reçus ou des résidents permanents dont la première date du statut d'immigrant reçu se situe dans les cinq années précédant la date de référence du recensement"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_new_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4432"),
    vec_2016 = list("v_CA16_3433"),
    vec_2011 = list("v_CA11N_44"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "New immigrants - Male",
      fr = "Nouveaux immigrants - Homme"
    )),
    var_short = list(list(
      en = "New imm. M",
      fr = "Nouv. imm. H"
    )),
    description = list(list(
      en = "The number of males who are recent landed immigrants within the five years preceding the census",
      fr = "Le nombre d'hommes qui sont des immigrants reçus récents dans les cinq années précédant le recensement"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_imm_new_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4433"),
    vec_2016 = list("v_CA16_3434"),
    vec_2011 = list("v_CA11N_45"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "New immigrants - Female",
      fr = "Nouveaux immigrants - Femme"
    )),
    var_short = list(list(
      en = "New imm. F",
      fr = "Nouv. imm. F"
    )),
    description = list(list(
      en = "The number of females who are recent landed immigrants within the five years preceding the census",
      fr = "Le nombre de femmes qui sont des immigrants reçus récents dans les cinq années précédant le recensement"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonpr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4434"),
    vec_2016 = list("v_CA16_3435"),
    vec_2011 = list("v_CA11N_46"),
    vec_2006 = list("v_CA06_511"),
    vec_2001 = list("v_CA01_458"),
    vec_1996 = list("v_CA1996_180"),
    var_title = list(list(
      en = "Non-permanent residents",
      fr = "Résidents non permanents"
    )),
    var_short = list(list(
      en = "Non-permanent",
      fr = "Non-permanents"
    )),
    description = list(list(
      en = "The number of persons from another country with a usual place of residence in Canada who have a work or study permit or have claimed refugee status",
      fr = "Le nombre de personnes provenant d'un autre pays dont le lieu habituel de résidence est au Canada et qui détiennent un permis de travail ou d'études ou ont demandé le statut de réfugié"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonpr_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4435"),
    vec_2016 = list("v_CA16_3436"),
    vec_2011 = list("v_CA11N_47"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Non-permanent residents - Male",
      fr = "Résidents non permanents - Homme"
    )),
    var_short = list(list(
      en = "Non-perm. M",
      fr = "Non-perm. H"
    )),
    description = list(list(
      en = "The number of males from another country with a work or study permit or refugee status in Canada",
      fr = "Le nombre d'hommes provenant d'un autre pays détenant un permis de travail ou d'études ou le statut de réfugié au Canada"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "citizenship_nonpr_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4436"),
    vec_2016 = list("v_CA16_3437"),
    vec_2011 = list("v_CA11N_48"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Non-permanent residents - Female",
      fr = "Résidents non permanents - Femme"
    )),
    var_short = list(list(
      en = "Non-perm. F",
      fr = "Non-perm. F"
    )),
    description = list(list(
      en = "The number of females from another country with a work or study permit or refugee status in Canada",
      fr = "Le nombre de femmes provenant d'un autre pays détenant un permis de travail ou d'études ou le statut de réfugié au Canada"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  # Visible minority and ethnic origin ----
  tibble::add_row(
    var_code = "iden_vm",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4875"),
    vec_2016 = list("v_CA16_3957"),
    vec_2011 = list("v_CA11N_460"),
    vec_2006 = list("v_CA06_1303"),
    vec_2001 = list("v_CA01_703"),
    vec_1996 = list("v_CA1996_784"),
    var_title = list(list(
      en = "Visible minorities",
      fr = "Minorités visibles"
    )),
    var_short = list(list(
      en = "Vis. minorities",
      fr = "Minorités vis."
    )),
    description = list(list(
      en = "The number of persons, other than Indigenous peoples, who self-identify as members of at least one visible minority group",
      fr = "Le nombre de personnes, à l'exception des peuples autochtones, qui s'identifient comme appartenant à au moins un groupe de minorités visibles"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_vm_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4876"),
    vec_2016 = list("v_CA16_3958"),
    vec_2011 = list("v_CA11N_461"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Visible minorities - Male",
      fr = "Minorités visibles - Homme"
    )),
    var_short = list(list(
      en = "Vis. min. M",
      fr = "Min. vis. H"
    )),
    description = list(list(
      en = "The number of males who self-identify as members of at least one visible minority group",
      fr = "Le nombre d'hommes qui s'identifient comme appartenant à au moins un groupe de minorités visibles"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_vm_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4877"),
    vec_2016 = list("v_CA16_3959"),
    vec_2011 = list("v_CA11N_462"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Visible minorities - Female",
      fr = "Minorités visibles - Femme"
    )),
    var_short = list(list(
      en = "Vis. min. F",
      fr = "Min. vis. F"
    )),
    description = list(list(
      en = "The number of females who self-identify as members of at least one visible minority group",
      fr = "Le nombre de femmes qui s'identifient comme appartenant à au moins un groupe de minorités visibles"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_aboriginal",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4204"),
    vec_2016 = list("v_CA16_3855"),
    vec_2011 = list("v_CA11N_1354"),
    vec_2006 = list("v_CA06_565"),
    vec_2001 = list("v_CA01_718"),
    vec_1996 = list("v_CA1996_473"),
    var_title = list(list(
      en = "Indigenous",
      fr = "Autochtones"
    )),
    var_short = list(list(
      en = "Indigenous",
      fr = "Autochtones"
    )),
    description = list(list(
      en = "The number of persons who identify as Indigenous, that is, as First Nations, Métis or Inuit",
      fr = "Le nombre de personnes qui s'identifient comme Autochtones, c'est-à-dire comme Premières Nations, Métis ou Inuits"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_aboriginal_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4205"),
    vec_2016 = list("v_CA16_3856"),
    vec_2011 = list("v_CA11N_1355"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Indigenous - Male",
      fr = "Autochtones - Homme"
    )),
    var_short = list(list(
      en = "Indigenous M",
      fr = "Autochtones H"
    )),
    description = list(list(
      en = "The number of males who identify as Indigenous",
      fr = "Le nombre d'hommes qui s'identifient comme Autochtones"
    )),
    parent_vec = "population_ph_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "iden_aboriginal_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Visible minority and ethnic origin",
    vec_2021 = list("v_CA21_4206"),
    vec_2016 = list("v_CA16_3857"),
    vec_2011 = list("v_CA11N_1356"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Indigenous - Female",
      fr = "Autochtones - Femme"
    )),
    var_short = list(list(
      en = "Indigenous F",
      fr = "Autochtones F"
    )),
    description = list(list(
      en = "The number of females who identify as Indigenous",
      fr = "Le nombre de femmes qui s'identifient comme Autochtones"
    )),
    parent_vec = "population_ph_f",
    parent = FALSE
  )

census_vectors_identity_parent <-
  tibble::tibble(
    var_code = "population_ph",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4404"),
    vec_2016 = list("v_CA16_3405"),
    vec_2011 = list("v_CA11N_16"),
    vec_2006 = list("v_CA06_474"),
    vec_2001 = list("v_CA01_402"),
    vec_1996 = list("v_CA1996_125"),
    var_title = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    var_short = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    description = list(list(
      en = "The total number of persons living in private households",
      fr = "Le nombre total de personnes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_ph_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4405"),
    vec_2016 = list("v_CA16_3406"),
    vec_2011 = list("v_CA11N_17"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Individuals - Male",
      fr = "Individus - Homme"
    )),
    var_short = list(list(
      en = "Individuals M",
      fr = "Individus H"
    )),
    description = list(list(
      en = "The total number of males living in private households",
      fr = "Le nombre total d'hommes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_ph_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Citizenship",
    vec_2021 = list("v_CA21_4406"),
    vec_2016 = list("v_CA16_3407"),
    vec_2011 = list("v_CA11N_18"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Individuals - Female",
      fr = "Individus - Femme"
    )),
    var_short = list(list(
      en = "Individuals F",
      fr = "Individus F"
    )),
    description = list(list(
      en = "The total number of females living in private households",
      fr = "Le nombre total de femmes vivant dans des ménages privés"
    )),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(
  vectors_df = census_vectors_identity,
  parents_df = census_vectors_identity_parent
)

census_vectors_identity <- rbind(
  census_vectors_identity,
  census_vectors_identity_parent
)

usethis::use_data(census_vectors_identity, overwrite = TRUE)

## IMPORT TRANSPORT CENSUS VECTORS #############################################

census_vectors_transport <-
  tibble::tibble(
    var_code = "trans_car",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7635"),
    vec_2016 = list(c("v_CA16_5795", "v_CA16_5798")),
    vec_2011 = list(c("v_CA11N_2194", "v_CA11N_2197")),
    vec_2006 = list(c("v_CA06_1101", "v_CA06_1102")),
    vec_2001 = list(c(
      "v_CA01_1255",
      "v_CA01_1256",
      "v_CA01_1264",
      "v_CA01_1265"
    )),
    vec_1996 = list(c(
      "v_CA1996_1326",
      "v_CA1996_1327",
      "v_CA1996_1335",
      "v_CA1996_1336"
    )),
    var_title = list(list(
      en = "Drive to work",
      fr = "Se rendent au travail en voiture"
    )),
    var_short = list(list(
      en = "Drive",
      fr = "Auto"
    )),
    description = list(list(
      en = "The number of employed persons whose main mode of commuting between home and work is to travel as a driver or passenger in a car, truck or van",
      fr = "Le nombre de personnes occupées dont le principal mode de transport pour la navette entre le domicile et le lieu de travail est de se déplacer comme conducteur ou passager d'une automobile, d'un camion ou d'une fourgonnette"
    )),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_car_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7636"),
    vec_2016 = list(c("v_CA16_5796", "v_CA16_5799")),
    vec_2011 = list(c("v_CA11N_2195", "v_CA11N_2198")),
    vec_2006 = list(c("v_CA06_1110", "v_CA06_1111")),
    vec_2001 = list(c("v_CA01_1255", "v_CA01_1256")),
    vec_1996 = list(c("v_CA1996_1326", "v_CA1996_1327")),
    var_title = list(list(
      en = "Drive to work - Male",
      fr = "Se rendent au travail en voiture - Homme"
    )),
    var_short = list(list(
      en = "Drive M",
      fr = "Auto H"
    )),
    description = list(list(
      en = "The number of employed males whose main mode of commuting is to travel as a driver or passenger in a car, truck or van",
      fr = "Le nombre d'hommes occupés dont le principal mode de transport est de se déplacer comme conducteur ou passager d'une automobile, d'un camion ou d'une fourgonnette"
    )),
    parent_vec = "employment_lf_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_car_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7637"),
    vec_2016 = list(c("v_CA16_5797", "v_CA16_5800")),
    vec_2011 = list(c("v_CA11N_2196", "v_CA11N_2199")),
    vec_2006 = list(c("v_CA06_1119", "v_CA06_1120")),
    vec_2001 = list(c("v_CA01_1264", "v_CA01_1265")),
    vec_1996 = list(c("v_CA1996_1335", "v_CA1996_1336")),
    var_title = list(list(
      en = "Drive to work - Female",
      fr = "Se rendent au travail en voiture - Femme"
    )),
    var_short = list(list(
      en = "Drive F",
      fr = "Auto F"
    )),
    description = list(list(
      en = "The number of employed females whose main mode of commuting is to travel as a driver or passenger in a car, truck or van",
      fr = "Le nombre de femmes occupées dont le principal mode de transport est de se déplacer comme conductrice ou passagère d'une automobile, d'un camion ou d'une fourgonnette"
    )),
    parent_vec = "employment_lf_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_walk_or_bike",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7647", "v_CA21_7650")),
    vec_2016 = list(c("v_CA16_5804", "v_CA16_5807")),
    vec_2011 = list(c("v_CA11N_2203", "v_CA11N_2206")),
    vec_2006 = list(c("v_CA06_1104", "v_CA06_1105")),
    vec_2001 = list(c(
      "v_CA01_1258",
      "v_CA01_1259",
      "v_CA01_1267",
      "v_CA01_1268"
    )),
    vec_1996 = list(c(
      "v_CA1996_1329",
      "v_CA1996_1330",
      "v_CA1996_1338",
      "v_CA1996_1339"
    )),
    var_title = list(list(
      en = "Walk or cycle to work",
      fr = "Se rendent au travail à pied ou à vélo"
    )),
    var_short = list(list(
      en = "Walk or cycle",
      fr = "Marche ou vélo"
    )),
    description = list(list(
      en = "The number of employed persons whose main mode of commuting between home and work is walking or cycling",
      fr = "Le nombre de personnes occupées dont le principal mode de transport pour la navette entre le domicile et le lieu de travail est la marche ou la bicyclette"
    )),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_walk_or_bike_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7648", "v_CA21_7651")),
    vec_2016 = list(c("v_CA16_5805", "v_CA16_5808")),
    vec_2011 = list(c("v_CA11N_2204", "v_CA11N_2207")),
    vec_2006 = list(c("v_CA06_1113", "v_CA06_1114")),
    vec_2001 = list(c("v_CA01_1258", "v_CA01_1259")),
    vec_1996 = list(c("v_CA1996_1329", "v_CA1996_1330")),
    var_title = list(list(
      en = "Walk or cycle to work - Male",
      fr = "Se rendent au travail à pied ou à vélo - Homme"
    )),
    var_short = list(list(
      en = "Walk/cycle M",
      fr = "Marche/vélo H"
    )),
    description = list(list(
      en = "The number of employed males whose main mode of commuting is walking or cycling",
      fr = "Le nombre d'hommes occupés dont le principal mode de transport est la marche ou la bicyclette"
    )),
    parent_vec = "employment_lf_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_walk_or_bike_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7649", "v_CA21_7652")),
    vec_2016 = list(c("v_CA16_5806", "v_CA16_5809")),
    vec_2011 = list(c("v_CA11N_2205", "v_CA11N_2208")),
    vec_2006 = list(c("v_CA06_1122", "v_CA06_1123")),
    vec_2001 = list(c("v_CA01_1267", "v_CA01_1268")),
    vec_1996 = list(c("v_CA1996_1338", "v_CA1996_1339")),
    var_title = list(list(
      en = "Walk or cycle to work - Female",
      fr = "Se rendent au travail à pied ou à vélo - Femme"
    )),
    var_short = list(list(
      en = "Walk/cycle F",
      fr = "Marche/vélo F"
    )),
    description = list(list(
      en = "The number of employed females whose main mode of commuting is walking or cycling",
      fr = "Le nombre de femmes occupées dont le principal mode de transport est la marche ou la bicyclette"
    )),
    parent_vec = "employment_lf_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_transit",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7644"),
    vec_2016 = list("v_CA16_5801"),
    vec_2011 = list("v_CA11N_2200"),
    vec_2006 = list("v_CA06_1103"),
    vec_2001 = list(c("v_CA01_1257", "v_CA01_1266")),
    vec_1996 = list(c("v_CA1996_1328", "v_CA1996_1337")),
    var_title = list(list(
      en = "Public transit to work",
      fr = "Se rendent au travail en transport en commun"
    )),
    var_short = list(list(
      en = "Transit",
      fr = "Transit"
    )),
    description = list(list(
      en = "The number of employed persons whose main mode of commuting between home and work is public transit, such as bus, subway, light rail, streetcar or commuter train",
      fr = "Le nombre de personnes occupées dont le principal mode de transport pour la navette entre le domicile et le lieu de travail est le transport en commun, comme l'autobus, le métro, le train léger, le tramway ou le train de banlieue"
    )),
    parent_vec = "employment_lf",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_transit_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7645"),
    vec_2016 = list("v_CA16_5802"),
    vec_2011 = list("v_CA11N_2201"),
    vec_2006 = list("v_CA06_1112"),
    vec_2001 = list("v_CA01_1257"),
    vec_1996 = list("v_CA1996_1328"),
    var_title = list(list(
      en = "Public transit to work - Male",
      fr = "Se rendent au travail en transport en commun - Homme"
    )),
    var_short = list(list(
      en = "Transit M",
      fr = "Transit H"
    )),
    description = list(list(
      en = "The number of employed males whose main mode of commuting is public transit",
      fr = "Le nombre d'hommes occupés dont le principal mode de transport est le transport en commun"
    )),
    parent_vec = "employment_lf_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_transit_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7646"),
    vec_2016 = list("v_CA16_5803"),
    vec_2011 = list("v_CA11N_2202"),
    vec_2006 = list("v_CA06_1121"),
    vec_2001 = list("v_CA01_1266"),
    vec_1996 = list("v_CA1996_1337"),
    var_title = list(list(
      en = "Public transit to work - Female",
      fr = "Se rendent au travail en transport en commun - Femme"
    )),
    var_short = list(list(
      en = "Transit F",
      fr = "Transit F"
    )),
    description = list(list(
      en = "The number of employed females whose main mode of commuting is public transit",
      fr = "Le nombre de femmes occupées dont le principal mode de transport est le transport en commun"
    )),
    parent_vec = "employment_lf_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_15",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7659"),
    vec_2016 = list("v_CA16_5816"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute under 15 minutes",
      fr = "Trajets domicile-travail de moins de 15 minutes"
    )),
    var_short = list(list(
      en = "Commute <15m",
      fr = "Trajet <15m"
    )),
    description = list(list(
      en = "The number of employed persons whose usual one-way commute time between home and work is less than 15 minutes",
      fr = "Le nombre de personnes occupées dont le temps habituel de trajet aller entre le domicile et le lieu de travail est inférieur à 15 minutes"
    )),
    parent_vec = "employment_lf_dur",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_15_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7660"),
    vec_2016 = list("v_CA16_5817"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute under 15 minutes - Male",
      fr = "Trajets domicile-travail de moins de 15 minutes - Homme"
    )),
    var_short = list(list(
      en = "Commute <15m M",
      fr = "Trajet <15m H"
    )),
    description = list(list(
      en = "The number of employed males whose usual one-way commute time is less than 15 minutes",
      fr = "Le nombre d'hommes occupés dont le temps habituel de trajet aller est inférieur à 15 minutes"
    )),
    parent_vec = "employment_lf_dur_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_15_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7661"),
    vec_2016 = list("v_CA16_5818"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute under 15 minutes - Female",
      fr = "Trajets domicile-travail de moins de 15 minutes - Femme"
    )),
    var_short = list(list(
      en = "Commute <15m F",
      fr = "Trajet <15m F"
    )),
    description = list(list(
      en = "The number of employed females whose usual one-way commute time is less than 15 minutes",
      fr = "Le nombre de femmes occupées dont le temps habituel de trajet aller est inférieur à 15 minutes"
    )),
    parent_vec = "employment_lf_dur_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7662", "v_CA21_7665")),
    vec_2016 = list(c("v_CA16_5819", "v_CA16_5822")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute 15-45 minutes",
      fr = "Trajet domicile-travail de 15 à 45 minutes"
    )),
    var_short = list(list(
      en = "Commute 15-45m",
      fr = "Trajet 15-45m"
    )),
    description = list(list(
      en = "The number of employed persons whose usual one-way commute time between home and work is between 15 and 45 minutes",
      fr = "Le nombre de personnes occupées dont le temps habituel de trajet aller entre le domicile et le lieu de travail est compris entre 15 et 45 minutes"
    )),
    parent_vec = "employment_lf_dur",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7663", "v_CA21_7666")),
    vec_2016 = list(c("v_CA16_5820", "v_CA16_5823")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute 15-45 minutes - Male",
      fr = "Trajet domicile-travail de 15 à 45 minutes - Homme"
    )),
    var_short = list(list(
      en = "Commute 15-45 M",
      fr = "Trajet 15-45 H"
    )),
    description = list(list(
      en = "The number of employed males whose usual one-way commute time is between 15 and 45 minutes",
      fr = "Le nombre d'hommes occupés dont le temps habituel de trajet aller est compris entre 15 et 45 minutes"
    )),
    parent_vec = "employment_lf_dur_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7664", "v_CA21_7667")),
    vec_2016 = list(c("v_CA16_5821", "v_CA16_5824")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute 15-45 minutes - Female",
      fr = "Trajet domicile-travail de 15 à 45 minutes - Femme"
    )),
    var_short = list(list(
      en = "Commute 15-45 F",
      fr = "Trajet 15-45 F"
    )),
    description = list(list(
      en = "The number of employed females whose usual one-way commute time is between 15 and 45 minutes",
      fr = "Le nombre de femmes occupées dont le temps habituel de trajet aller est compris entre 15 et 45 minutes"
    )),
    parent_vec = "employment_lf_dur_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_plus",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7668", "v_CA21_7671")),
    vec_2016 = list(c("v_CA16_5825", "v_CA16_5828")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute more than 45 minutes",
      fr = "Trajet domicile-travail de plus de 45 minutes"
    )),
    var_short = list(list(
      en = "Commute >45m",
      fr = "Trajet >45m"
    )),
    description = list(list(
      en = "The number of employed persons whose usual one-way commute time between home and work is 45 minutes or more",
      fr = "Le nombre de personnes occupées dont le temps habituel de trajet aller entre le domicile et le lieu de travail est de 45 minutes ou plus"
    )),
    parent_vec = "employment_lf_dur",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_plus_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7669", "v_CA21_7672")),
    vec_2016 = list(c("v_CA16_5826", "v_CA16_5829")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute more than 45 minutes - Male",
      fr = "Trajet domicile-travail de plus de 45 minutes - Homme"
    )),
    var_short = list(list(
      en = "Commute >45m M",
      fr = "Trajet >45m H"
    )),
    description = list(list(
      en = "The number of employed males whose usual one-way commute time is 45 minutes or more",
      fr = "Le nombre d'hommes occupés dont le temps habituel de trajet aller est de 45 minutes ou plus"
    )),
    parent_vec = "employment_lf_dur_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "trans_t_45_plus_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list(c("v_CA21_7670", "v_CA21_7673")),
    vec_2016 = list(c("v_CA16_5827", "v_CA16_5830")),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Commute more than 45 minutes - Female",
      fr = "Trajet domicile-travail de plus de 45 minutes - Femme"
    )),
    var_short = list(list(
      en = "Commute >45m F",
      fr = "Trajet >45m F"
    )),
    description = list(list(
      en = "The number of employed females whose usual one-way commute time is 45 minutes or more",
      fr = "Le nombre de femmes occupées dont le temps habituel de trajet aller est de 45 minutes ou plus"
    )),
    parent_vec = "employment_lf_dur_f",
    parent = FALSE
  )

census_vectors_transport_parent <-
  tibble::tibble(
    var_code = "employment_lf",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7632"),
    vec_2016 = list("v_CA16_5792"),
    vec_2011 = list("v_CA11N_2191"),
    vec_2006 = list("v_CA06_1100"),
    vec_2001 = list(c("v_CA01_1253")),
    vec_1996 = list("v_CA1996_1324"),
    var_title = list(list(
      en = "Employed individuals",
      fr = "Personnes employées"
    )),
    var_short = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    description = list(list(
      en = "The number of employed persons aged 15 years and over in private households who have a usual place of work or no fixed workplace address",
      fr = "Le nombre de personnes occupées âgées de 15 ans et plus, vivant dans des ménages privés, qui ont un lieu habituel de travail ou n'ont pas d'adresse de travail fixe"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lf_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7633"),
    vec_2016 = list("v_CA16_5793"),
    vec_2011 = list("v_CA11N_2192"),
    vec_2006 = list("v_CA06_1109"),
    vec_2001 = list("v_CA01_1254"),
    vec_1996 = list("v_CA1996_1325"),
    var_title = list(list(
      en = "Employed individuals - Male",
      fr = "Personnes employées - Homme"
    )),
    var_short = list(list(
      en = "Employed M",
      fr = "Employés H"
    )),
    description = list(list(
      en = "The number of employed males aged 15 years and over with a usual place of work or no fixed workplace address",
      fr = "Le nombre d'hommes occupés âgés de 15 ans et plus ayant un lieu habituel de travail ou n'ayant pas d'adresse de travail fixe"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lf_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7634"),
    vec_2016 = list("v_CA16_5794"),
    vec_2011 = list("v_CA11N_2193"),
    vec_2006 = list("v_CA06_1118"),
    vec_2001 = list("v_CA01_1263"),
    vec_1996 = list("v_CA1996_1334"),
    var_title = list(list(
      en = "Employed individuals - Female",
      fr = "Personnes employées - Femme"
    )),
    var_short = list(list(
      en = "Employed F",
      fr = "Employées F"
    )),
    description = list(list(
      en = "The number of employed females aged 15 years and over with a usual place of work or no fixed workplace address",
      fr = "Le nombre de femmes occupées âgées de 15 ans et plus ayant un lieu habituel de travail ou n'ayant pas d'adresse de travail fixe"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lf_dur",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7656"),
    vec_2016 = list("v_CA16_5813"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Employed individuals - commuting duration",
      fr = "Personnes employées - durée du trajet"
    )),
    var_short = list(list(
      en = "Employed dur.",
      fr = "Employés dur."
    )),
    description = list(list(
      en = "The number of employed persons aged 15 years and over with a usual place of work or no fixed workplace address by commuting duration",
      fr = "Le nombre de personnes occupées âgées de 15 ans et plus ayant un lieu habituel de travail ou n'ayant pas d'adresse de travail fixe, selon la durée du trajet"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lf_dur_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7657"),
    vec_2016 = list("v_CA16_5814"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Employed individuals - commuting duration - Male",
      fr = "Personnes employées - durée du trajet - Homme"
    )),
    var_short = list(list(
      en = "Employed dur. M",
      fr = "Employés dur. H"
    )),
    description = list(list(
      en = "The number of employed males aged 15 years and over with a usual place of work or no fixed workplace address by commuting duration",
      fr = "Le nombre d'hommes occupés âgés de 15 ans et plus ayant un lieu habituel de travail ou n'ayant pas d'adresse de travail fixe, selon la durée du trajet"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lf_dur_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Transport",
    vec_2021 = list("v_CA21_7658"),
    vec_2016 = list("v_CA16_5815"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Employed individuals - commuting duration - Female",
      fr = "Personnes employées - durée du trajet - Femme"
    )),
    var_short = list(list(
      en = "Employed dur. F",
      fr = "Employées dur F"
    )),
    description = list(list(
      en = "The number of employed females aged 15 years and over with a usual place of work or no fixed workplace address by commuting duration",
      fr = "Le nombre de femmes occupées âgées de 15 ans et plus ayant un lieu habituel de travail ou n'ayant pas d'adresse de travail fixe, selon la durée du trajet"
    )),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(
  vectors_df = census_vectors_transport,
  parents_df = census_vectors_transport_parent
)

census_vectors_transport <- rbind(
  census_vectors_transport,
  census_vectors_transport_parent
)

usethis::use_data(census_vectors_transport, overwrite = TRUE)

## IMPORT EMPLOYMENT CENSUS VECTORS ############################################

census_vectors_employment <-
  tibble::tibble(
    var_code = "employment_er",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6498"),
    vec_2016 = list("v_CA16_5603"),
    vec_2011 = list("v_CA11N_1993"),
    vec_2006 = list("v_CA06_577"),
    vec_2001 = list("v_CA01_737"),
    vec_1996 = list("v_CA1996_799"),
    var_title = list(list(
      en = "Employed individuals (15+)",
      fr = "Personnes en emploi (15 ans et plus)"
    )),
    var_short = list(list(
      en = "Employed",
      fr = "En emploi"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over in the labour force who, during the reference week, did any work for pay or profit or had a job but were absent from work.",
      fr = "Le nombre de personnes de 15 ans et plus faisant partie de la population active qui, au cours de la semaine de référence, ont travaillé contre rémunération ou en profit ou avaient un emploi mais étaient absentes du travail."
    )),
    parent_vec = "employment_15older",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_er_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6499"),
    vec_2016 = list("v_CA16_5604"),
    vec_2011 = list("v_CA11N_1994"),
    vec_2006 = list("v_CA06_601"),
    vec_2001 = list("v_CA01_761"),
    vec_1996 = list("v_CA1996_823"),
    var_title = list(list(
      en = "Employed individuals (15+) - Male",
      fr = "Personnes en emploi (15 ans et plus) - Homme"
    )),
    var_short = list(list(
      en = "Employed M",
      fr = "En emploi H"
    )),
    description = list(list(
      en = "The number of males aged 15 years and over in the labour force who, during the reference week, did any work for pay or profit or had a job but were absent from work.",
      fr = "Le nombre d'hommes de 15 ans et plus faisant partie de la population active qui, au cours de la semaine de référence, ont travaillé contre rémunération ou en profit ou avaient un emploi mais étaient absents du travail."
    )),
    parent_vec = "employment_15older_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_er_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6500"),
    vec_2016 = list("v_CA16_5605"),
    vec_2011 = list("v_CA11N_1995"),
    vec_2006 = list("v_CA06_625"),
    vec_2001 = list("v_CA01_785"),
    vec_1996 = list("v_CA1996_847"),
    var_title = list(list(
      en = "Employed individuals (15+) - Female",
      fr = "Personnes en emploi (15 ans et plus) - Femme"
    )),
    var_short = list(list(
      en = "Employed F",
      fr = "En emploi F"
    )),
    description = list(list(
      en = "The number of females aged 15 years and over in the labour force who, during the reference week, did any work for pay or profit or had a job but were absent from work.",
      fr = "Le nombre de femmes de 15 ans et plus faisant partie de la population active qui, au cours de la semaine de référence, ont travaillé contre rémunération ou en profit ou avaient un emploi mais étaient absentes du travail."
    )),
    parent_vec = "employment_15older_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_uer",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6501"),
    vec_2016 = list("v_CA16_5606"),
    vec_2011 = list("v_CA11N_1996"),
    vec_2006 = list("v_CA06_578"),
    vec_2001 = list("v_CA01_738"),
    vec_1996 = list("v_CA1996_800"),
    var_title = list(list(
      en = "Unemployed individuals (15+)",
      fr = "Personnes en chômage (15 ans et plus)"
    )),
    var_short = list(list(
      en = "Unemployed",
      fr = "Chômeurs"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over in the labour force who were without work, available for work and looking for work, or on temporary lay-off with an expectation of recall.",
      fr = "Le nombre de personnes de 15 ans et plus faisant partie de la population active qui étaient sans travail, disponibles pour travailler et à la recherche d'un emploi, ou mises à pied temporairement en attente de rappel."
    )),
    parent_vec = "employment_15older",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_uer_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6502"),
    vec_2016 = list("v_CA16_5607"),
    vec_2011 = list("v_CA11N_1997"),
    vec_2006 = list("v_CA06_602"),
    vec_2001 = list("v_CA01_762"),
    vec_1996 = list("v_CA1996_824"),
    var_title = list(list(
      en = "Unemployed individuals (15+) - Male",
      fr = "Personnes en chômage (15 ans et plus) - Homme"
    )),
    var_short = list(list(
      en = "Unemployed M",
      fr = "Chômeurs H"
    )),
    description = list(list(
      en = "The number of males aged 15 years and over in the labour force who were without work, available for work and looking for work, or on temporary lay-off with an expectation of recall.",
      fr = "Le nombre d'hommes de 15 ans et plus faisant partie de la population active qui étaient sans travail, disponibles pour travailler et à la recherche d'un emploi, ou mis à pied temporairement en attente de rappel."
    )),
    parent_vec = "employment_15older_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_uer_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6503"),
    vec_2016 = list("v_CA16_5608"),
    vec_2011 = list("v_CA11N_1998"),
    vec_2006 = list("v_CA06_626"),
    vec_2001 = list("v_CA01_786"),
    vec_1996 = list("v_CA1996_848"),
    var_title = list(list(
      en = "Unemployed individuals (15+) - Female",
      fr = "Personnes en chômage (15 ans et plus) - Femme"
    )),
    var_short = list(list(
      en = "Unemployed F",
      fr = "Chômeuses F"
    )),
    description = list(list(
      en = "The number of females aged 15 years and over in the labour force who were without work, available for work and looking for work, or on temporary lay-off with an expectation of recall.",
      fr = "Le nombre de femmes de 15 ans et plus faisant partie de la population active qui étaient sans travail, disponibles pour travailler et à la recherche d'un emploi, ou mises à pied temporairement en attente de rappel."
    )),
    parent_vec = "employment_15older_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powoutmun",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list(c("v_CA21_7623", "v_CA21_7626", "v_CA21_7629")),
    vec_2016 = list(c("v_CA16_5783", "v_CA16_5786", "v_CA16_5789")),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1079"),
    vec_2001 = list(c("v_CA01_1240", "v_CA01_1248")),
    vec_1996 = list(c("v_CA1996_1311", "v_CA1996_1319")),
    var_title = list(list(
      en = "Work outside municipality of residence",
      fr = "Travail hors de la municipalité de résidence"
    )),
    var_short = list(list(
      en = "Outside",
      fr = "Hors"
    )),
    description = list(list(
      en = "The number of employed persons with a usual place of work whose workplace is located outside their municipality (census subdivision) of residence.",
      fr = "Le nombre de personnes occupées ayant un lieu habituel de travail dont le lieu de travail est situé à l'extérieur de leur municipalité (subdivision de recensement) de résidence."
    )),
    parent_vec = "employment_lfupow",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powoutmun_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list(c("v_CA21_7624", "v_CA21_7627", "v_CA21_7630")),
    vec_2016 = list(c("v_CA16_5784", "v_CA16_5787", "v_CA16_5790")),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1087"),
    vec_2001 = list("v_CA01_1240"),
    vec_1996 = list("v_CA1996_1311"),
    var_title = list(list(
      en = "Work outside municipality of residence - Male",
      fr = "Travail hors de la municipalité de résidence - Homme"
    )),
    var_short = list(list(
      en = "Outside M",
      fr = "Hors H"
    )),
    description = list(list(
      en = "The number of employed males with a usual place of work whose workplace is located outside their municipality of residence.",
      fr = "Le nombre d'hommes occupés ayant un lieu habituel de travail dont le lieu de travail est situé à l'extérieur de leur municipalité de résidence."
    )),
    parent_vec = "employment_lfupow_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powoutmun_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list(c("v_CA21_7625", "v_CA21_7628", "v_CA21_7631")),
    vec_2016 = list(c("v_CA16_5785", "v_CA16_5788", "v_CA16_5791")),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1095"),
    vec_2001 = list("v_CA01_1248"),
    vec_1996 = list("v_CA1996_1319"),
    var_title = list(list(
      en = "Work outside municipality of residence - Female",
      fr = "Travail hors de la municipalité de résidence - Femme"
    )),
    var_short = list(list(
      en = "Outside F",
      fr = "Hors F"
    )),
    description = list(list(
      en = "The number of employed females with a usual place of work whose workplace is located outside their municipality of residence.",
      fr = "Le nombre de femmes occupées ayant un lieu habituel de travail dont le lieu de travail est situé à l'extérieur de leur municipalité de résidence."
    )),
    parent_vec = "employment_lfupow_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powinmun",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7620"),
    vec_2016 = list("v_CA16_5780"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1078"),
    vec_2001 = list(c("v_CA01_1239", "v_CA01_1247")),
    vec_1996 = list(c("v_CA1996_1310", "v_CA1996_1318")),
    var_title = list(list(
      en = "Work in municipality of residence",
      fr = "Travail dans la municipalité de résidence"
    )),
    var_short = list(list(
      en = "Within",
      fr = "Intérieur"
    )),
    description = list(list(
      en = "The number of employed persons with a usual place of work whose workplace is located in the same municipality (census subdivision) as their place of residence.",
      fr = "Le nombre de personnes occupées ayant un lieu habituel de travail dont le lieu de travail est situé dans la même municipalité (subdivision de recensement) que leur lieu de résidence."
    )),
    parent_vec = "employment_lfupow",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powinmun_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7621"),
    vec_2016 = list("v_CA16_5781"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1086"),
    vec_2001 = list("v_CA01_1239"),
    vec_1996 = list("v_CA1996_1310"),
    var_title = list(list(
      en = "Work in municipality of residence - Male",
      fr = "Travail dans la municipalité de résidence - Homme"
    )),
    var_short = list(list(
      en = "Within M",
      fr = "Intérieur H"
    )),
    description = list(list(
      en = "The number of employed males with a usual place of work whose workplace is located in the same municipality as their place of residence.",
      fr = "Le nombre d'hommes occupés ayant un lieu habituel de travail dont le lieu de travail est situé dans la même municipalité que leur lieu de résidence."
    )),
    parent_vec = "employment_lfupow_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powinmun_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7622"),
    vec_2016 = list("v_CA16_5782"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1094"),
    vec_2001 = list("v_CA01_1247"),
    vec_1996 = list("v_CA1996_1318"),
    var_title = list(list(
      en = "Work in municipality of residence - Female",
      fr = "Travail dans la municipalité de résidence - Femme"
    )),
    var_short = list(list(
      en = "Within F",
      fr = "Intérieur F"
    )),
    description = list(list(
      en = "The number of employed females with a usual place of work whose workplace is located in the same municipality as their place of residence.",
      fr = "Le nombre de femmes occupées ayant un lieu habituel de travail dont le lieu de travail est situé dans la même municipalité que leur lieu de résidence."
    )),
    parent_vec = "employment_lfupow_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powhome",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7605"),
    vec_2016 = list("v_CA16_5765"),
    vec_2011 = list("v_CA11N_2179"),
    vec_2006 = list("v_CA06_1081"),
    vec_2001 = list(c("v_CA01_1242", "v_CA01_1250")),
    vec_1996 = list(c("v_CA1996_1313", "v_CA1996_1321")),
    var_title = list(list(
      en = "Work at home",
      fr = "Travail à domicile"
    )),
    var_short = list(list(
      en = "Home",
      fr = "Domicile"
    )),
    description = list(list(
      en = "The number of employed persons in the labour force who usually work at home.",
      fr = "Le nombre de personnes occupées faisant partie de la population active qui travaillent habituellement à domicile."
    )),
    parent_vec = "employment_em",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powhome_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7606"),
    vec_2016 = list("v_CA16_5766"),
    vec_2011 = list("v_CA11N_2180"),
    vec_2006 = list("v_CA06_1089"),
    vec_2001 = list("v_CA01_1242"),
    vec_1996 = list("v_CA1996_1313"),
    var_title = list(list(
      en = "Work at home - Male",
      fr = "Travail à domicile - Homme"
    )),
    var_short = list(list(
      en = "Home M",
      fr = "Domicile H"
    )),
    description = list(list(
      en = "The number of employed males in the labour force who usually work at home.",
      fr = "Le nombre d'hommes occupés faisant partie de la population active qui travaillent habituellement à domicile."
    )),
    parent_vec = "employment_em_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "employment_powhome_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7607"),
    vec_2016 = list("v_CA16_5767"),
    vec_2011 = list("v_CA11N_2181"),
    vec_2006 = list("v_CA06_1097"),
    vec_2001 = list("v_CA01_1250"),
    vec_1996 = list("v_CA1996_1321"),
    var_title = list(list(
      en = "Work at home - Female",
      fr = "Travail à domicile - Femme"
    )),
    var_short = list(list(
      en = "Home F",
      fr = "Domicile F"
    )),
    description = list(list(
      en = "The number of employed females in the labour force who usually work at home.",
      fr = "Le nombre de femmes occupées faisant partie de la population active qui travaillent habituellement à domicile."
    )),
    parent_vec = "employment_em_f",
    parent = FALSE
  )

## PARENTS #####################################################################

census_vectors_employment_parent <-
  tibble::tibble(
    var_code = "employment_15older",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6492"),
    vec_2016 = list("v_CA16_5597"),
    vec_2011 = list("v_CA11N_1987"),
    vec_2006 = list("v_CA06_575"),
    vec_2001 = list("v_CA01_735"),
    vec_1996 = list("v_CA1996_797"),
    var_title = list(list(
      en = "Individuals aged 15 years and over",
      fr = "Personnes de 15 ans et plus"
    )),
    var_short = list(list(
      en = "Pop 15+",
      fr = "Pop 15+"
    )),
    description = list(list(
      en = "The total population aged 15 years and over in private households.",
      fr = "La population totale de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_15older_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6493"),
    vec_2016 = list("v_CA16_5598"),
    vec_2011 = list("v_CA11N_1988"),
    vec_2006 = list("v_CA06_599"),
    vec_2001 = list("v_CA01_759"),
    vec_1996 = list("v_CA1996_821"),
    var_title = list(list(
      en = "Individuals aged 15 years and over - Male",
      fr = "Personnes de 15 ans et plus - Homme"
    )),
    var_short = list(list(
      en = "Pop 15+ M",
      fr = "Pop 15+ H"
    )),
    description = list(list(
      en = "The total male population aged 15 years and over in private households.",
      fr = "La population masculine totale de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_15older_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6494"),
    vec_2016 = list("v_CA16_5599"),
    vec_2011 = list("v_CA11N_1989"),
    vec_2006 = list("v_CA06_623"),
    vec_2001 = list("v_CA01_783"),
    vec_1996 = list("v_CA1996_845"),
    var_title = list(list(
      en = "Individuals aged 15 years and over - Female",
      fr = "Personnes de 15 ans et plus - Femme"
    )),
    var_short = list(list(
      en = "Pop 15+ F",
      fr = "Pop 15+ F"
    )),
    description = list(list(
      en = "The total female population aged 15 years and over in private households.",
      fr = "La population féminine totale de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lfupow",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7617"),
    vec_2016 = list("v_CA16_5777"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1077"),
    vec_2001 = list(c("v_CA01_1238", "v_CA01_1246")),
    vec_1996 = list(c("v_CA1996_1309", "v_CA1996_1317")),
    var_title = list(list(
      en = "Individuals employed in the labour force with a usual place of work",
      fr = "Personnes employées dans la population active ayant un lieu de travail habituel"
    )),
    var_short = list(list(
      en = "Usual workplace",
      fr = "Habituel"
    )),
    description = list(list(
      en = "The number of employed individuals with a usual place of work",
      fr = "Le nombre d'individus à l'emploi ayant un lieu de travail habituel"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lfupow_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7618"),
    vec_2016 = list("v_CA16_5778"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1085"),
    vec_2001 = list("v_CA01_1238"),
    vec_1996 = list("v_CA1996_1309"),
    var_title = list(list(
      en = "Employed individuals with a usual place of work - Male",
      fr = "Personnes employées ayant un lieu de travail habituel - Homme"
    )),
    var_short = list(list(
      en = "Usual place M",
      fr = "Habituel H"
    )),
    description = list(list(
      en = "The number of employed males with a usual place of work.",
      fr = "Le nombre d'hommes occupés ayant un lieu de travail habituel."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_lfupow_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7619"),
    vec_2016 = list("v_CA16_5779"),
    vec_2011 = list(NA),
    vec_2006 = list("v_CA06_1093"),
    vec_2001 = list("v_CA01_1246"),
    vec_1996 = list("v_CA1996_1317"),
    var_title = list(list(
      en = "Employed individuals with a usual place of work - Female",
      fr = "Personnes employées ayant un lieu de travail habituel - Femme"
    )),
    var_short = list(list(
      en = "Usual place F",
      fr = "Habituel F"
    )),
    description = list(list(
      en = "The number of employed females with a usual place of work.",
      fr = "Le nombre de femmes occupées ayant un lieu de travail habituel."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_em",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_6498"),
    vec_2016 = list("v_CA16_5603"),
    vec_2011 = list("v_CA11N_1993"),
    vec_2006 = list("v_CA06_577"),
    vec_2001 = list("v_CA01_737"),
    vec_1996 = list("v_CA1996_799"),
    var_title = list(list(
      en = "Employed individuals in the labour force",
      fr = "Personnes en emploi dans la population active"
    )),
    var_short = list(list(
      en = "Employed",
      fr = "Employé"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over in the labour force who are employed.",
      fr = "Le nombre de personnes de 15 ans et plus faisant partie de la population active qui sont en emploi."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_em_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7603"),
    vec_2016 = list("v_CA16_5763"),
    vec_2011 = list("v_CA11N_2177"),
    vec_2006 = list("v_CA06_1084"),
    vec_2001 = list("v_CA01_1237"),
    vec_1996 = list("v_CA1996_1308"),
    var_title = list(list(
      en = "Employed individuals in the labour force - Male",
      fr = "Personnes en emploi dans la population active - Homme"
    )),
    var_short = list(list(
      en = "Employed M",
      fr = "Employé H"
    )),
    description = list(list(
      en = "The number of males aged 15 years and over in the labour force who are employed.",
      fr = "Le nombre d'hommes de 15 ans et plus faisant partie de la population active qui sont en emploi."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "employment_em_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Employment",
    vec_2021 = list("v_CA21_7604"),
    vec_2016 = list("v_CA16_5764"),
    vec_2011 = list("v_CA11N_2178"),
    vec_2006 = list("v_CA06_1092"),
    vec_2001 = list("v_CA01_1245"),
    vec_1996 = list("v_CA1996_1316"),
    var_title = list(list(
      en = "Employed individuals in the labour force - Female",
      fr = "Personnes en emploi dans la population active - Femme"
    )),
    var_short = list(list(
      en = "Employed F",
      fr = "Employée F"
    )),
    description = list(list(
      en = "The number of females aged 15 years and over in the labour force who are employed.",
      fr = "Le nombre de femmes de 15 ans et plus faisant partie de la population active qui sont en emploi."
    )),
    parent_vec = NA,
    parent = TRUE
  )

# addition of needed parent available from the transport vectors
from_transport <- census_vectors_transport_parent[
  census_vectors_transport_parent$var_code %in%
    census_vectors_employment$parent_vec,
]

verify_parents(
  vectors_df = census_vectors_employment,
  parents_df = rbind(census_vectors_employment_parent, from_transport)
)

census_vectors_employment <- rbind(
  census_vectors_employment,
  census_vectors_employment_parent
)

usethis::use_data(census_vectors_employment, overwrite = TRUE)

## IMPORT FAMILY CENSUS VECTORS ############################################

census_vectors_family <-
  tibble::tibble(
    var_code = "family_children",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Household",
    vec_2021 = list(c("v_CA21_502", "v_CA21_505")),
    vec_2016 = list(c(
      "v_CA16_494",
      "v_CA16_495",
      "v_CA16_496",
      "v_CA16_498",
      "v_CA16_499",
      "v_CA16_500"
    )),
    vec_2011 = list(c("v_CA11F_129", "v_CA11F_119", "v_CA11F_125")),
    vec_2006 = list(c("v_CA06_65", "v_CA06_59", "v_CA06_69")),
    vec_2001 = list(c("v_CA01_63", "v_CA01_57", "v_CA01_67")),
    vec_1996 = list(c("v_CA1996_68", "v_CA1996_74", "v_CA1996_78")),
    var_title = list(list(
      en = "Families with children",
      fr = "Familles avec enfants"
    )),
    var_short = list(list(
      en = "With child",
      fr = "Avec enfants"
    )),
    description = list(list(
      en = "The number of census families in private households with at least one child living in the same dwelling",
      fr = "Le nombre de familles de recensement dans les ménages privés comptant au moins un enfant vivant dans le même logement"
    )),
    parent_vec = "census_families",
    parent = FALSE
  )

census_vectors_family_parent <-
  tibble::tibble(
    var_code = "census_families",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Household",
    vec_2021 = list("v_CA21_499"),
    vec_2016 = list("v_CA16_478"),
    vec_2011 = list("v_CA11F_110"),
    vec_2006 = list("v_CA06_50"),
    vec_2001 = list("v_CA01_53"),
    vec_1996 = list("v_CA1996_60"),
    var_title = list(list(
      en = "Census families",
      fr = "Familles de recensement"
    )),
    var_short = list(list(
      en = "Families",
      fr = "Familles"
    )),
    description = list(list(
      en = "The number of census families in private households, defined as couples (married or common-law) with or without children, or lone parents with at least one child in the same dwelling",
      fr = "Le nombre de familles de recensement dans les ménages privés, définies comme des couples mariés ou en union libre avec ou sans enfants, ou des parents seuls avec au moins un enfant vivant dans le même logement"
    )),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(
  vectors_df = census_vectors_family,
  parents_df = census_vectors_family_parent
)

census_vectors_family <- rbind(
  census_vectors_family,
  census_vectors_family_parent
)

usethis::use_data(census_vectors_family, overwrite = TRUE)

## IMPORT LANGUAGE CENSUS VECTORS ##############################################

census_vectors_language <-
  tibble::tibble(
    var_code = "lang_french_only",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1150"),
    vec_2016 = list("v_CA16_518"),
    vec_2011 = list("v_CA11F_557"),
    vec_2006 = list("v_CA06_245"),
    vec_2001 = list("v_CA01_215"),
    vec_1996 = list("v_CA1996_312"),
    var_title = list(list(
      en = "Knows French only",
      fr = "Connaissent seulement le français"
    )),
    var_short = list(list(
      en = "Fr. only",
      fr = "Fr. seulement"
    )),
    description = list(list(
      en = "The number of individuals who know only French as an official language",
      fr = "Nombre de personnes qui connaissent seulement le français comme langue officielle"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_only_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1151"),
    vec_2016 = list("v_CA16_519"),
    vec_2011 = list("v_CA11F_558"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows French only - Male",
      fr = "Connaissent seulement le français - Homme"
    )),
    var_short = list(list(en = "Fr. only M", fr = "Fr. seulement H")),
    description = list(list(
      en = "The number of males who know only French as an official language",
      fr = "Nombre d'hommes qui connaissent seulement le français comme langue officielle"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_only_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1152"),
    vec_2016 = list("v_CA16_520"),
    vec_2011 = list("v_CA11F_559"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows French only - Female",
      fr = "Connaissent seulement le français - Femme"
    )),
    var_short = list(list(en = "Fr. only F", fr = "Fr. seulement F")),
    description = list(list(
      en = "The number of females who know only French as an official language",
      fr = "Nombre de femmes qui connaissent seulement le français comme langue officielle"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_eng_only",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1147"),
    vec_2016 = list("v_CA16_515"),
    vec_2011 = list("v_CA11F_554"),
    vec_2006 = list("v_CA06_244"),
    vec_2001 = list("v_CA01_214"),
    vec_1996 = list("v_CA1996_311"),
    var_title = list(list(
      en = "Knows English only",
      fr = "Connaissent seulement l'anglais"
    )),
    var_short = list(list(
      en = "Eng. only",
      fr = "Ang. seulement"
    )),
    description = list(list(
      en = "The number of individuals who know only English as an official language",
      fr = "Nombre de personnes qui connaissent seulement l'anglais comme langue officielle"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_eng_only_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1148"),
    vec_2016 = list("v_CA16_516"),
    vec_2011 = list("v_CA11F_555"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows English only - Male",
      fr = "Connaissent seulement l'anglais - Homme"
    )),
    var_short = list(list(en = "Eng. only M", fr = "Ang seulement H")),
    description = list(list(
      en = "The number of males who know only English as an official language",
      fr = "Nombre d'hommes qui connaissent seulement l'anglais comme langue officielle"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_eng_only_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1149"),
    vec_2016 = list("v_CA16_517"),
    vec_2011 = list("v_CA11F_556"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows English only - Female",
      fr = "Connaissent seulement l'anglais - Femme"
    )),
    var_short = list(list(en = "Eng. only F", fr = "Ang seulement F")),
    description = list(list(
      en = "The number of females who know only English as an official language",
      fr = "Nombre de femmes qui connaissent seulement l'anglais comme langue officielle"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_eng",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1153"),
    vec_2016 = list("v_CA16_521"),
    vec_2011 = list("v_CA11F_560"),
    vec_2006 = list("v_CA06_246"),
    vec_2001 = list("v_CA01_216"),
    vec_1996 = list("v_CA1996_313"),
    var_title = list(list(
      en = "Knows French and English",
      fr = "Connaissent le français et l'anglais"
    )),
    var_short = list(list(
      en = "Fr. and Eng.",
      fr = "Fr. et ang."
    )),
    description = list(list(
      en = "The number of individuals who know both of the official languages (French and English)",
      fr = "Nombre de personnes qui connaissent les deux langues officielles (français et anglais)"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_eng_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1154"),
    vec_2016 = list("v_CA16_522"),
    vec_2011 = list("v_CA11F_561"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows French and English - Male",
      fr = "Connaissent le français et l'anglais - Homme"
    )),
    var_short = list(list(en = "Fr. and Eng. M", fr = "Fr. et ang. H")),
    description = list(list(
      en = "The number of males who know both official languages (French and English)",
      fr = "Nombre d'hommes qui connaissent les deux langues officielles (français et anglais)"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_french_eng_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1155"),
    vec_2016 = list("v_CA16_523"),
    vec_2011 = list("v_CA11F_562"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows French and English - Female",
      fr = "Connaissent le français et l'anglais - Femme"
    )),
    var_short = list(list(en = "Fr. and Eng. F", fr = "Fr. et ang. F")),
    description = list(list(
      en = "The number of females who know both official languages (French and English)",
      fr = "Nombre de femmes qui connaissent les deux langues officielles (français et anglais)"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_no_official",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1156"),
    vec_2016 = list("v_CA16_524"),
    vec_2011 = list("v_CA11F_563"),
    vec_2006 = list("v_CA06_247"),
    vec_2001 = list("v_CA01_217"),
    vec_1996 = list("v_CA1996_314"),
    var_title = list(list(
      en = "Knows neither French nor English",
      fr = "Ne connaissent ni le français ni l'anglais"
    )),
    var_short = list(list(
      en = "Non-official",
      fr = "Non officielle"
    )),
    description = list(list(
      en = "The number of individuals who do not know either of the official languages (French or English)",
      fr = "Nombre de personnes qui ne connaissent ni le français ni l'anglais, les deux langues officielles"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_no_official_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1157"),
    vec_2016 = list("v_CA16_525"),
    vec_2011 = list("v_CA11F_564"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows neither French nor English - Male",
      fr = "Ne connaissent ni le français ni l'anglais - Homme"
    )),
    var_short = list(list(en = "Non-official M", fr = "Non off H")),
    description = list(list(
      en = "The number of males who do not know either of the official languages",
      fr = "Nombre d'hommes qui ne connaissent ni le français ni l'anglais"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_no_official_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_1158"),
    vec_2016 = list("v_CA16_526"),
    vec_2011 = list("v_CA11F_565"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Knows neither French nor English - Female",
      fr = "Ne connaissent ni le français ni l'anglais - Femme"
    )),
    var_short = list(list(en = "Non-official F", fr = "Non off F")),
    description = list(list(
      en = "The number of females who do not know either of the official languages",
      fr = "Nombre de femmes qui ne connaissent ni le français ni l'anglais"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_eng",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2209"),
    vec_2016 = list("v_CA16_1364"),
    vec_2011 = list("v_CA11F_593"),
    vec_2006 = list("v_CA06_257"),
    vec_2001 = list("v_CA01_227"),
    vec_1996 = list("v_CA1996_324"),
    var_title = list(list(
      en = "Most often speak English at home",
      fr = "Parle le plus souvent l'anglais à la maison"
    )),
    var_short = list(list(
      en = "English",
      fr = "Anglais"
    )),
    description = list(list(
      en = "The number of individuals who report English as the language spoken most often at home",
      fr = "Nombre de personnes qui déclarent l'anglais comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_eng_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2210"),
    vec_2016 = list("v_CA16_1365"),
    vec_2011 = list("v_CA11F_594"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak English at home - Male",
      fr = "Parle le plus souvent l'anglais à la maison - Homme"
    )),
    var_short = list(list(en = "English M", fr = "Anglais H")),
    description = list(list(
      en = "The number of males who report English as the language spoken most often at home",
      fr = "Nombre d'hommes qui déclarent l'anglais comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_eng_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2211"),
    vec_2016 = list("v_CA16_1366"),
    vec_2011 = list("v_CA11F_595"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak English at home - Female",
      fr = "Parle le plus souvent l'anglais à la maison - Femme"
    )),
    var_short = list(list(en = "English F", fr = "Anglais F")),
    description = list(list(
      en = "The number of females who report English as the language spoken most often at home",
      fr = "Nombre de femmes qui déclarent l'anglais comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_fr",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2212"),
    vec_2016 = list("v_CA16_1367"),
    vec_2011 = list("v_CA11F_596"),
    vec_2006 = list("v_CA06_258"),
    vec_2001 = list("v_CA01_228"),
    vec_1996 = list("v_CA1996_325"),
    var_title = list(list(
      en = "Most often speak French at home",
      fr = "Parle le plus souvent le français à la maison"
    )),
    var_short = list(list(
      en = "French",
      fr = "Français"
    )),
    description = list(list(
      en = "The number of individuals who report French as the language spoken most often at home",
      fr = "Nombre de personnes qui déclarent le français comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_fr_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2213"),
    vec_2016 = list("v_CA16_1368"),
    vec_2011 = list("v_CA11F_597"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak French at home - Male",
      fr = "Parle le plus souvent le français à la maison - Homme"
    )),
    var_short = list(list(en = "French M", fr = "Français H")),
    description = list(list(
      en = "The number of males who report French as the language spoken most often at home",
      fr = "Nombre d'hommes qui déclarent le français comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_fr_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2214"),
    vec_2016 = list("v_CA16_1369"),
    vec_2011 = list("v_CA11F_598"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak French at home - Female",
      fr = "Parle le plus souvent le français à la maison - Femme"
    )),
    var_short = list(list(en = "French F", fr = "Français F")),
    description = list(list(
      en = "The number of females who report French as the language spoken most often at home",
      fr = "Nombre de femmes qui déclarent le français comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_nonof",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2215"),
    vec_2016 = list("v_CA16_1370"),
    vec_2011 = list("v_CA11F_599"),
    vec_2006 = list("v_CA06_259"),
    vec_2001 = list("v_CA01_229"),
    vec_1996 = list("v_CA1996_326"),
    var_title = list(list(
      en = "Most often speak a non-official language at home",
      fr = "Parle le plus souvent une langue non officielle à la maison"
    )),
    var_short = list(list(
      en = "Non-official",
      fr = "Non officielle"
    )),
    description = list(list(
      en = "The number of individuals who report a non-official language as the language spoken most often at home",
      fr = "Nombre de personnes qui déclarent une langue non officielle comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_nonof_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2216"),
    vec_2016 = list("v_CA16_1371"),
    vec_2011 = list("v_CA11F_600"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak a non-official language at home - Male",
      fr = "Parle le plus souvent une langue non officielle à la maison - Homme"
    )),
    var_short = list(list(en = "Non-official M", fr = "Non off H")),
    description = list(list(
      en = "The number of males who report a non-official language as the language spoken most often at home",
      fr = "Nombre d'hommes qui déclarent une langue non officielle comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "lang_home_nonof_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Language",
    vec_2021 = list("v_CA21_2217"),
    vec_2016 = list("v_CA16_1372"),
    vec_2011 = list("v_CA11F_601"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Most often speak a non-official language at home - Female",
      fr = "Parle le plus souvent une langue non officielle à la maison - Femme"
    )),
    var_short = list(list(en = "Non-official F", fr = "Non off F")),
    description = list(list(
      en = "The number of females who report a non-official language as the language spoken most often at home",
      fr = "Nombre de femmes qui déclarent une langue non officielle comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population_f",
    parent = FALSE
  )

census_vectors_language_parent <-
  tibble::tibble(
    var_code = "c_population",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_1"),
    vec_2016 = list("v_CA16_401"),
    vec_2011 = list("v_CA11F_1"),
    vec_2006 = list("v_CA06_1"),
    vec_2001 = list("v_CA01_2"),
    vec_1996 = list("v_CA1996_2"),
    var_title = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    var_short = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    description = list(list(
      en = "The total count of individuals",
      fr = "Le nombre total d'individus"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "c_population_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_9"),
    vec_2016 = list("v_CA16_2"),
    vec_2011 = list("v_CA11F_6"),
    vec_2006 = list("v_CA06_3"),
    vec_2001 = list("v_CA01_6"),
    vec_1996 = list("v_CA1996_6"),
    var_title = list(list(en = "Male individuals", fr = "Individus - Homme")),
    var_short = list(list(en = "Males", fr = "Hommes")),
    description = list(list(
      en = "The total count of male individuals",
      fr = "Le nombre total d'individus de sexe masculin"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "c_population_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_10"),
    vec_2016 = list("v_CA16_3"),
    vec_2011 = list("v_CA11F_7"),
    vec_2006 = list("v_CA06_22"),
    vec_2001 = list("v_CA01_25"),
    vec_1996 = list("v_CA1996_30"),
    var_title = list(list(en = "Female individuals", fr = "Individus - Femme")),
    var_short = list(list(en = "Females", fr = "Femmes")),
    description = list(list(
      en = "The total count of female individuals",
      fr = "Le nombre total d'individus de sexe féminin"
    )),
    parent_vec = NA,
    parent = TRUE
  )

verify_parents(
  vectors_df = census_vectors_language,
  parents_df = census_vectors_language_parent
)

census_vectors_language <- rbind(
  census_vectors_language,
  census_vectors_language_parent
)

usethis::use_data(census_vectors_language, overwrite = TRUE)

## IMPORT POPULATION CENSUS VECTORS BY SEX ##############################################

census_vectors_age <- tibble::tibble()

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
        Reduce(rbind, c(outt, list(t[-w, ])))
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
total_age_2011 <- get_rows_from_parent(
  cancensus::list_census_vectors("CA11"),
  "v_CA11F_5"
)
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
  mapply(
    function(z, year) {
      out <- if (!is.data.frame(z)) {
        lapply(z, cleanup, year)
      } else {
        cleanup(z, year)
      }
      binded <- Reduce(rbind, out)
      listed <- if (!is.data.frame(z)) {
        lapply(unique(binded$label), function(x) {
          binded$vector[binded$label == x]
        })
      } else {
        z$vector
      }
      if (!is.data.frame(z)) {
        out <- out[[1]]
      }
      out$vector <- listed
      out
    },
    data,
    c("2011", "2006", "2001", "1996"),
    SIMPLIFY = FALSE
  )
}

older <- list(total_age_2011, total_age_2006, total_age_2001, total_age_1996)
names(older) <- c("2011", "2006", "2001", "1996")
cleaned_older <- handle_older_data(older)

# Processing recent data and structuring into the desired format
recent <- mapply(
  \(x, year) {
    categories <- lapply(cleaned_older$`2011`$label, \(z) eval(parse(text = z)))
    second_penultimate_vecs <- lapply(
      categories[2:(length(categories) - 1)],
      \(c) x$vector[x$label %in% c]
    )
    first <- list(x$vector[1:5])
    last <- list(x$vector[which(x$label == 85):nrow(x)])
    tibble::tibble(
      vector = c(first, second_penultimate_vecs, last),
      label = cleaned_older$`2011`$label,
      year = year
    )
  },
  list(total_age_2021, total_age_2016),
  c("2021", "2016"),
  SIMPLIFY = FALSE
)

names(recent) <- c("2021", "2016")

# Bind
final <- c(recent, cleaned_older)

# Create age page tibble
categories <- unique(final[[1]]$label)


# ------------------------------------------------------------------------------
# ADD: Build MALE and FEMALE versions (same structure as 'final')
# ------------------------------------------------------------------------------

# Helper: get root vector for CA21/CA16 by sex (type = Male/Female)
get_recent_age_root <- function(census_dataset, sex_type) {
  vecs <- cancensus::list_census_vectors(census_dataset)
  out <- vecs$vector[
    vecs$label == "Total - Age" & as.character(vecs$type) == sex_type
  ]
  out[1]
}

# Helper: same traversal as recent_census(), but starting from a provided root vector
recent_census_root <- function(census_dataset, root_vec) {
  vecs <- cancensus::list_census_vectors(census_dataset)

  total_age <- get_rows_from_parent(vecs, root_vec)

  all_ages <- lapply(total_age$vector, \(bcat) {
    mid_cat <- get_rows_from_parent(vecs, bcat)
    out <- lapply(mid_cat$vector, \(mcat) {
      get_rows_from_parent(vecs, mcat)
    })

    out <- lapply(out, \(t) {
      if (sum(grepl(" to ", t$label)) > 0) {
        w <- which(grepl(" to ", t$label))
        for (i in w) {
          outt <- lapply(t$vector[w], \(lcat) {
            get_rows_from_parent(vecs, lcat)
          })
        }
        Reduce(rbind, c(outt, list(t[-w, ])))
      } else {
        t
      }
    })

    Reduce(rbind, out)
  })

  Reduce(rbind, all_ages)
}

# Helper: get CA11 root vector by sex (Male/Female)
get_ca11_age_root <- function(sex_type) {
  vecs <- cancensus::list_census_vectors("CA11")
  out <- vecs$vector[
    vecs$label == "Total population by age groups" &
      as.character(vecs$type) == sex_type
  ]
  out[1]
}

# Helper: get older male/female "total" node (label = 'Male, total' / 'Female, total')
get_older_sex_root <- function(census_dataset, sex_label) {
  vecs <- cancensus::list_census_vectors(census_dataset)
  out <- vecs$vector[vecs$label == sex_label]
  out[1]
}

# --- 2021 / 2016 (modern)
root_21_m <- get_recent_age_root("CA21", "Male")
root_21_f <- get_recent_age_root("CA21", "Female")
root_16_m <- get_recent_age_root("CA16", "Male")
root_16_f <- get_recent_age_root("CA16", "Female")

total_age_2021_m <- recent_census_root("CA21", root_21_m)
total_age_2021_f <- recent_census_root("CA21", root_21_f)
total_age_2016_m <- recent_census_root("CA16", root_16_m)
total_age_2016_f <- recent_census_root("CA16", root_16_f)

# --- 2011
root_11_m <- get_ca11_age_root("Male")
root_11_f <- get_ca11_age_root("Female")

total_age_2011_m <- get_rows_from_parent(
  cancensus::list_census_vectors("CA11"),
  root_11_m
)
total_age_2011_f <- get_rows_from_parent(
  cancensus::list_census_vectors("CA11"),
  root_11_f
)

# --- 2006 / 2001 / 1996 (older)
vecs_06 <- cancensus::list_census_vectors("CA06")
vecs_01 <- cancensus::list_census_vectors("CA01")
vecs_96 <- cancensus::list_census_vectors("CA1996")

root_06_m <- get_older_sex_root("CA06", "Male, total")
root_06_f <- get_older_sex_root("CA06", "Female, total")
root_01_m <- get_older_sex_root("CA01", "Male, total")
root_01_f <- get_older_sex_root("CA01", "Female, total")
root_96_m <- get_older_sex_root("CA1996", "Male, total")
root_96_f <- get_older_sex_root("CA1996", "Female, total")

# Wrap in list() so handle_older_data() keeps list-column behavior for older years
total_age_2006_m <- list(get_rows_from_parent(vecs_06, root_06_m))
total_age_2006_f <- list(get_rows_from_parent(vecs_06, root_06_f))
total_age_2001_m <- list(get_rows_from_parent(vecs_01, root_01_m))
total_age_2001_f <- list(get_rows_from_parent(vecs_01, root_01_f))
total_age_1996_m <- list(get_rows_from_parent(vecs_96, root_96_m))
total_age_1996_f <- list(get_rows_from_parent(vecs_96, root_96_f))

older_m <- list(
  total_age_2011_m,
  total_age_2006_m,
  total_age_2001_m,
  total_age_1996_m
)
names(older_m) <- c("2011", "2006", "2001", "1996")
cleaned_older_m <- handle_older_data(older_m)

older_f <- list(
  total_age_2011_f,
  total_age_2006_f,
  total_age_2001_f,
  total_age_1996_f
)
names(older_f) <- c("2011", "2006", "2001", "1996")
cleaned_older_f <- handle_older_data(older_f)

recent_m <- mapply(
  \(x, year) {
    categories <- lapply(cleaned_older_m$`2011`$label, \(z) {
      eval(parse(text = z))
    })
    second_penultimate_vecs <- lapply(
      categories[2:(length(categories) - 1)],
      \(c) x$vector[x$label %in% c]
    )
    first <- list(x$vector[1:5])
    last <- list(x$vector[which(x$label == 85):nrow(x)])
    tibble::tibble(
      vector = c(first, second_penultimate_vecs, last),
      label = cleaned_older_m$`2011`$label,
      year = year
    )
  },
  list(total_age_2021_m, total_age_2016_m),
  c("2021", "2016"),
  SIMPLIFY = FALSE
)
names(recent_m) <- c("2021", "2016")
final_m <- c(recent_m, cleaned_older_m)

recent_f <- mapply(
  \(x, year) {
    categories <- lapply(cleaned_older_f$`2011`$label, \(z) {
      eval(parse(text = z))
    })
    second_penultimate_vecs <- lapply(
      categories[2:(length(categories) - 1)],
      \(c) x$vector[x$label %in% c]
    )
    first <- list(x$vector[1:5])
    last <- list(x$vector[which(x$label == 85):nrow(x)])
    tibble::tibble(
      vector = c(first, second_penultimate_vecs, last),
      label = cleaned_older_f$`2011`$label,
      year = year
    )
  },
  list(total_age_2021_f, total_age_2016_f),
  c("2021", "2016"),
  SIMPLIFY = FALSE
)
names(recent_f) <- c("2021", "2016")
final_f <- c(recent_f, cleaned_older_f)


# ------------------------------------------------------------------------------
# REPLACE: Build 54 variables (total + male + female), same base structure
# ------------------------------------------------------------------------------

final_by_sex <- list(
  total = final,
  m = final_m,
  f = final_f
)

sex_meta <- list(
  total = list(
    suffix = "",
    parent_vec = c("c_population")
  ),
  m = list(
    suffix = "_m",
    parent_vec = c("c_population", "c_population_m")
  ),
  f = list(
    suffix = "_f",
    parent_vec = c("c_population", "c_population_f")
  )
)

vectors_table <- unlist(
  lapply(categories, function(cat) {
    rows_for_cat <- lapply(names(sex_meta), function(sex_key) {
      suffix <- sex_meta[[sex_key]]$suffix
      parent_vec <- sex_meta[[sex_key]]$parent_vec

      var_code <- gsub(":", "_", cat)
      var_code <- sprintf("age_%s%s", var_code, suffix)

      vec_2021 <- final_by_sex[[sex_key]]$`2021`$vector[
        final_by_sex[[sex_key]]$`2021`$label == cat
      ]
      vec_2016 <- final_by_sex[[sex_key]]$`2016`$vector[
        final_by_sex[[sex_key]]$`2016`$label == cat
      ]
      vec_2011 <- final_by_sex[[sex_key]]$`2011`$vector[
        final_by_sex[[sex_key]]$`2011`$label == cat
      ]
      vec_2006 <- final_by_sex[[sex_key]]$`2006`$vector[
        final_by_sex[[sex_key]]$`2006`$label == cat
      ]
      vec_2001 <- final_by_sex[[sex_key]]$`2001`$vector[
        final_by_sex[[sex_key]]$`2001`$label == cat
      ]
      vec_1996 <- final_by_sex[[sex_key]]$`1996`$vector[
        final_by_sex[[sex_key]]$`1996`$label == cat
      ]

      if (grepl(":", cat)) {
        bounds <- strsplit(cat, ":")[[1]]
        from <- bounds[1]
        to <- bounds[2]

        if (sex_key == "total") {
          var_title_en <- sprintf("Persons aged %s to %s years", from, to)
          var_title_fr <- sprintf("Personnes âgées de %s à %s ans", from, to)
          var_short_en <- sprintf("%s–%s", from, to)
          var_short_fr <- sprintf("%s–%s ans", from, to)
          description_en <- sprintf(
            "The number of persons aged %s to %s years.",
            from,
            to
          )
          description_fr <- sprintf(
            "Le nombre de personnes âgées de %s à %s ans.",
            from,
            to
          )
        } else if (sex_key == "m") {
          var_title_en <- sprintf("Males aged %s to %s years", from, to)
          var_title_fr <- sprintf("Hommes âgés de %s à %s ans", from, to)
          var_short_en <- sprintf("%s–%s (M)", from, to)
          var_short_fr <- sprintf("%s–%s ans (H)", from, to)
          description_en <- sprintf(
            "The number of males aged %s to %s years.",
            from,
            to
          )
          description_fr <- sprintf(
            "Le nombre d'hommes âgés de %s à %s ans.",
            from,
            to
          )
        } else {
          var_title_en <- sprintf("Females aged %s to %s years", from, to)
          var_title_fr <- sprintf("Femmes âgées de %s à %s ans", from, to)
          var_short_en <- sprintf("%s–%s (F)", from, to)
          var_short_fr <- sprintf("%s–%s ans (F)", from, to)
          description_en <- sprintf(
            "The number of females aged %s to %s years.",
            from,
            to
          )
          description_fr <- sprintf(
            "Le nombre de femmes âgées de %s à %s ans.",
            from,
            to
          )
        }
      } else {
        if (sex_key == "total") {
          var_title_en <- "Persons aged 85 years and over"
          var_title_fr <- "Personnes âgées de 85 ans et plus"
          var_short_en <- "85+"
          var_short_fr <- "85+ ans"
          description_en <- "The number of persons aged 85 years and over."
          description_fr <- "Le nombre de personnes âgées de 85 ans et plus."
        } else if (sex_key == "m") {
          var_title_en <- "Males aged 85 years and over"
          var_title_fr <- "Hommes âgés de 85 ans et plus"
          var_short_en <- "85+ (M)"
          var_short_fr <- "85+ ans (H)"
          description_en <- "The number of males aged 85 years and over."
          description_fr <- "Le nombre d'hommes âgés de 85 ans et plus."
        } else {
          var_title_en <- "Females aged 85 years and over"
          var_title_fr <- "Femmes âgées de 85 ans et plus"
          var_short_en <- "85+ (F)"
          var_short_fr <- "85+ ans (F)"
          description_en <- "The number of females aged 85 years and over."
          description_fr <- "Le nombre de femmes âgées de 85 ans et plus."
        }
      }

      tibble::tibble(
        var_code = var_code,
        type = list(list(
          unit = "count",
          aggregation_field = "sum",
          measurement_scale = "scalar"
        )),
        theme = "Age",
        vec_2021 = vec_2021,
        vec_2016 = vec_2016,
        vec_2011 = vec_2011,
        vec_2006 = vec_2006,
        vec_2001 = vec_2001,
        vec_1996 = vec_1996,
        var_title = list(list(
          en = var_title_en,
          fr = var_title_fr
        )),
        var_short = list(list(
          en = var_short_en,
          fr = var_short_fr
        )),
        description = list(list(
          en = description_en,
          fr = description_fr
        )),
        parent_vec = list(parent_vec),
        parent = FALSE
      )
    })

    rows_for_cat
  }),
  recursive = FALSE
)

census_vectors_age_page <- Reduce(rbind, vectors_table)

# Parents divided with language
verify_parents(
  vectors_df = census_vectors_age_page,
  parents_df = census_vectors_language_parent
)

census_vectors_age <- rbind(census_vectors_age, census_vectors_age_page)

usethis::use_data(census_vectors_age, overwrite = TRUE)

## IMPORT EDUCATION CENSUS VECTORS #############################################

census_vectors_education <-
  tibble::tibble(
    var_code = "edu_no_degree",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5820"),
    vec_2016 = list("v_CA16_5054"),
    vec_2011 = list("v_CA11N_1774"),
    vec_2006 = list(c("v_CA06_1235", "v_CA06_1249", "v_CA06_1263")),
    vec_2001 = list(c("v_CA01_1387", "v_CA01_1391", "v_CA01_1394")),
    vec_1996 = list(c("v_CA1996_1350", "v_CA1996_1354", "v_CA1996_1357")),
    var_title = list(list(
      en = "No certificate, diploma or degree",
      fr = "Aucun certificat, diplôme ou grade"
    )),
    var_short = list(list(en = "No degree", fr = "Sans diplôme")),
    description = list(list(
      en = "The number of persons aged 15 years and over without any certificate, diploma or degree.",
      fr = "Le nombre de personnes âgées de 15 ans et plus n'ayant aucun certificat, diplôme ou grade."
    )),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_no_degree_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5821"),
    vec_2016 = list("v_CA16_5055"),
    vec_2011 = list("v_CA11N_1775"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "No certificate, diploma or degree - Male",
      fr = "Aucun certificat, diplôme ou grade - Homme"
    )),
    var_short = list(list(en = "No degree M", fr = "Sans diplôme H")),
    description = list(list(
      en = "The number of males aged 15 years and over without any certificate, diploma or degree.",
      fr = "Le nombre d'hommes âgés de 15 ans et plus n'ayant aucun certificat, diplôme ou grade."
    )),
    parent_vec = "population_15plus_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_no_degree_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5822"),
    vec_2016 = list("v_CA16_5056"),
    vec_2011 = list("v_CA11N_1776"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "No certificate, diploma or degree - Female",
      fr = "Aucun certificat, diplôme ou grade - Femme"
    )),
    var_short = list(list(en = "No degree F", fr = "Sans diplôme F")),
    description = list(list(
      en = "The number of females aged 15 years and over without any certificate, diploma or degree.",
      fr = "Le nombre de femmes âgées de 15 ans et plus n'ayant aucun certificat, diplôme ou grade."
    )),
    parent_vec = "population_15plus_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_secondary",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5823"),
    vec_2016 = list("v_CA16_5057"),
    vec_2011 = list("v_CA11N_1777"),
    vec_2006 = list(c("v_CA06_1237", "v_CA06_1251", "v_CA06_1265")),
    vec_2001 = list("v_CA01_1388"),
    vec_1996 = list("v_CA1996_1351"),
    var_title = list(list(
      en = "Secondary school diploma or equivalent",
      fr = "Diplôme d'études secondaires ou équivalent"
    )),
    var_short = list(list(en = "Secondary", fr = "Secondaire")),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a secondary (high) school diploma or equivalent.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un diplôme d'études secondaires ou un équivalent."
    )),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_secondary_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5824"),
    vec_2016 = list("v_CA16_5058"),
    vec_2011 = list("v_CA11N_1778"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Secondary school diploma or equivalent - Male",
      fr = "Diplôme d'études secondaires ou équivalent - Homme"
    )),
    var_short = list(list(en = "Secondary M", fr = "Secondaire H")),
    description = list(list(
      en = "The number of males aged 15 years and over whose highest certificate, diploma or degree is a secondary (high) school diploma or equivalent.",
      fr = "Le nombre d'hommes âgés de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un diplôme d'études secondaires ou un équivalent."
    )),
    parent_vec = "population_15plus_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_secondary_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5825"),
    vec_2016 = list("v_CA16_5059"),
    vec_2011 = list("v_CA11N_1779"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Secondary school diploma or equivalent - Female",
      fr = "Diplôme d'études secondaires ou équivalent - Femme"
    )),
    var_short = list(list(en = "Secondary F", fr = "Secondaire F")),
    description = list(list(
      en = "The number of females aged 15 years and over whose highest certificate, diploma or degree is a secondary (high) school diploma or equivalent.",
      fr = "Le nombre de femmes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un diplôme d'études secondaires ou un équivalent."
    )),
    parent_vec = "population_15plus_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_nonuni",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5841"),
    vec_2016 = list("v_CA16_5072"),
    vec_2011 = list("v_CA11N_1786"),
    vec_2006 = list(c("v_CA06_1239", "v_CA06_1253", "v_CA06_1267")),
    vec_2001 = list("v_CA01_1392"),
    vec_1996 = list("v_CA1996_1355"),
    var_title = list(list(
      en = "College, CEGEP or other non-university certificate or diploma",
      fr = "Certificat ou diplôme d'un collège, d'un CÉGEP ou autre non universitaire"
    )),
    var_short = list(list(en = "College / CEGEP", fr = "Collège / CÉGEP")),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is from a college, CEGEP or other non-university institution.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade provient d'un collège, d'un CÉGEP ou d'un autre établissement non universitaire."
    )),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_nonuni_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5842"),
    vec_2016 = list("v_CA16_5073"),
    vec_2011 = list("v_CA11N_1787"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "College, CEGEP or other non-university certificate or diploma - Male",
      fr = "Certificat ou diplôme d'un collège, d'un CÉGEP ou autre non universitaire - Homme"
    )),
    var_short = list(list(en = "College/CEGEP M", fr = "Collège/CÉGEP H")),
    description = list(list(
      en = "The number of males aged 15 years and over whose highest certificate, diploma or degree is from a college, CEGEP or other non-university institution.",
      fr = "Le nombre d'hommes âgés de 15 ans et plus dont le plus haut certificat, diplôme ou grade provient d'un collège, d'un CÉGEP ou d'un autre établissement non universitaire."
    )),
    parent_vec = "population_15plus_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_nonuni_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5843"),
    vec_2016 = list("v_CA16_5074"),
    vec_2011 = list("v_CA11N_1788"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "College, CEGEP or other non-university certificate or diploma - Female",
      fr = "Certificat ou diplôme d'un collège, d'un CÉGEP ou autre non universitaire - Femme"
    )),
    var_short = list(list(en = "College/CEGEP F", fr = "Collège/CÉGEP F")),
    description = list(list(
      en = "The number of females aged 15 years and over whose highest certificate, diploma or degree is from a college, CEGEP or other non-university institution.",
      fr = "Le nombre de femmes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade provient d'un collège, d'un CÉGEP ou d'un autre établissement non universitaire."
    )),
    parent_vec = "population_15plus_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_uni_below",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5844"),
    vec_2016 = list("v_CA16_5075"),
    vec_2011 = list("v_CA11N_1789"),
    vec_2006 = list(c("v_CA06_1241", "v_CA06_1255", "v_CA06_1269")),
    vec_2001 = list("v_CA01_1396"),
    vec_1996 = list("v_CA1996_1359"),
    var_title = list(list(
      en = "University certificate or diploma below bachelor level",
      fr = "Certificat ou diplôme universitaire inférieur au baccalauréat"
    )),
    var_short = list(list(en = "Sub-bch. uni.", fr = "Uni. sous bac.")),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a university certificate or diploma below the bachelor level.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un certificat ou diplôme universitaire inférieur au niveau du baccalauréat."
    )),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_uni_below_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5845"),
    vec_2016 = list("v_CA16_5076"),
    vec_2011 = list("v_CA11N_1790"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "University certificate or diploma below bachelor level - Male",
      fr = "Certificat ou diplôme universitaire inférieur au baccalauréat - Homme"
    )),
    var_short = list(list(en = "Sub-bch uni M", fr = "Uni sous bac H")),
    description = list(list(
      en = "The number of males aged 15 years and over whose highest certificate, diploma or degree is a university certificate or diploma below the bachelor level.",
      fr = "Le nombre d'hommes âgés de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un certificat ou diplôme universitaire inférieur au niveau du baccalauréat."
    )),
    parent_vec = "population_15plus_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_uni_below_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5846"),
    vec_2016 = list("v_CA16_5077"),
    vec_2011 = list("v_CA11N_1791"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "University certificate or diploma below bachelor level - Female",
      fr = "Certificat ou diplôme universitaire inférieur au baccalauréat - Femme"
    )),
    var_short = list(list(en = "Sub-bch uni F", fr = "Uni sous bac F")),
    description = list(list(
      en = "The number of females aged 15 years and over whose highest certificate, diploma or degree is a university certificate or diploma below the bachelor level.",
      fr = "Le nombre de femmes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un certificat ou diplôme universitaire inférieur au niveau du baccalauréat."
    )),
    parent_vec = "population_15plus_f",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_bachelor_above",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5847"),
    vec_2016 = list("v_CA16_5078"),
    vec_2011 = list("v_CA11N_1792"),
    vec_2006 = list(c(
      "v_CA06_1243",
      "v_CA06_1244",
      "v_CA06_1257",
      "v_CA06_1258",
      "v_CA06_1271",
      "v_CA06_1272"
    )),
    vec_2001 = list("v_CA01_1397"),
    vec_1996 = list("v_CA1996_1360"),
    var_title = list(list(
      en = "Bachelor and above",
      fr = "Baccalauréat et plus"
    )),
    var_short = list(list(en = "Bachelor+", fr = "Baccalauréat+")),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a bachelor's degree or a higher university credential.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un baccalauréat ou un grade universitaire supérieur."
    )),
    parent_vec = "population_15plus",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_bachelor_above_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5848"),
    vec_2016 = list("v_CA16_5079"),
    vec_2011 = list("v_CA11N_1793"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Bachelor and above - Male",
      fr = "Baccalauréat et plus - Homme"
    )),
    var_short = list(list(en = "Bachelor+ M", fr = "Bacc+ H")),
    description = list(list(
      en = "The number of males aged 15 years and over whose highest certificate, diploma or degree is a bachelor's degree or a higher university credential.",
      fr = "Le nombre d'hommes âgés de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un baccalauréat ou un grade universitaire supérieur."
    )),
    parent_vec = "population_15plus_m",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "edu_bachelor_above_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Education",
    vec_2021 = list("v_CA21_5849"),
    vec_2016 = list("v_CA16_5080"),
    vec_2011 = list("v_CA11N_1794"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Bachelor and above - Female",
      fr = "Baccalauréat et plus - Femme"
    )),
    var_short = list(list(en = "Bachelor+ F", fr = "Baccalauréat+ F")),
    description = list(list(
      en = "The number of females aged 15 years and over whose highest certificate, diploma or degree is a bachelor's degree or a higher university credential.",
      fr = "Le nombre de femmes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un baccalauréat ou un grade universitaire supérieur."
    )),
    parent_vec = "population_15plus_f",
    parent = FALSE
  )


census_vectors_education_parent <-
  tibble::tibble(
    var_code = "population_15plus",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_5817"),
    vec_2016 = list("v_CA16_5051"),
    vec_2011 = list("v_CA11N_1771"),
    vec_2006 = list(c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262")),
    vec_2001 = list("v_CA01_1384"),
    vec_1996 = list("v_CA1996_1347"),
    var_title = list(list(
      en = "Population aged 15 years and over",
      fr = "Population âgée de 15 ans et plus"
    )),
    var_short = list(list(en = "Individuals", fr = "Individus")),
    description = list(list(
      en = "The total number of persons aged 15 years and over in private households.",
      fr = "Le nombre total de personnes âgées de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_15plus_m",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_5818"),
    vec_2016 = list("v_CA16_5052"),
    vec_2011 = list("v_CA11N_1772"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Population aged 15 years and over - Male",
      fr = "Population âgée de 15 ans et plus - Homme"
    )),
    var_short = list(list(en = "Individuals M", fr = "Individus H")),
    description = list(list(
      en = "The total number of males aged 15 years and over in private households.",
      fr = "Le nombre total d'hommes âgés de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "population_15plus_f",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Identity",
    vec_2021 = list("v_CA21_5819"),
    vec_2016 = list("v_CA16_5053"),
    vec_2011 = list("v_CA11N_1773"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Population aged 15 years and over - Female",
      fr = "Population âgée de 15 ans et plus - Femme"
    )),
    var_short = list(list(en = "Individuals F", fr = "Individus F")),
    description = list(list(
      en = "The total number of females aged 15 years and over in private households.",
      fr = "Le nombre total de femmes âgées de 15 ans et plus vivant dans des ménages privés."
    )),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(
  vectors_df = census_vectors_education,
  parents_df = census_vectors_education_parent
)

census_vectors_education <- rbind(
  census_vectors_education,
  census_vectors_education_parent
)

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

########################
# Final check
########################

#### Check vectors duplication

check_census_vectors_vec <- function(x) {
  vec_cols <- grep("^vec_\\d{4}$", names(x), value = TRUE)
  n <- nrow(x)
  var_code <- if ("var_code" %in% names(x)) {
    as.character(x$var_code)
  } else {
    rep(NA_character_, n)
  }
  parent_flag <- if ("parent" %in% names(x)) {
    as.logical(x$parent)
  } else {
    rep(NA, n)
  }

  normalize_vec_cell <- function(z) {
    if (is.null(z)) {
      return(character(0))
    }
    if (length(z) == 1L && is.atomic(z) && is.na(z)) {
      return(character(0))
    }
    if (is.list(z)) {
      out <- unlist(z, recursive = TRUE, use.names = FALSE)
    } else {
      out <- z
    }
    out <- as.character(out)
    out <- out[!is.na(out)]
    out <- trimws(out)
    out <- out[out != ""]
    unique(out)
  }

  # Pattern 1 : Total = M + F (même préfixe, l'un sans suffixe, l'autre avec _m/_f)
  is_valid_mf_pair <- function(vc_i, vc_j) {
    base_i <- sub("_(m|f)$", "", vc_i)
    base_j <- sub("_(m|f)$", "", vc_j)
    if (base_i != base_j) {
      return(FALSE)
    }
    has_suffix_i <- grepl("_(m|f)$", vc_i)
    has_suffix_j <- grepl("_(m|f)$", vc_j)
    xor(has_suffix_i, has_suffix_j)
  }

  # Pattern 2 : même concept, un parent = TRUE et l'autre parent = FALSE
  is_valid_parent_child_pair <- function(i, j) {
    pi <- parent_flag[i]
    pj <- parent_flag[j]
    if (is.na(pi) || is.na(pj)) {
      return(FALSE)
    }
    xor(pi, pj)
  }

  is_valid_pair <- function(i, j) {
    is_valid_mf_pair(var_code[i], var_code[j]) ||
      is_valid_parent_child_pair(i, j)
  }

  # --- Duplicate var_code ---
  dup_var_code <- rep(FALSE, n)
  dup_var_code_peers <- vector("list", n)

  non_na_code <- !is.na(var_code) & var_code != ""
  if (any(non_na_code)) {
    tab <- table(var_code[non_na_code])
    dup_vals <- names(tab)[tab > 1L]
    if (length(dup_vals)) {
      for (val in dup_vals) {
        rows <- which(var_code == val & non_na_code)
        dup_var_code[rows] <- TRUE
        for (i in rows) {
          dup_var_code_peers[[i]] <- setdiff(rows, i)
        }
      }
    }
  }

  # --- Duplicate vectors ---
  dup_vec_any <- rep(FALSE, n)
  dup_vec_details <- vector("list", n)

  if (length(vec_cols) > 0) {
    for (colname in vec_cols) {
      all_codes <- vector("list", n)
      for (i in seq_len(n)) {
        all_codes[[i]] <- normalize_vec_cell(x[[colname]][[i]])
      }

      occ <- list()
      for (i in seq_len(n)) {
        codes <- all_codes[[i]]
        if (length(codes) == 0) {
          next
        }
        for (cc in codes) {
          occ[[cc]] <- c(occ[[cc]], i)
        }
      }

      dup_codes <- names(occ)[vapply(
        occ,
        function(rr) length(unique(rr)) > 1L,
        logical(1)
      )]
      if (length(dup_codes) == 0) {
        next
      }

      for (vc in dup_codes) {
        rows <- unique(occ[[vc]])
        pairs <- combn(rows, 2, simplify = FALSE)

        all_valid <- all(vapply(
          pairs,
          function(p) {
            is_valid_pair(p[1], p[2])
          },
          logical(1)
        ))

        if (all_valid) {
          next
        }

        for (i in rows) {
          invalid_peers <- setdiff(rows, i)[
            !vapply(
              setdiff(rows, i),
              function(j) is_valid_pair(i, j),
              logical(1)
            )
          ]
          if (length(invalid_peers) == 0) {
            next
          }
          dup_vec_any[i] <- TRUE
          dup_vec_details[[i]] <- c(
            dup_vec_details[[i]],
            paste0(
              colname,
              " duplicated: ",
              vc,
              " (also rows ",
              paste(invalid_peers, collapse = ", "),
              ")"
            )
          )
        }
      }
    }
  }

  any_issue <- dup_var_code | dup_vec_any
  idx <- which(any_issue)

  if (length(idx) == 0L) {
    message(
      "All vector checks passed (no duplicate var_code, no invalid duplicate vec_YYYY values)."
    )
    return(invisible(NULL))
  }

  for (i in idx) {
    vc <- var_code[i]
    if (is.na(vc) || vc == "") {
      vc <- "<NA>"
    }

    issues <- character(0)

    if (dup_var_code[i]) {
      peers <- dup_var_code_peers[[i]]
      if (!is.null(peers) && length(peers)) {
        issues <- c(
          issues,
          paste0(
            "duplicate var_code also at rows ",
            paste(peers, collapse = ", ")
          )
        )
      } else {
        issues <- c(issues, "duplicate var_code")
      }
    }

    if (dup_vec_any[i]) {
      issues <- c(issues, dup_vec_details[[i]])
    }

    message(
      "var_code '",
      vc,
      "' (row ",
      i,
      "): ",
      paste(issues, collapse = "; ")
    )
  }

  invisible(
    tibble::tibble(
      row_id = idx,
      var_code = var_code[idx],
      dup_var_code = dup_var_code[idx],
      dup_vec_any = dup_vec_any[idx]
    )
  )
}

check_census_vectors_vec(census_vectors_table)

#### Check vectors documentation

check_census_vectors_doc <- function(x) {
  max_short <- 15L

  needed_basic <- c("var_title", "var_short")
  missing_basic <- setdiff(needed_basic, names(x))
  if (length(missing_basic) > 0) {
    message(
      "Missing required columns for check: ",
      paste(missing_basic, collapse = ", "),
      ". Nothing checked."
    )
    return(invisible(NULL))
  }

  desc_col <- NULL
  if ("description" %in% names(x)) {
    desc_col <- "description"
  } else if ("explanation" %in% names(x)) {
    desc_col <- "explanation"
  } else {
    message(
      "No 'description' or 'explanation' column found, description checks skipped."
    )
  }

  is_blank <- function(z) is.na(z) | trimws(z) == ""

  extract_lang <- function(col, lang) {
    vapply(
      col,
      FUN.VALUE = character(1),
      FUN = function(z) {
        if (is.null(z)) {
          return(NA_character_)
        }
        if (length(z) == 1L && is.atomic(z) && is.na(z)) {
          return(NA_character_)
        }
        if (is.list(z) && !is.null(z[[lang]])) {
          val <- z[[lang]]
          if (is.null(val) || length(val) == 0L || all(is.na(val))) {
            return(NA_character_)
          }
          return(as.character(val[1]))
        }
        if (is.atomic(z) && length(z) == 1L && is.character(z)) {
          if (lang == "en") return(as.character(z)) else return(NA_character_)
        }
        NA_character_
      }
    )
  }

  n <- nrow(x)
  var_code <- if ("var_code" %in% names(x)) {
    as.character(x$var_code)
  } else {
    rep(NA_character_, n)
  }

  title_en <- extract_lang(x$var_title, "en")
  title_fr <- extract_lang(x$var_title, "fr")
  short_en <- extract_lang(x$var_short, "en")
  short_fr <- extract_lang(x$var_short, "fr")

  if (!is.null(desc_col)) {
    desc_en <- extract_lang(x[[desc_col]], "en")
    desc_fr <- extract_lang(x[[desc_col]], "fr")
  } else {
    desc_en <- rep(NA_character_, n)
    desc_fr <- rep(NA_character_, n)
  }

  missing_title_en <- is_blank(title_en)
  missing_title_fr <- is_blank(title_fr)
  missing_short_en <- is_blank(short_en)
  missing_short_fr <- is_blank(short_fr)
  missing_desc_en <- is_blank(desc_en)
  missing_desc_fr <- is_blank(desc_fr)
  short_en_too_long <- !is.na(short_en) & nchar(short_en) > max_short
  short_fr_too_long <- !is.na(short_fr) & nchar(short_fr) > max_short

  any_issue <- missing_title_en |
    missing_title_fr |
    missing_short_en |
    missing_short_fr |
    (!is.null(desc_col) & (missing_desc_en | missing_desc_fr)) |
    short_en_too_long |
    short_fr_too_long

  idx <- which(any_issue)

  if (length(idx) == 0L) {
    message(
      "All documentation checks passed ",
      "(titles, short names and descriptions in EN/FR, short names <= ",
      max_short,
      " characters)."
    )
    return(invisible(NULL))
  }

  for (i in idx) {
    vc <- var_code[i]
    if (is.na(vc) || vc == "") {
      vc <- "<NA>"
    }

    issues <- character(0)

    if (missing_title_en[i]) {
      issues <- c(issues, "missing English title")
    }
    if (missing_title_fr[i]) {
      issues <- c(issues, "missing French title")
    }

    if (missing_short_en[i]) {
      issues <- c(issues, "missing English short name")
    } else if (short_en_too_long[i]) {
      issues <- c(
        issues,
        paste0(
          "English short name > ",
          max_short,
          " chars (len = ",
          nchar(short_en[i]),
          ")"
        )
      )
    }

    if (missing_short_fr[i]) {
      issues <- c(issues, "missing French short name")
    } else if (short_fr_too_long[i]) {
      issues <- c(
        issues,
        paste0(
          "French short name > ",
          max_short,
          " chars (len = ",
          nchar(short_fr[i]),
          ")"
        )
      )
    }

    if (!is.null(desc_col)) {
      if (missing_desc_en[i]) {
        issues <- c(issues, "missing English description")
      }
      if (missing_desc_fr[i]) issues <- c(issues, "missing French description")
    }

    message(
      "var_code '",
      vc,
      "' (row ",
      i,
      "): ",
      paste(issues, collapse = "; ")
    )
  }

  invisible(
    tibble::tibble(
      row_id = idx,
      var_code = var_code[idx],
      missing_title_en = missing_title_en[idx],
      missing_title_fr = missing_title_fr[idx],
      missing_short_en = missing_short_en[idx],
      missing_short_fr = missing_short_fr[idx],
      missing_desc_en = missing_desc_en[idx],
      missing_desc_fr = missing_desc_fr[idx],
      short_en_too_long = short_en_too_long[idx],
      short_fr_too_long = short_fr_too_long[idx]
    )
  )
}

check_census_vectors_doc(census_vectors_table)

usethis::use_data(census_vectors_table, overwrite = TRUE)
