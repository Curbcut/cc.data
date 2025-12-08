verify_parents <- function(vectors_df, parents_df) {
  
  parents_type <- unique(vapply(parents_df$type, function(x) x$unit, character(1)))
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
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4239"),
    vec_2016 = list("v_CA16_4838"),
    vec_2011 = list("v_CA11N_2254"),
    vec_2006 = list("v_CA06_103"),
    vec_2001 = list("v_CA01_100"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = list(list(
      en = "Tenant-occupied",
      fr = "Occupés par des locataires")),
    var_short = list(list(
      en = "Tenant",
      fr = "Locataire")),
    description = list(list(
      en = "The number of private households where no member of the household owns their dwelling. The dwelling is considered to be rented even if no cash rent is paid",
      fr = "Le nombre des ménages privés dont aucun membre n'est propriétaire du logement. Le logement est considéré comme étant loué même si aucun loyer en argent n'est versé")),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_shelcost_tenant",
    type = list(list(
      unit = "dollar",                      
      aggregation_field = "avg",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4318"),
    vec_2016 = list("v_CA16_4901"),
    vec_2011 = list("v_CA11N_2292"),
    vec_2006 = list("v_CA06_2050"),
    vec_2001 = list("v_CA01_1667"),
    vec_1996 = list("v_CA1996_1701"),
    var_title = list(list(
      en = "Average shelter cost for tenant households",
      fr = "Frais de logement moyens des ménages locataires")),
    var_short = list(list(
      en = "Shelter tenant",
      fr = "Frais log. loc")),
    description = list(list(
      en = "The average monthly shelter expenses paid by tenant households, including, where applicable, rent plus costs of electricity, heat, water and municipal services",
      fr = "Les frais mensuels moyens de logement payés par les ménages locataires, incluant, s’il y a lieu, le loyer ainsi que les coûts d’électricité, de chauffage, d’eau et des services municipaux"
    )),
    parent_vec = "tenant_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_shelcost_owner",
    type = list(list(
      unit = "dollar",                      
      aggregation_field = "avg",
      measurement_scale = "scalar")),
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
      fr = "Les frais mensuels moyens de logement payés par les ménages propriétaires, incluant, s’il y a lieu, les versements hypothécaires, les taxes foncières, les frais de copropriété ainsi que les coûts d’électricité, de chauffage et d’eau"
    )),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_repairs",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4274"),
    vec_2016 = list("v_CA16_4872"),
    vec_2011 = list("v_CA11N_2232"),
    vec_2006 = list("v_CA06_108"),
    vec_2001 = list("v_CA01_104"),
    vec_1996 = list("v_CA1996_1687"),
    var_title = list(list(
      en ="Housing requiring major repairs",
      fr = "Logements nécessitant des réparations importantes")),
    var_short = list(list(
      en = "Repairs",
      fr = "Réparations")),    
    description = list(list(
      en = "The number of occupied private dwellings that require major repairs, including dwellings with defective plumbing or electrical wiring, or structural repairs to walls, floors or ceilings",
      fr = "Le nombre de logements privés occupés nécessitant des réparations majeures, y compris les logements où la plomberie ou l'installation électrique est défectueuse, ou qui ont besoin de réparations à la charpente des murs, des planchers ou des plafonds")),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_value",
    type = list(list(
      unit = "dollar",                      
      aggregation_field = "avg",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4312"),
    vec_2016 = list("v_CA16_4896"),
    vec_2011 = list("v_CA11N_2287"),
    vec_2006 = list("v_CA06_2054"),
    vec_2001 = list("v_CA01_1674"),
    vec_1996 = list("v_CA1996_1681"),
    var_title = list(list(
      en = "Average property value",
      fr = "Valeur moyenne des propriétés")),
    var_short = list(list(
      en = "Avg. value",
      fr = "Valeur moyenne")),
    description = list(list(
      en = "The average owner-estimated market value of owner-occupied private dwellings, including the value of the entire dwelling, the land it is on, and any other structures on the property (such as a garage)",
      fr = "Valeur marchande moyenne estimée par le propriétaire pour les logements privés occupés par leur propriétaire, incluant la valeur de l’ensemble du logement, du terrain sur lequel il se trouve et de toute autre construction sur la propriété"
    )),
    parent_vec = "owner_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_unafford",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4290"),
    vec_2016 = list("v_CA16_4888"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Unaffordable housing",
      fr = "Logement inabordable")),
    var_short = list(list(
      en = "Unaffordable",
      fr = "Inabordable")),
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
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4262"),
    vec_2016 = list("v_CA16_4861"),
    vec_2011 = list("v_CA11N_2276"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Unsuitable housing",
      fr = "Logement inadapté")),
    var_short = list(list(
      en = "Unsuitable",
      fr = "Inadéquat")),
    description = list(list(
      en = "The number of private households living in unsuitable housing, that is, in dwellings that do not have enough bedrooms for the size and composition of the household according to the National Occupancy Standard (NOS)",
      fr = "Nombre de ménages privés vivant dans un logement de taille non convenable, c’est-à-dire dans des logements qui ne comptent pas suffisamment de chambres pour la taille et la composition du ménage, selon la Norme nationale d’occupation (NNO)"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_stress_renter",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4315"),
    vec_2016 = list("v_CA16_4899"),
    vec_2011 = list("v_CA11N_2290"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Renter housing stress",
      fr = "Difficultés financières lié au logement des locataires")),
    var_short = list(list(
      en = "Renter stress",
      fr = "Stress loc.")),
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
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4307"),
    vec_2016 = list("v_CA16_4892"),
    vec_2011 = list("v_CA11N_2283"),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Owner housing stress",
      fr = "Difficultés financières lié au logement des propriétaires")),
    var_short = list(list(
      en = "Owner stress",
      fr = "Stress prop.")),
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
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_5751"),
    vec_2016 = list("v_CA16_6698"),
    vec_2011 = list("v_CA11N_1723"),
    vec_2006 = list("v_CA06_453"),
    vec_2001 = list("v_CA01_383"),
    vec_1996 = list("v_CA1996_1387"),
    var_title = list(list(
      en = "One-year housing mobility",
      fr = "Mobilité du logement sur un an")),
    var_short = list(list(
      en = "1-year mob.",
      fr = "Mob. 1 an")),
    description = list(list(
      en = "The number of persons who moved in the past year, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours de l’année précédente, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_1",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "housing_mobility_five",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_5778"),
    vec_2016 = list("v_CA16_6725"),
    vec_2011 = list("v_CA11N_1750"),
    vec_2006 = list("v_CA06_462"),
    vec_2001 = list("v_CA01_392"),
    vec_1996 = list("v_CA1996_1396"),
    var_title = list(list(
      en = "Five-year housing mobility",
      fr = "Mobilité du logement sur cinq ans")),
    var_short = list(list(
      en = "5-year mob.",
      fr = "Mob. 5 ans")),
    description = list(list(
      en = "The number of persons who moved in the past 5 years, measured as those whose place of residence on the reference day was different from their place of residence on the same date one year earlier",
      fr = "Le nombre de personnes ayant déménagé au cours des 5 dernières années, mesuré comme celles dont le lieu de résidence au jour de référence différait de leur lieu de résidence à la même date un an plus tôt"
    )),
    parent_vec = "mobility_status_5",
    parent = FALSE
  )


census_vectors_housing_parent <-
  tibble::tibble(
    var_code = "private_households",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4237"),
    vec_2016 = list("v_CA16_4836"),
    vec_2011 = list("v_CA11N_2252"),
    vec_2006 = list("v_CA06_136"),
    vec_2001 = list("v_CA01_129"),
    vec_1996 = list("v_CA1996_1694"),
    var_title = list(list(
      en = "Households",
      fr = "Ménages")),
    var_short = list(list(
      en = "Households",
      fr = "Ménages")),
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
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4272"),
    vec_2016 = list("v_CA16_4870"),
    vec_2011 = list("v_CA11N_2230"),
    vec_2006 = list("v_CA06_105"),
    vec_2001 = list("v_CA01_96"),
    vec_1996 = list("v_CA1996_1678"),
    var_title = list(list(
      en = "Dwellings",
      fr = "Logements")),
    var_short = list(list(
      en = "Dwellings",
      fr = "Logements")),
    description = list(list(
      en = "The number of private dwellings, defined as a separate set of living quarters with a private entrance from outside the building or from a common area inside, that meet the conditions for year-round occupancy (a source of heat or power and an enclosed space providing shelter from the elements)",
      fr = "Le nombre de logements privés, définis comme un ensemble séparé de pièces d'habitation possédant une entrée privée depuis l’extérieur de l’immeuble ou à partir d’un espace commun intérieur, qui répondent aux conditions les rendant propres à l’habitation durant toute l’année (source de chauffage ou d’énergie et espace clos permettant de s’abriter des intempéries)"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_tenant_households",
    type = list(list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4288"),
    vec_2016 = list("v_CA16_4886"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Owner and tenant households",
      fr = "Ménages propriétaires et locataires")),
    var_short = list(list(
      en = "Households",
      fr = "Ménages")),
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
      measurement_scale = "scalar")),
    theme = "Housing",
    vec_2021 = list("v_CA21_4313"),
    vec_2016 = list("v_CA16_4897"),
    vec_2011 = list("v_CA11N_2288"),
    vec_2006 = list("v_CA06_2049"),
    vec_2001 = list("v_CA01_1666"),
    vec_1996 = list("v_CA1996_1683"),
    var_title = list(list(
      en = "Tenant households",
      fr = "Ménages locataires")),
    var_short = list(list(
      en = "Tenant",
      fr = "Locataire")),
    description = list(list(
      en = "The number of private households where no member of the household owns their dwelling. The dwelling is considered to be rented even if no cash rent is paid",
      fr = "Le nombre des ménages privés dont aucun membre n'est propriétaire du logement. Le logement est considéré comme étant loué même si aucun loyer en argent n'est versé"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "owner_households",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Housing",
    vec_2021 = list("v_CA21_4305"),
    vec_2016 = list("v_CA16_4890"),
    vec_2011 = list("v_CA11N_2281"),
    vec_2006 = list("v_CA06_2053"),
    vec_2001 = list("v_CA01_1670"),
    vec_1996 = list("v_CA1996_1682"),
    var_title = list(list(
      en = "Owner households",
      fr = "Ménages propriétaires")),
    var_short = list(list(
      en = "Owner",
      fr = "Propriétaire")),
    description = list(list(
      en = "The number of owner households, defined as private households where at least one member of the household owns the dwelling, even if it is still being paid for",
      fr = "Le nombre de ménages propriétaires, définis comme des ménages privés au sein desquels au moins un des membres du ménage est propriétaire du logement, même s’il est encore en train de le payer"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_1",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Housing",
    vec_2021 = list("v_CA21_5745"),
    vec_2016 = list("v_CA16_6692"),
    vec_2011 = list("v_CA11N_1717"),
    vec_2006 = list("v_CA06_451"),
    vec_2001 = list("v_CA01_381"),
    vec_1996 = list("v_CA1996_1385"),
    var_title = list(list(
      en = "Residents",
      fr = "Résidents")),
    var_short = list(list(
      en = "Residents",
      fr = "Résidents")),
    description = list(list(
      en = "The total number of residents one year prior",
      fr = "Le nombre total de résidents un an auparavant"
    )),
    parent_vec = NA,
    parent = TRUE
  ) |>
  tibble::add_row(
    var_code = "mobility_status_5",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),    
    theme = "Housing",
    vec_2021 = list("v_CA21_5772"),
    vec_2016 = list("v_CA16_6719"),
    vec_2011 = list("v_CA11N_1744"),
    vec_2006 = list("v_CA06_460"),
    vec_2001 = list("v_CA01_390"),
    vec_1996 = list("v_CA1996_1394"),
    var_title = list(list(
      en = "Residents",
      fr = "Résidents")),
    var_short = list(list(
      en = "Residents",
      fr = "Résidents")),
    description = list(list(
      en = "The total number of residents five years prior",
      fr = "Le nombre total de résidents cinq ans auparavant"
    )),
    parent_vec = NA,
    parent = TRUE)

verify_parents(vectors_df = census_vectors_housing,
               parents_df = census_vectors_housing_parent)

census_vectors_housing <- rbind(census_vectors_housing,
                                census_vectors_housing_parent)

usethis::use_data(census_vectors_housing, overwrite = TRUE)


## IMPORT HOUSING TYPOLOGY #####################################################

census_vectors_typology <-
  tibble::tibble(
    var_code = "typology_single_detached",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_435"),
    vec_2016 = list("v_CA16_409"),
    vec_2011 = list("v_CA11F_200"),
    vec_2006 = list("v_CA06_120"),
    vec_2001 = list("v_CA01_113"),
    vec_1996 = list("v_CA1996_108"),
    var_title = list(list(
      en = "Single-detached",
      fr = "Maisons individuelles")),
    var_short = list(list(
      en = "Single-detached",
      fr = "Maison ind.")),
      description = list(list(
        en = "The number of private dwellings that are single-detached houses, defined as single dwellings not attached to any other dwelling or structure (except their own garage or shed), surrounded by open space on all sides and with no dwellings above or below. Mobile homes permanently fixed to a foundation are also included in this category.",
        fr = "Le nombre de logements privés qui sont des maisons individuelles non attenantes, définies comme des logements individuels non joints à un autre logement ou à une autre construction (sauf à leur propre garage ou hangar), entourés d’espaces libres sur tous les côtés et sans logement au-dessus ni au-dessous. Les habitations mobiles installées de façon permanente sur des fondations sont également incluses dans cette catégorie."
      )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_semi_detached",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_436"),
    vec_2016 = list("v_CA16_412"),
    vec_2011 = list("v_CA11F_204"),
    vec_2006 = list("v_CA06_121"),
    vec_2001 = list("v_CA01_114"),
    vec_1996 = list("v_CA1996_109"),
    var_title = list(list(
      en = "Semi-detached",
      fr = "Semi-détaché")),
    var_short = list(list(
      en = "Semi-detached",
      fr = "Semi-détaché")),
    description = list(list(
      en = "The number of private dwellings that are semi-detached houses, defined as one of two dwellings attached side by side (or back to back) to each other, but not attached to any other dwelling or structure (except their own garage or shed), with no dwellings above or below and open space around the pair.",
      fr = "Le nombre de logements privés qui sont des maisons jumelées, définies comme l’un de deux logements réunis côte à côte (ou de l’arrière à l’arrière) par un mur commun, mais non joints à aucun autre logement ou construction (sauf à leur propre garage ou hangar), sans logement au-dessus ou au-dessous et entourés d’espaces libres."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_row_house",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_437"),
    vec_2016 = list("v_CA16_413"),
    vec_2011 = list("v_CA11F_205"),
    vec_2006 = list("v_CA06_122"),
    vec_2001 = list("v_CA01_115"),
    vec_1996 = list("v_CA1996_110"),
    var_title = list(list(
      en = "Row houses",
      fr = "Maisons en rangée")),
    var_short = list(list(
      en = "Row houses",
      fr = "Rangées")),
    description = list(list(
      en = "The number of private dwellings that are row houses, defined as dwellings in a row of at least three dwellings joined side by side (or occasionally side to back), such as townhouses or garden homes, with no dwellings above or below; townhouses attached to a high-rise building are also included in this category.",
      fr = "Le nombre de logements privés qui sont des maisons en rangée, définies comme des logements dans une rangée d’au moins trois logements réunis côte à côte (ou parfois réunis par un des côtés d’un logement et l’arrière d’un autre logement), comme une maison en bande ou une maison-jardin, sans logement au-dessus ni au-dessous; les maisons en bande jointes à une tour d’habitation sont également incluses dans cette catégorie."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_duplex",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_438"),
    vec_2016 = list("v_CA16_414"),
    vec_2011 = list("v_CA11F_206"),
    vec_2006 = list("v_CA06_123"),
    vec_2001 = list("v_CA01_116"),
    vec_1996 = list("v_CA1996_111"),
    var_title = list(list(
      en = "Duplex",
      fr = "Duplex")),
    var_short = list(list(
      en = "Duplex",
      fr = "Duplex")),
    description = list(list(
      en = "The number of private dwellings that are apartments or flats in a duplex, defined as one of two dwellings located one above the other, which may or may not be attached to other dwellings or buildings.",
      fr = "Le nombre de logements privés qui sont des appartements ou plain-pieds dans un duplex, définis comme l’un de deux logements superposés qui peuvent être ou ne pas être joints à d’autres logements ou immeubles."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_small",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_439"),
    vec_2016 = list("v_CA16_415"),
    vec_2011 = list("v_CA11F_207"),
    vec_2006 = list("v_CA06_125"),
    vec_2001 = list("v_CA01_118"),
    vec_1996 = list("v_CA1996_113"),
    var_title = list(list(
      en = "Apartments, less than 5 storeys",
      fr = "Appartements de moins de 5 étages")),
    var_short = list(list(
      en = "Apt. <5",
      fr = "Apt. <5")),
    description = list(list(
      en = "The number of private dwellings that are apartments in a building that has fewer than five storeys, defined as dwelling units attached to other dwelling units, commercial units or other non-residential space in a building with fewer than five storeys.",
      fr = "Le nombre de logements privés qui sont des appartements dans un immeuble de moins de cinq étages, définis comme des logements joints à d’autres logements ou à d’autres locaux commerciaux ou non résidentiels dans un immeuble de moins de cinq étages."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_apart_large",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_440"),
    vec_2016 = list("v_CA16_410"),
    vec_2011 = list("v_CA11F_201"),
    vec_2006 = list("v_CA06_124"),
    vec_2001 = list("v_CA01_117"),
    vec_1996 = list("v_CA1996_112"),
    var_title = list(list(
      en = "Apartments, 5 or more storeys",
      fr = "Appartements de 5 étages ou plus")),
    var_short = list(list(
      en = "Apt. >=5",
      fr = "Apt. >=5")),
    description = list(list(
      en = "The number of private dwellings that are apartments in a building that has five or more storeys, that is, dwelling units in high-rise apartment buildings with five or more storeys.",
      fr = "Le nombre de logements privés qui sont des appartements dans un immeuble de cinq étages ou plus, c’est-à-dire des logements situés dans une tour d’habitation comportant cinq étages ou plus."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_other_single",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_441"),
    vec_2016 = list("v_CA16_416"),
    vec_2011 = list("v_CA11F_208"),
    vec_2006 = list("v_CA06_126"),
    vec_2001 = list("v_CA01_119"),
    vec_1996 = list("v_CA1996_114"),
    var_title = list(list(
      en = "Other single-attached",
      fr = "Autres maisons individuelles attenantes")),
    var_short = list(list(
      en = "Other single",
      fr = "Autres")),
    description = list(list(
      en = "The number of private dwellings that are other single-attached houses, defined as single dwellings attached to another building that do not fall into the other dwelling type categories, such as a single dwelling attached to a non-residential structure (e.g., a store or a church) or occasionally to another residential structure (e.g., an apartment building).",
      fr = "Le nombre de logements privés qui sont d’autres maisons individuelles attenantes, définis comme des logements individuels joints à un autre immeuble et ne se classant dans aucune des autres catégories de type de logement, comme un logement individuel réuni à une construction non résidentielle (p. ex., un magasin ou une église) ou, parfois, à une autre construction résidentielle (p. ex., un immeuble d’appartements)."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "typology_movable",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Building typology",
    vec_2021 = list("v_CA21_442"),
    vec_2016 = list("v_CA16_417"),
    vec_2011 = list("v_CA11F_202"),
    vec_2006 = list("v_CA06_127"),
    vec_2001 = list("v_CA01_120"),
    vec_1996 = list("v_CA1996_115"),
    var_title = list(list(
      en = "Movable dwellings",
      fr = "Logements mobiles")),
    var_short = list(list(
      en = "Movable",
      fr = "Mobiles")),
    description = list(list(
      en = "The number of private dwellings that are mobile homes or other movable dwellings, defined as single dwellings designed and constructed to be transported on their own chassis and capable of being moved to a new location on short notice, or other single dwellings used as places of residence that can be moved on short notice, such as tents, recreational vehicles, travel trailers, houseboats or floating homes.",
      fr = "Le nombre de logements privés qui sont des habitations mobiles ou d’autres logements mobiles, définis comme des logements individuels conçus et construits pour être transportés sur leur propre châssis et pouvant être déplacés sans grand délai, ou d’autres logements individuels utilisés comme résidence et pouvant être déplacés sans grand délai, tels qu’une tente, un véhicule récréatif, une roulotte de voyage, un bateau-maison ou une maison flottante."
    )),
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
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4245"),
    vec_2016 = list("v_CA16_4844"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "0 bedrooms",
      fr = "0 chambres à coucher")),
    var_short = list(list(
      en = "0 bedrooms",
      fr = "0 CC")),
    description = list(list(
      en = "The number of occupied private dwellings with zero bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes and exclude rooms designed for another use during the day, such as living rooms and dining rooms. One-room private dwellings, such as bachelor or studio apartments, are classified as having zero bedrooms.",
      fr = "Le nombre de logements privés occupés ne comptant aucune chambre à coucher. Les chambres à coucher désignent des pièces utilisées principalement pour dormir et excluent les pièces conçues pour un autre usage pendant la journée, comme les salons et les salles à manger. Les logements privés d’une pièce, comme les studios, sont classés comme n’ayant aucune chambre à coucher."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_one",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
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
      fr = "1 chambre à coucher")),
    var_short = list(list(
      en = "1 bedroom",
      fr = "1 CC")),
    description = list(list(
      en = "The number of occupied private dwellings with one bedroom. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding rooms such as living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant une chambre à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d’autres fins (comme une chambre d’ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_two",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4247"),
    vec_2016 = list("v_CA16_4846"),
    # vec_2011 = list("v_CA11N_2249"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "2 bedrooms",
      fr = "2 chambres à coucher")),
    var_short = list(list(
      en = "2 bedrooms",
      fr = "2 CC")),
    description = list(list(
      en = "The number of occupied private dwellings with two bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant deux chambres à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d’autres fins (comme une chambre d’ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_three",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4248"),
    vec_2016 = list("v_CA16_4847"),
    # vec_2011 = list("v_CA11N_2250"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "3 bedrooms",
      fr = "3 chambres à coucher")),
    var_short = list(list(
      en = "3 bedrooms",
      fr = "3 CC")),
    description = list(list(
      en = "The number of occupied private dwellings with three bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant trois chambres à coucher. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d’autres fins (comme une chambre d’ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
    parent_vec = "private_dwellings",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "bedroom_four",
    type = list((list(
      unit = "count",                      
      aggregation_field = "sum",
      measurement_scale = "scalar"))),
    theme = "Dwelling size",
    vec_2021 = list("v_CA21_4249"),
    vec_2016 = list("v_CA16_4848"),
    # vec_2011 = list("v_CA11N_2251"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "4 or more bedrooms",
      fr = "4 chambres à coucher ou plus")),
    var_short = list(list(
      en = "4+ bedrooms",
      fr = "4+ CC")),
    description = list(list(
      en = "The number of occupied private dwellings with four or more bedrooms. Bedrooms refer to rooms designed mainly for sleeping purposes, including rooms now used for other purposes (such as guest rooms or television rooms), and excluding living rooms and dining rooms.",
      fr = "Le nombre de logements privés occupés comptant quatre chambres à coucher ou plus. Les chambres à coucher désignant des pièces conçues principalement pour dormir, y compris celles maintenant utilisées à d’autres fins (comme une chambre d’ami ou une salle de télévision), et excluant les salons et les salles à manger."
    )),
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
      fr = "Le nombre de logements privés occupés construits avant 1960. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 1961 et 1980. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 1981 et 1990. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 1991 et 2000. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 2001 et 2005. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 2006 et 2010. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 2011 et 2015. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
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
      fr = "Le nombre de logements privés occupés construits entre 2016 et 2020. La période de construction désigne la période au cours de laquelle le logement a été originellement construit. Il s’agit de la période d’achèvement de la construction et non de celle des rénovations, rajouts ou transformations ultérieures. Pour les propriétés ayant plusieurs structures résidentielles, la période de construction correspond à la période au cours de laquelle la structure la plus récente a été complétée."
    )),
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
      fr = "Le nombre de ménages privés composés d’une seule personne"
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
  census_vectors_housing_parent$var_code %in% census_vectors_householdsize$parent_vec, ]

verify_parents(vectors_df = census_vectors_householdsize,
               parents_df = from_hou)

usethis::use_data(census_vectors_householdsize, overwrite = TRUE)

## IMPORT INCOME CENSUS VECTORS ################################################

census_vectors_income <-
  tibble::tibble(
    var_code = "inc_median_income",
    type = list((list(
      unit = "dollar",
      aggregation_field = "med",
      measurement_scale = "scalar"
    ))),
    theme = "Income",
    vec_2021 = list("v_CA21_906"),
    vec_2016 = list("v_CA16_2397"),
    vec_2011 = list("v_CA11N_2562"),
    vec_2006 = list("v_CA06_2000"),
    vec_2001 = list("v_CA01_1634"),
    vec_1996 = list("v_CA1996_1627"),
    var_title = list(list(
      en = "Median household income",
      fr = "Revenu médian des ménages")),
    var_short = list(list(
      en = "Med. inc.",
      fr = "Rev. Méd.")),
    description = list(list(
      en = "The median income of households before tax",
      fr = "Le revenu médian des ménages avant impôt"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_average_income",
    type = list((list(
      unit = "dollar",
      aggregation_field = "avg",
      measurement_scale = "scalar"
    ))),
    theme = "Income",
    vec_2021 = list("v_CA21_915"),
    vec_2016 = list("v_CA16_4985"),
    vec_2011 = list("v_CA11N_2563"),
    vec_2006 = list("v_CA06_2001"),
    vec_2001 = list("v_CA01_1633"),
    vec_1996 = list("v_CA1996_1626"),
    var_title = list(list(
      en = "Average household income",
      fr = "Revenu moyen des ménages")),
    var_short = list(list(
      en = "Avg. inc.",
      fr = "Revenu moy.")),
    description = list(list(
      en = "The average income of households before tax",
      fr = "Le revenu moyen des ménages avant impôt"
    )),
    parent_vec = "private_households",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_50",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
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
    var_title = list(list(
      en = "Income under $50k",
      fr = "Revenu inférieur à 50 000 $")),
    var_short = list(list(
      en = "Inc. <$50k",
      fr = "Rev. <$50k")),
    description = list(list(
      en = "The number of persons whose total income is under $50,000",
      fr = "Le nombre de personnes dont le revenu total est inférieur à 50 000 $"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_100",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list(c("v_CA21_689", "v_CA21_692", "v_CA21_695", "v_CA21_698",
                      "v_CA21_701")),
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
      en = "The number of persons whose total income is between $50,000 and $99,999",
      fr = "Le nombre de personnes dont le revenu total se situe entre 50 000 $ et 99 999 $"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_high",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_704"),
    vec_2016 = list("v_CA16_2421"),
    vec_2011 = list(c("v_CA11N_2544", "v_CA11N_2545", "v_CA11N_2546")),
    vec_2006 = list("v_CA06_1999"),
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
      en = "The number of persons whose total income is $100,000 or more",
      fr = "Le nombre de personnes dont le revenu total est de 100 000 $ ou plus"
    )),
    parent_vec = "with_income",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "inc_limat",
    type = list(list(
      unit = NULL,
      aggregation_field = "pct",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1040"),
    vec_2016 = list("v_CA16_2540"),
    vec_2011 = list(NA),
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
      fr = "La prévalence des faibles revenus dans les ménages privés sur la base de la mesure de faible revenu après impôt (MFR-AT), selon laquelle un ménage est considéré à faible revenu si son revenu après impôt ajusté en fonction de la taille du ménage est inférieur à 50 % du revenu médian après impôt ajusté à l’échelle nationale"
    )),
    parent_vec = "income_status",
    parent = FALSE
  )

census_vectors_income_parent <-
  tibble::tibble(
    var_code = "with_income",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_671"),
    vec_2016 = list("v_CA16_2405"),
    vec_2011 = list("v_CA11N_2533"),
    vec_2006 = list("v_CA06_1988"),
    vec_2001 = list("v_CA01_1621"),
    vec_1996 = list("v_CA1996_1614"),
    var_title = list(list(
      en = "Labour force individuals",
      fr = "Personnes actives"
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
    var_code = "income_status",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Income",
    vec_2021 = list("v_CA21_1010"),
    vec_2016 = list("v_CA16_2510"),
    vec_2011 = list(NA),
    vec_2006 = list(NA),
    vec_2001 = list(NA),
    vec_1996 = list(NA),
    var_title = list(list(
      en = "Households",
      fr = "Ménages"
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
      fr = "Immigrants")),
    var_short = list(list(
      en = "Immigrants",
      fr = "Immigrants")),
    description = list(list(
      en = "The number of persons who are, or have ever been, landed immigrants or permanent residents of Canada, having been granted the right to live in Canada permanently by immigration authorities",
      fr = "Le nombre de personnes qui sont ou ont déjà été des immigrants reçus ou des résidents permanents du Canada, à qui les autorités de l’immigration ont accordé le droit de s’établir au Canada de façon permanente"
    )),
    parent_vec = "population_ph",
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
      fr = "Nouveaux immigrants")),
    var_short = list(list(
      en = "New immigrants",
      fr = "Nouveaux imm.")),
    description = list(list(
      en = "The number of persons who are landed immigrants or permanent residents whose first date of landed immigrant or permanent resident status in Canada was within the five years preceding the census reference day",
      fr = "Le nombre de personnes qui sont des immigrants reçus ou des résidents permanents dont la première date du statut d’immigrant reçu ou de résident permanent au Canada se situe dans les cinq années précédant la date de référence du recensement"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
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
      fr = "Minorités visibles")),
    var_short = list(list(
      en = "Vis. minorities",
      fr = "Minorités vis.")),
    description = list(list(
      en = "The number of persons, other than Indigenous peoples, who self-identify as members of at least one visible minority group as defined by the Employment Equity Act",
      fr = "Le nombre de personnes, à l’exception des peuples autochtones, qui s’identifient comme appartenant à au moins un groupe de minorités visibles tel que défini dans la Loi sur l’équité en matière d’emploi"
    )),
    parent_vec = "population_ph",
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
      fr = "Autochtones")),
    var_short = list(list(
      en = "Indigenous",
      fr = "Autochtones")),
    description = list(list(
      en = "The number of persons who identify as Indigenous, that is, as First Nations (North American Indian), Métis or Inuit, either alone or in combination with another identity",
      fr = "Le nombre de personnes qui s’identifient comme Autochtones, c’est-à-dire comme Premières Nations (Indiens d’Amérique du Nord), Métis ou Inuits, seules ou en combinaison avec une autre identité"
    )),
    parent_vec = "population_ph",
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
      fr = "Résidents non permanents")),
    var_short = list(list(
      en = "Non-permanent",
      fr = "Non-permanents")),
      description = list(list(
        en = "The number of persons from another country with a usual place of residence in Canada who have a work or study permit or have claimed refugee status, as well as their family members living in Canada",
        fr = "Le nombre de personnes provenant d’un autre pays dont le lieu habituel de résidence est au Canada et qui détiennent un permis de travail ou d’études ou ont demandé le statut de réfugié, ainsi que les membres de leur famille vivant au Canada"
      )),
    parent_vec = "population_ph",
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
      fr = "Non-immigrants")),
    var_short = list(list(
      en = "Non-immigrants",
      fr = "Non-immigrants")),
    description = list(list(
      en = "The number of persons who are Canadian citizens by birth and have never been landed immigrants or permanent residents of Canada",
      fr = "Le nombre de personnes qui sont des citoyens canadiens de naissance et qui n’ont jamais été des immigrants reçus ou des résidents permanents du Canada"
    )),
    parent_vec = "population_ph",
    parent = FALSE
  ) |>
  tibble::add_row(
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
      fr = "Citoyens canadiens")),
    var_short = list(list(
      en = "Citizens",
      fr = "Citoyens")),
    description = list(list(
      en = "The number of persons who hold Canadian citizenship, whether by birth in Canada or by naturalization",
      fr = "Le nombre de personnes qui possèdent la citoyenneté canadienne, soit par la naissance au Canada, soit par naturalisation"
    )),
    parent_vec = "population_ph",
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
      fr = "Individus")),
    var_short = list(list(
      en = "Individuals",
      fr = "Individus")),
    description = list(list(
      en = "The total number of persons living in private households",
      fr = "Le nombre total de personnes vivant dans des ménages privés"
    )),
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
    vec_2001 = list(c("v_CA01_1255", "v_CA01_1256", "v_CA01_1264", "v_CA01_1265")),
    vec_1996 = list(c(
      "v_CA1996_1326", "v_CA1996_1327", "v_CA1996_1335",
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
      fr = "Le nombre de personnes occupées dont le principal mode de transport pour la navette entre le domicile et le lieu de travail est de se déplacer comme conducteur ou passager d’une automobile, d’un camion ou d’une fourgonnette"
    )),
    parent_vec = "employment_lf",
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
    vec_2001 = list(c("v_CA01_1258", "v_CA01_1259", "v_CA01_1267", "v_CA01_1268")),
    vec_1996 = list(c(
      "v_CA1996_1329", "v_CA1996_1330", "v_CA1996_1338",
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
    vec_2001 = list(c("v_CA01_1266", "v_CA01_1257")),
    vec_1996 = list(c("v_CA1996_1337", "v_CA1996_1328")),
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
      fr = "Le nombre de personnes occupées dont le principal mode de transport pour la navette entre le domicile et le lieu de travail est le transport en commun, comme l’autobus, le métro, le train léger, le tramway ou le train de banlieue"
    )),
    parent_vec = "employment_lf",
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
    parent_vec = "employment_lf",
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
    parent_vec = "employment_lf",
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
    parent_vec = "employment_lf",
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
    vec_2001 = list(c("v_CA01_1254", "v_CA01_1263")),
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
      fr = "Le nombre de personnes occupées âgées de 15 ans et plus, vivant dans des ménages privés, qui ont un lieu habituel de travail ou n’ont pas d’adresse de travail fixe"
    )),
    parent_vec = NA,
    parent = TRUE
  )


verify_parents(vectors_df = census_vectors_transport,
               parents_df = census_vectors_transport_parent)

census_vectors_transport <- rbind(census_vectors_transport,
                                  census_vectors_transport_parent)

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
      fr = "Le nombre de personnes de 15 ans et plus faisant partie de la population active qui étaient sans travail, disponibles pour travailler et à la recherche d’un emploi, ou mises à pied temporairement en attente de rappel."
    )),
    parent_vec = "employment_lf",
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
    vec_2006 = list(c("v_CA06_1079", "v_CA06_1082")),
    vec_2001 = list(c("v_CA01_1240", "v_CA01_1243", "v_CA01_1248", "v_CA01_1251")),
    vec_1996 = list(c("v_CA1996_1311", "v_CA1996_1314", "v_CA1996_1319", "v_CA1996_1322")),
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
      fr = "Le nombre de personnes occupées ayant un lieu habituel de travail dont le lieu de travail est situé à l’extérieur de leur municipalité (subdivision de recensement) de résidence."
    )),
    parent_vec = "employment_lfupow",
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
      en = "15+ population",
      fr = "Population 15+"
    )),
    description = list(list(
      en = "The total population aged 15 years and over in private households.",
      fr = "La population totale de 15 ans et plus vivant dans des ménages privés."
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
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Household",
    vec_2021 = list(c("v_CA21_502", "v_CA21_505")),
    vec_2016 = list(c("v_CA16_494", "v_CA16_495", "v_CA16_496",
                      "v_CA16_498", "v_CA16_499", "v_CA16_500")),
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
  ) |>
  tibble::add_row(
    var_code = "family_one_person",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Household",
    vec_2021 = list("v_CA21_444"),
    vec_2016 = list("v_CA16_419"),
    vec_2011 = list("v_CA11F_210"),
    vec_2006 = list("v_CA06_129"),
    vec_2001 = list("v_CA01_122"),
    vec_1996 = list("v_CA1996_117"),
    var_title = list(list(
      en = "Living alone",
      fr = "Vivant seul"
    )),
    var_short = list(list(
      en = "Living alone",
      fr = "Vivant seul"
    )),
    description = list(list(
      en = "The number of private households consisting of a single person living alone",
      fr = "Le nombre de ménages privés composés d’une seule personne vivant seule"
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
    vec_2021 = list("v_CA21_443"),
    vec_2016 = list("v_CA16_418"),
    vec_2011 = list("v_CA11F_209"),
    vec_2006 = list("v_CA06_128"),
    vec_2001 = list("v_CA01_121"),
    vec_1996 = list("v_CA1996_116"),
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

verify_parents(vectors_df = census_vectors_family,
               parents_df = census_vectors_family_parent)

census_vectors_family <- rbind(census_vectors_family,
                               census_vectors_family_parent)

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
      fr = "Connaissent seulement l’anglais"
    )),
    var_short = list(list(
      en = "Eng. only",
      fr = "Ang. seulement"
    )),
    description = list(list(
      en = "The number of individuals who know only English as an official language",
      fr = "Nombre de personnes qui connaissent seulement l’anglais comme langue officielle"
    )),
    parent_vec = "c_population",
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
      fr = "Connaissent le français et l’anglais"
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
      fr = "Ne connaissent ni le français ni l’anglais"
    )),
    var_short = list(list(
      en = "Non-official",
      fr = "Non officielle"
    )),
    description = list(list(
      en = "The number of individuals who do not know either of the official languages (French or English)",
      fr = "Nombre de personnes qui ne connaissent ni le français ni l’anglais, les deux langues officielles"
    )),
    parent_vec = "c_population",
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
      fr = "Nombre de personnes qui déclarent l’anglais comme langue parlée le plus souvent à la maison"
    )),
    parent_vec = "c_population",
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
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
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
    var_title = list(list(
      en = "Aged between 0 and 14",
      fr = "Agés de 0 à 14 ans"
    )),
    var_short = list(list(
      en = "0-14 yo",
      fr = "0–14 ans"
    )),
    description = list(list(
      en = "The number of persons aged 0 to 14 years.",
      fr = "Le nombre de personnes âgées de 0 à 14 ans."
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "age_15_64",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Age",
    vec_2021 = list("v_CA21_68"),
    vec_2016 = list("v_CA16_61"),
    vec_2011 = list(paste0("v_CA11F_", c(17, 35, 38, 41, 44, 47, 50, 53, 56, 59))),
    vec_2006 = list(c(paste0("v_CA06_", 7:16), paste0("v_CA06_", 26:35))),
    vec_2001 = list(c(paste0("v_CA01_", 10:19), paste0("v_CA01_", 29:38))),
    vec_1996 = list(c(paste0("v_CA1996_", 15:24), paste0("v_CA1996_", 39:48))),
    var_title = list(list(
      en = "Aged between 15 and 64",
      fr = "Agés entre 15 et 64 ans"
    )),
    var_short = list(list(
      en = "15-64 yo",
      fr = "15–64 ans"
    )),
    description = list(list(
      en = "The number of persons aged 15 to 64 years.",
      fr = "Le nombre de personnes âgées de 15 à 64 ans."
    )),
    parent_vec = "c_population",
    parent = FALSE
  ) |>
  tibble::add_row(
    var_code = "age_65_plus",
    type = list(list(
      unit = "count",
      aggregation_field = "sum",
      measurement_scale = "scalar"
    )),
    theme = "Age",
    vec_2021 = list("v_CA21_251"),
    vec_2016 = list("v_CA16_244"),
    vec_2011 = list(c(paste0("v_CA11F_", c(62, 65, 68, 71, 74)))),
    vec_2006 = list(c(paste0("v_CA06_", 17:21), paste0("v_CA06_", 36:40))),
    vec_2001 = list(c(paste0("v_CA01_", 20:24), paste0("v_CA01_", 39:43))),
    vec_1996 = list(c(paste0("v_CA1996_", 25:29), paste0("v_CA1996_", 49:53))),
    var_title = list(list(
      en = "Aged 65 and above",
      fr = "Personnes âgées de 65 ans et plus"
    )),
    var_short = list(list(
      en = "65+ yo",
      fr = "65+ ans"
    )),
    description = list(list(
      en = "The number of persons aged 65 years and over.",
      fr = "Le nombre de personnes âgées de 65 ans et plus."
    )),
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
  second_penultimate_vecs <- lapply(categories[2:(length(categories) - 1)], \(c) x$vector[x$label %in% c])
  first <- list(x$vector[1:5])
  last <- list(x$vector[which(x$label == 85):nrow(x)])
  tibble::tibble(
    vector = c(first, second_penultimate_vecs, last),
    label  = cleaned_older$`2011`$label,
    year   = year
  )
  
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
    bounds <- strsplit(cat, ":")[[1]]
    from <- bounds[1]
    to   <- bounds[2]
    
    var_title_en   <- sprintf("Persons aged %s to %s years", from, to)
    var_title_fr   <- sprintf("Personnes âgées de %s à %s ans", from, to)
    var_short_en   <- sprintf("%s–%s", from, to)
    var_short_fr   <- sprintf("%s–%s ans", from, to)
    description_en <- sprintf("The number of persons aged %s to %s years.", from, to)
    description_fr <- sprintf("Le nombre de personnes âgées de %s à %s ans.", from, to)
    
  } else {
    var_title_en   <- "Persons aged 85 years and over"
    var_title_fr   <- "Personnes âgées de 85 ans et plus"
    var_short_en   <- "85+"
    var_short_fr   <- "85+ ans"
    description_en <- "The number of persons aged 85 years and over."
    description_fr <- "Le nombre de personnes âgées de 85 ans et plus."
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
    parent_vec = "c_population",
    parent = FALSE
  )
  
})

census_vectors_age_page <- Reduce(rbind, vectors_table)

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
    vec_2001 = list("v_CA01_1387"),
    vec_1996 = list("v_CA1996_1350"),
    var_title = list(list(
      en = "No certificate, diploma or degree",
      fr = "Aucun certificat, diplôme ou grade"
    )),
    var_short = list(list(
      en = "No degree",
      fr = "Sans diplôme"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over without any certificate, diploma or degree.",
      fr = "Le nombre de personnes âgées de 15 ans et plus n’ayant aucun certificat, diplôme ou grade."
    )),
    parent_vec = "population_15plus",
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
    vec_2001 = list("v_CA01_1387"),
    vec_1996 = list("v_CA1996_1351"),
    var_title = list(list(
      en = "Secondary school diploma or equivalent",
      fr = "Diplôme d’études secondaires ou équivalent"
    )),
    var_short = list(list(
      en = "Secondary",
      fr = "Secondaire"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a secondary (high) school diploma or equivalent.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un diplôme d’études secondaires ou un équivalent."
    )),
    parent_vec = "population_15plus",
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
      fr = "Certificat ou diplôme d’un collège, d’un CÉGEP ou autre non universitaire"
    )),
    var_short = list(list(
      en = "College / CEGEP",
      fr = "Collège / CÉGEP"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is from a college, CEGEP or other non-university institution.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade provient d’un collège, d’un CÉGEP ou d’un autre établissement non universitaire."
    )),
    parent_vec = "population_15plus",
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
    var_short = list(list(
      en = "Sub-bch. uni.",
      fr = "Uni. sous bac."
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a university certificate or diploma below the bachelor level.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un certificat ou diplôme universitaire inférieur au niveau du baccalauréat."
    )),
    parent_vec = "population_15plus",
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
    vec_2006 = list(c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268")),
    vec_2001 = list("v_CA01_1397"),
    vec_1996 = list("v_CA1996_1360"),
    var_title = list(list(
      en = "Bachelor and above",
      fr = "Baccalauréat et plus"
    )),
    var_short = list(list(
      en = "Bachelor+",
      fr = "Baccalauréat+"
    )),
    description = list(list(
      en = "The number of persons aged 15 years and over whose highest certificate, diploma or degree is a bachelor’s degree or a higher university credential.",
      fr = "Le nombre de personnes âgées de 15 ans et plus dont le plus haut certificat, diplôme ou grade est un baccalauréat ou un grade universitaire supérieur."
    )),
    parent_vec = "population_15plus",
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
    var_short = list(list(
      en = "Individuals",
      fr = "Individus"
    )),
    description = list(list(
      en = "The total number of persons aged 15 years and over in private households.",
      fr = "Le nombre total de personnes âgées de 15 ans et plus vivant dans des ménages privés."
    )),
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

check_census_vectors <- function(x) {
  # internal max length for short names
  max_short <- 15L
  
  # required base columns
  needed_basic  <- c("var_title", "var_short")
  missing_basic <- setdiff(needed_basic, names(x))
  if (length(missing_basic) > 0) {
    message(
      "Missing required columns for check: ",
      paste(missing_basic, collapse = ", "),
      ". Nothing checked."
    )
    return(invisible(NULL))
  }
  
  # description column: description or explanation
  desc_col <- NULL
  if ("description" %in% names(x)) {
    desc_col <- "description"
  } else if ("explanation" %in% names(x)) {
    desc_col <- "explanation"
  } else {
    message("No 'description' or 'explanation' column found, description checks skipped.")
  }
  
  # helpers ---------------------------------------------------------------
  is_blank <- function(z) is.na(z) | trimws(z) == ""
  
  extract_lang <- function(col, lang) {
    vapply(
      col,
      FUN.VALUE = character(1),
      FUN = function(z) {
        if (is.null(z)) return(NA_character_)
        if (length(z) == 1L && is.atomic(z) && is.na(z)) {
          return(NA_character_)
        }
        # list(en = "...", fr = "...")
        if (is.list(z) && !is.null(z[[lang]])) {
          val <- z[[lang]]
          if (is.null(val) || length(val) == 0L || all(is.na(val))) {
            return(NA_character_)
          }
          return(as.character(val[1]))
        }
        # plain string → treat as EN only
        if (is.atomic(z) && length(z) == 1L && is.character(z)) {
          if (lang == "en") {
            return(as.character(z))
          } else {
            return(NA_character_)
          }
        }
        NA_character_
      }
    )
  }
  # -----------------------------------------------------------------------
  
  n <- nrow(x)
  
  # var_code if available
  var_code <- if ("var_code" %in% names(x)) as.character(x$var_code) else rep(NA_character_, n)
  
  # extract EN/FR for title / short / desc
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
  
  # missing flags
  missing_title_en <- is_blank(title_en)
  missing_title_fr <- is_blank(title_fr)
  missing_short_en <- is_blank(short_en)
  missing_short_fr <- is_blank(short_fr)
  missing_desc_en  <- is_blank(desc_en)
  missing_desc_fr  <- is_blank(desc_fr)
  
  # length flags
  short_en_too_long <- !is.na(short_en) & nchar(short_en) > max_short
  short_fr_too_long <- !is.na(short_fr) & nchar(short_fr) > max_short
  
  # duplicate var_code flags + peers --------------------------------------
  dup_var_code      <- rep(FALSE, n)
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
  
  # any issue on the row
  any_issue <- missing_title_en | missing_title_fr |
    missing_short_en | missing_short_fr |
    (!is.null(desc_col) & (missing_desc_en | missing_desc_fr)) |
    short_en_too_long | short_fr_too_long |
    dup_var_code
  
  idx <- which(any_issue)
  
  # no issues
  if (length(idx) == 0L) {
    message(
      "All census vector documentation checks passed ",
      "(titles, short names and descriptions in EN/FR, short names <= ",
      max_short, " characters, no duplicated var_code)."
    )
    return(invisible(NULL))
  }
  
  # messages per variable -------------------------------------------------
  for (i in idx) {
    vc <- var_code[i]
    if (is.na(vc) || vc == "") vc <- "<NA>"
    
    issues <- character(0)
    
    # missing titles
    if (missing_title_en[i]) issues <- c(issues, "missing English title (var_title en)")
    if (missing_title_fr[i]) issues <- c(issues, "missing French title (var_title fr)")
    
    # missing / too long short names
    if (missing_short_en[i]) {
      issues <- c(issues, "missing English short name (var_short en)")
    } else if (short_en_too_long[i]) {
      issues <- c(
        issues,
        paste0(
          "English short name longer than ", max_short,
          " characters (len = ", nchar(short_en[i]), ")"
        )
      )
    }
    
    if (missing_short_fr[i]) {
      issues <- c(issues, "missing French short name (var_short fr)")
    } else if (short_fr_too_long[i]) {
      issues <- c(
        issues,
        paste0(
          "French short name longer than ", max_short,
          " characters (len = ", nchar(short_fr[i]), ")"
        )
      )
    }
    
    # missing descriptions
    if (!is.null(desc_col)) {
      if (missing_desc_en[i]) issues <- c(issues, "missing English description")
      if (missing_desc_fr[i]) issues <- c(issues, "missing French description")
    }
    
    # duplicated var_code with details
    if (dup_var_code[i]) {
      peers <- dup_var_code_peers[[i]]
      if (!is.null(peers) && length(peers)) {
        issues <- c(
          issues,
          paste0(
            "duplicate var_code also used at rows ",
            paste(peers, collapse = ", ")
          )
        )
      } else {
        issues <- c(issues, "duplicate var_code")
      }
    }
    
    message(
      "var_code '", vc, "' (row ", i, "): ",
      paste(issues, collapse = "; ")
    )
  }
  
  invisible(
    tibble::tibble(
      row_id            = idx,
      var_code          = var_code[idx],
      missing_title_en  = missing_title_en[idx],
      missing_title_fr  = missing_title_fr[idx],
      missing_short_en  = missing_short_en[idx],
      missing_short_fr  = missing_short_fr[idx],
      missing_desc_en   = missing_desc_en[idx],
      missing_desc_fr   = missing_desc_fr[idx],
      short_en_too_long = short_en_too_long[idx],
      short_fr_too_long = short_fr_too_long[idx],
      dup_var_code      = dup_var_code[idx]
    )
  )
}


check_census_vectors(census_vectors_table)

usethis::use_data(census_vectors_table, overwrite = TRUE)