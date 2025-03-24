#'cmhc_vectors_details
#' 
#' CMHC Variable Metadata Table
#' 
#' This tibble provides detailed metadata about CMHC 
#' variables used in the housing dataset. It includes information about variable codes, descriptions, 
#' data sources, survey types, dimensions, time coverage, and measurement scales.

cmhc_vectors_details <-
      tibble::tibble(
        var_code = "st_dt_sgl",
        var_long = "starts_dwelling_type_single",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new single housing construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_dt_semi",
        var_long = "starts_dwelling_type_semi_detached",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new semi-detached housing construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_dt_rw",
        var_long = "starts_dwelling_type_row",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new row housing construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_dt_apt",
        var_long = "starts_dwelling_type_apartment",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new appartement construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_dt_all",
        var_long = "starts_dwelling_type_all",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new housing construction starts (all types)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_unk",
        var_long = "starts_intended_market_unknown",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new housing construction starts (market unknown)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_hm",
        var_long = "starts_intended_market_homeowner",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new homeowner-occupied housing construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_rnt",
        var_long = "starts_intended_market_rental",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new rental housing construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_cnd",
        var_long = "starts_intended_market_condo",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new condo construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_coop",
        var_long = "starts_intended_market_co_op",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new co-op construction starts",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "st_im_all",
        var_long = "starts_intended_market_all",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new housing construction starts (all market)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "starts_saar",
        var_long = "st_saar",
        source = "CMHC",
        survey = "Scss",
        series = "starts",
        dimension = "intended Market",
        period = "2002:2025",
        frequency = "annual",
        scale = "CMA",
        var_title = "new housing construction starts (seasonally adjusted annual rate)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_dt_sgl",
        var_long = "completions_dwelling_type_single",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new single housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_dt_semi",
        var_long = "completions_dwelling_type_semi_detached",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new semi-detached housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_dt_rw",
        var_long = "completions_dwelling_type_row",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new row housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_dt_apt",
        var_long = "completions_dwelling_type_apartment",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new apartment completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_dt_all",
        var_long = "completions_dwelling_type_all",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new housing completions (all type)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_im_hm",
        var_long = "completions_intended_market_homeowner",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new homeowner housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_im_rnt",
        var_long = "completions_intended_market_rental",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new rental housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_im_cnd",
        var_long = "completions_intended_market_condo",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new condo completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_im_coop",
        var_long = "completions_intended_market_co_op",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new co-op housing completions",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "comp_im_all",
        var_long = "completions_intended_market_all",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "intended Market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "new housing completions (all market)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_dt_sgl",
        var_long = "under_construction_dwelling_type_single",
        source = "CMHC",
        survey = "Scss",
        series = "completions",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "single housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_dt_semi",
        var_long = "under_construction_dwelling_type_semi_detached",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "semi detached housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_dt_rw",
        var_long = "under_construction_dwelling_type_row",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "row housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_dt_apt",
        var_long = "under_construction_dwelling_type_apartment",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "apartment under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_dt_all",
        var_long = "under_construction_dwelling_type_all",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "housing under construction (all type)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_unk",
        var_long = "under_construction_intended_market_unknown",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "housing under construction (market unknown)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_hm",
        var_long = "under_construction_intended_market_homeowner",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "homeowner housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_rnt",
        var_long = "under_construction_intended_market_rental",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "rental housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_cnd",
        var_long = "under_construction_intended_market_condo",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "condo under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_coop",
        var_long = "under_construction_intended_market_co_op",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "co-op under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "uc_im_all",
        var_long = "under_construction_intended_market_all",
        source = "CMHC",
        survey = "Scss",
        series = "under construction",
        dimension = "intended market",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "housing under construction (all market)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "lc_dt_sgl",
        var_long = "length_of_construction_dwelling_type_single",
        source = "CMHC",
        survey = "Scss",
        series = "lenght of construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "Length of single housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "lc_dt_semi",
        var_long = "length_of_construction_dwelling_type_semi_detached",
        source = "CMHC",
        survey = "Scss",
        series = "lenght of construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "Length of semi-detached housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "lc_dt_rw",
        var_long = "length_of_construction_dwelling_type_row",
        source = "CMHC",
        survey = "Scss",
        series = "lenght of construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "Length of semi-detached housing under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "lc_dt_apt",
        var_long = "length_of_construction_dwelling_type_apartment",
        source = "CMHC",
        survey = "Scss",
        series = "lenght of construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "Length of apartment under construction",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "lc_dt_all",
        var_long = "length_of_construction_dwelling_type_all",
        source = "CMHC",
        survey = "Scss",
        series = "lenght of construction",
        dimension = "dwelling type",
        period = "1990:2025",
        frequency = "monthly",
        scale = "CMA",
        var_title = "Length of construction (all type)",
        type = "count"
        ) |>
        
        tibble::add_row(
          var_code = "lc_im_hm",
          var_long = "length_of_construction_intended_market_homeowner",
          source = "CMHC",
          survey = "Scss",
          series = "under construction",
          dimension = "intended market",
          period = "1990:2025",
          frequency = "monthly",
          scale = "CMA",
          var_title = "lenght of homeowner housing construction",
          type = "count"
        ) |>
          tibble::add_row(
            var_code = "lc_im_rnt",
            var_long = "length_of_construction_intended_market_rental",
            source = "CMHC",
            survey = "Scss",
            series = "under construction",
            dimension = "intended market",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "lenght of rental housing construction",
            type = "count"
          ) |>
          tibble::add_row(
            var_code = "lc_im_cnd",
            var_long = "length_of_construction_intended_market_condo",
            source = "CMHC",
            survey = "Scss",
            series = "under construction",
            dimension = "intended market",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "lenght of condo under construction",
            type = "count"
          ) |>
          tibble::add_row(
            var_code = "lc_im_coop",
            var_long = "length_of_construction_intended_market_co_op",
            source = "CMHC",
            survey = "Scss",
            series = "under construction",
            dimension = "intended market",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "lenght of co-op under construction",
            type = "count"
          ) |>
          tibble::add_row(
            var_code = "lc_im_all",
            var_long = "length_of_construction_intended_market_all",
            source = "CMHC",
            survey = "Scss",
            series = "under construction",
            dimension = "intended market",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "lenght of housing under construction (all market)",
            type = "count"
          ) |>
          tibble::add_row(
            var_code = "sac_dt_sgl",
            var_long = "share_absorbed_at_completion_dwelling_type_single",
            source = "CMHC",
            survey = "Scss",
            series = "Share absorbed at completion",
            dimension = "Dwelling Type",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "Percentage of single units absorbed at completion",
            type = "pct"
          ) |>
          tibble::add_row(
            var_code = "sac_dt_semi",
            var_long = "share_absorbed_at_completion_dwelling_type_semi_detached",
            source = "CMHC",
            survey = "Scss",
            series = "Share absorbed at completion",
            dimension = "Dwelling Type",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "Percentage of semi-detached units absorbed at completion",
            type = "pct"
          ) |>
          tibble::add_row(
            var_code = "sac_dt_rw",
            var_long = "share_absorbed_at_completion_dwelling_type_row",
            source = "CMHC",
            survey = "Scss",
            series = "Share absorbed at completion",
            dimension = "Dwelling Type",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "Percentage of row absorbed at completion",
            type = "pct"
          ) |>
          tibble::add_row(
            var_code = "sac_dt_apt",
            var_long = "share_absorbed_at_completion_dwelling_type_apartment",
            source = "CMHC",
            survey = "Scss",
            series = "Share absorbed at completion",
            dimension = "Dwelling Type",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "Percentage of apartment absorbed at completion",
            type = "pct"
          ) |>
          tibble::add_row(
            var_code = "sac_dt_all",
            var_long = "share_absorbed_at_completion_dwelling_type_apartment",
            source = "CMHC",
            survey = "Scss",
            series = "Share absorbed at completion",
            dimension = "Dwelling Type",
            period = "1990:2025",
            frequency = "monthly",
            scale = "CMA",
            var_title = "Percentage of units absorbed at completion (all type)",
            type = "pct" 
          ) |>
          tibble::add_row(
            var_code = "vacr_bt_bach",
            var_long = "vacancy_rate_bedroom_type_bachelor",
            source = "CMHC",
            survey = "Rms",
            series = "vacancy rate",
            dimension = "bedroom type",
            period = "1990:2024",
            frequency = "annual",
            scale = "CMA",
            var_title = "vacancy rate for bachelor units",
            type = "pct" 
          ) |>
          tibble::add_row(
            var_code = "vacr_bt_1bd",
            var_long = "vacancy_rate_bedroom_type_1_bedroom",
            source = "CMHC",
            survey = "Rms",
            series = "vacancy rate",
            dimension = "bedroom type",
            period = "1990:2024",
            frequency = "annual",
            scale = "CMA",
            var_title = "vacancy rate for one-bedroom units",
            type = "pct" 
          ) |>
          tibble::add_row(
            var_code = "vacr_bt_2bd",
            var_long = "vacancy_rate_bedroom_type_2_bedroom",
            source = "CMHC",
            survey = "Rms",
            series = "vacancy rate",
            dimension = "bedroom type",
            period = "1990:2024",
            frequency = "annual",
            scale = "CMA",
            var_title = "vacancy rate for two-bedroom units",
            type = "pct" 
          ) |>
          tibble::add_row(
            var_code = "vacr_bt_3bd",
            var_long = "vacancy_rate_bedroom_type_3_bedroom",
            source = "CMHC",
            survey = "Rms",
            series = "vacancy rate",
            dimension = "bedroom type",
            period = "1990:2024",
            frequency = "annual",
            scale = "CMA",
            var_title = "vacancy rate for three-bedroom units or more",
            type = "pct" 
          ) |>
          tibble::add_row(
            var_code = "vacr_bt_all",
            var_long = "vacancy_rate_bedroom_type_total",
            source = "CMHC",
            survey = "Rms",
            series = "vacancy rate",
            dimension = "bedroom type",
            period = "1990:2024",
            frequency = "annual",
            scale = "CMA",
            var_title = "vacancy rate (all type)",
            type = "pct" 
          ) |>
            tibble::add_row(
              var_code = "vacr_yc_b60",
              var_long = "vacancy_rate_year_of_construction_before_1960",
              source = "CMHC",
              survey = "Rms",
              series = "vacancy rate",
              dimension = "year of construction",
              period = "1990:2024",
              frequency = "annual",
              scale = "CMA",
              var_title = "vacancy rate for units constructed before 1960",
              type = "pct" 
            ) |>
              tibble::add_row(
                var_code = "vacr_yc_60_79",
                var_long = "vacancy_rate_year_of_construction_1960_1979",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for units constructed between 1960 and 1979",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_yc_80_99",
                var_long = "vacancy_rate_year_of_construction_1980_1999",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for units constructed between 1980 and 1999",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_yc_2000",
                var_long = "vacancy_rate_year_of_construction_2000_or_later",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for units constructed in 2000 or later",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_yc_all",
                var_long = "vacancy_rate_year_of_construction_total",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate (all years)",
                type = "pct" 
                
              ) |>
            tibble::add_row(
              var_code = "vacr_sz_3_5u",
              var_long = "vacancy_rate_structure_size_3_5_units",
              source = "CMHC",
              survey = "Rms",
              series = "vacancy rate",
              dimension = "structure size",
              period = "1990:2024",
              frequency = "annual",
              scale = "CMA",
              var_title = "vacancy rate for 3 to 5-unit housing" ,
              type = "pct" 
            ) |>
              tibble::add_row(
                var_code = "vacr_sz_6_19u",
                var_long = "vacancy_rate_structure_size_6_19_units",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for 6 to 19-unit housing",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_sz_20_49u",
                var_long = "vacancy_rate_structure_size_20_49_units",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for 20 to 49-unit housing",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_sz_50_199u",
                var_long = "vacancy_rate_structure_size_50_199_units",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for 50 to 199-unit housing",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_sz_all",
                var_long = "vacancy_rate_structure_size_total",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate (all structure siz)",
                type = "pct" 
                
                
              ) |>
            tibble::add_row(
              var_code = "vacr_rr_lt750",
              var_long = "vacancy_rate_rent_ranges_less_than_750",
              source = "CMHC",
              survey = "Rms",
              series = "vacancy rate",
              dimension = "rent ranges",
              period = "1990:2024",
              frequency = "annual",
              scale = "CMA",
              var_title = "vacancy rate for housing with rent under 750 $" ,
              type = "pct" 
            ) |>
              tibble::add_row(
                var_code = "vacr_rr_750_999",
                var_long = "vacancy_rate_rent_ranges_750_999",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for housing with rent 750-999 $",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_rr_1000_1250",
                var_long = "vacancy_rate_rent_ranges_1_000_1_249",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for housing with rent 1000-1249 $",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_rr_1250_1500",
                var_long = "vacancy_rate_rent_ranges_1_250_1_499",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for housing with rent 1250-1499 $",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_rr_1500",
                var_long = "vacancy_rate_rent_ranges_1_500",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for housing with rent 1500 $ or above",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_rr_nmunk",
                var_long = "vacancy_rate_rent_ranges_non_market_unknown",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate for housing with unknow rent",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "vacr_rr_all",
                var_long = "vacancy_rate_rent_ranges_total",
                source = "CMHC",
                survey = "Rms",
                series = "vacancy rate",
                dimension = "rent ranges",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "vacancy rate (all rent ranges",
                type = "pct" 
              ) |>
              tibble::add_row(
                var_code = "avgr_bt_bach",
                var_long = "average_rent_bedroom_type_bachelor",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for bachelor housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_bt_1bd",
                var_long = "average_rent_bedroom_type_1_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for one-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_bt_2bd",
                var_long = "average_rent_bedroom_type_2_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for two-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_bt_3bd",
                var_long = "average_rent_bedroom_type_3_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for three-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_bt_all",
                var_long = "average_rent_bedroom_type_total",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent (all type)",
                type = "dollar"
                
              ) |>
              tibble::add_row(
                var_code = "avgr_yc_b60",
                var_long = "average_rent_year_of_construction_before_1960",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for dwellings constructed before 1960",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_yc_60_79",
                var_long = "average_rent_year_of_construction_1960_1979",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for dwellings constructed between 1960 and 1979",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_yc_80_99",
                var_long = "average_rent_year_of_construction_1980_1999",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for dwellings constructed between 1980 and 1999",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_yc_2000+",
                var_long = "average_rent_year_of_construction_2000_or_later",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for dwellings constructed in 2000 or later",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_yc_all",
                var_long = "average_rent_year_of_construction_total",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent (all years)",
                type = "dollar"
                
              ) |>
              tibble::add_row(
                var_code = "avgr_sz_3_5u",
                var_long = "average_rent_structure_size_3_5_units",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for 3 to 5-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_sz_6_19u",
                var_long = "average_rent_structure_size_6_19_units",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for 6 to 19-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_sz_20_49u",
                var_long = "average_rent_structure_size_20_49_units",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for 20 to 49-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_sz_50_199u",
                var_long = "average_rent_structure_size_50_199_units",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent for 50 to 199-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "avgr_sz_all",
                var_long = "average_rent_structure_size_total",
                source = "CMHC",
                survey = "Rms",
                series = "average rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "average rent (structure size)",
                type = "dollar"
              ) |>
              tibble::add_row(
                var_code = "medr_bt_bach",
                var_long = "median_rent_bedroom_type_bachelor",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for bachelor housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_bt_1bd",
                var_long = "median_rent_bedroom_type_1_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for one-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_bt_2bd",
                var_long = "median_rent_bedroom_type_2_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for two-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_bt_3bd",
                var_long = "median_rent_bedroom_type_3_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for three-bedroom units",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_bt_all",
                var_long = "median_rent_bedroom_type_total",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent (all type)",
                type = "dollar"
                
              ) |>
              tibble::add_row(
                var_code = "medr_yc_b60",
                var_long = "median_rent_year_of_construction_before_1960",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for dwellings constructed before 1960",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_yc_60_79",
                var_long = "median_rent_year_of_construction_1960_1979",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for dwellings constructed between 1960 and 1979",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_yc_80_99",
                var_long = "median_rent_year_of_construction_1980_1999",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for dwellings constructed between 1980 and 1999",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_yc_2000+",
                var_long = "median_rent_year_of_construction_2000_or_later",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for dwellings constructed in 2000 or later",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_yc_all",
                var_long = "median_rent_year_of_construction_total",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "year of construction",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent (all years)",
                type = "dollar"
                
              ) |>
              tibble::add_row(
                var_code = "medr_sz_3_5u",
                var_long = "median_rent_structure_size_3_5_units",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for 3 to 5-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_sz_6_19u",
                var_long = "median_rent_structure_size_6_19_units",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for 6 to 19-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_sz_20_49u",
                var_long = "median_rent_structure_size_20_49_units",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for 20 to 49-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_sz_50_199u",
                var_long = "median_rent_structure_size_50_199_units",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent for 50 to 199-unit housing",
                type = "dollar" 
              ) |>
              tibble::add_row(
                var_code = "medr_sz_all",
                var_long = "median_rent_structure_size_total",
                source = "CMHC",
                survey = "Rms",
                series = "median rent",
                dimension = "structure size",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "median rent (structure size)",
                type = "dollar"
              ) |>
              tibble::add_row(
              var_code = "ru_bt_bach",
              var_long = "rental_universe_bedroom_type_bachelor",
              source = "CMHC",
              survey = "Rms",
              series = "rental universe",
              dimension = "bedroom type",
              period = "1990:2024",
              frequency = "annual",
              scale = "CMA",
              var_title = "rental universe for bachelor units",
              type = "count" 
            ) |>
              tibble::add_row(
                var_code = "ru_bt_1bd",
                var_long = "rental_universe_bedroom_type_1_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "rental universe",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "Rental universe for one-bedroom units",
                type = "count" 
              ) |>
              tibble::add_row(
                var_code = "ru_bt_2bd",
                var_long = "rental_universe_bedroom_type_2_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "rental universe",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "Rental universe for two-bedroom units",
                type = "count" 
              ) |>
              tibble::add_row(
                var_code = "ru_bt_3bd",
                var_long = "rental_universe_bedroom_type_3_bedroom",
                source = "CMHC",
                survey = "Rms",
                series = "rental universe",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "Rental universe for three-bedroom units",
                type = "count" 
              ) |>
              tibble::add_row(
                var_code = "ru_bt_all",
                var_long = "rental_universe_bedroom_type_total",
                source = "CMHC",
                survey = "Rms",
                series = "rental universe",
                dimension = "bedroom type",
                period = "1990:2024",
                frequency = "annual",
                scale = "CMA",
                var_title = "rental universe (all type)",
                type = "count" 
                
              ) |>
      tibble::add_row(
        var_code = "ru_yc_b60",
        var_long = "rental_universe_year_of_construction_before_1960",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "year of construction",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "Rental universe for dwellings constructed before 1960",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_yc_60_79",
        var_long = "rental_universe_year_of_construction_1960_1979",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "year of construction",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for dwellings constructed between 1960 and 1979",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_yc_80_99",
        var_long = "rental_universe_year_of_construction_1980_1999",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "year of construction",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for dwellings constructed between 1980 and 1999",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_yc_2000+",
        var_long = "rental_universe_year_of_construction_2000_or_later",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "year of construction",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for dwellings constructed in 2000 or later",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_yc_all",
        var_long = "rental_universe_year_of_construction_total",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "year of construction",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe (all years)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "ru_sz_3_5u",
        var_long = "rental_universe_structure_size_3_5_units",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for 3 to 5-unit housing",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_sz_6_19u",
        var_long = "rental_universe_structure_size_6_19_units",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for 6 to 19-unit housing",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_sz_20_49u",
        var_long = "rental_universe_structure_size_20_49_units",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for 20 to 49-unit housing",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_sz_50_199u",
        var_long = "rental_universe_structure_size_50_199_units",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for 50 to 199-unit housing",
        type = "count" 
      ) |>
      tibble::add_row(
        var_code = "ru_sz_all",
        var_long = "rental_universe_structure_size_total",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe (structure size)",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "vacr_sz_200u",
        var_long = "vacancy_rate_structure_size_200_units",
        source = "CMHC",
        survey = "Rms",
        series = "vacancy rate",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "vacancy rate for 200 unit housing or more",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "avgr_sz_200u",
        var_long = "average_rent_structure_size_200_units",
        source = "CMHC",
        survey = "Rms",
        series = "average rent",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "average rent for 200 unit housing or more",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "medr_sz_200u",
        var_long = "median_rent_structure_size_200_units",
        source = "CMHC",
        survey = "Rms",
        series = "median rent",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "median rent for 200 unit housing or more",
        type = "count"
      ) |>
      tibble::add_row(
        var_code = "ru_sz_200u",
        var_long = "rental_universe_structure_size_200_units",
        source = "CMHC",
        survey = "Rms",
        series = "rental universe",
        dimension = "structure size",
        period = "1990:2024",
        frequency = "annual",
        scale = "CMA",
        var_title = "rental universe for 200 unit housing or more",
        type = "count"
      
)

usethis::use_data(cmhc_vectors_details, overwrite = TRUE)