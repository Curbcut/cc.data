## CMHC Full Extraction Script ################################################
##
## Extracts ALL CMHC data using the new long-format functions.
## Caller must set mirai::daemons() before running.
##
## Unique series extracted (from dictionary_V3.R):
##
## Scss survey (Starts & Completions):
##   - Starts (SAAR)                             × Dwelling Type / Intended Market
##   - Starts                                     × Dwelling Type / Intended Market
##   - Completions                                × Dwelling Type / Intended Market
##   - Under Construction                         × Dwelling Type / Intended Market
##   - Length of Construction                     × Dwelling Type / Intended Market
##   - Share absorbed at completion               × Dwelling Type
##
## Rms survey (Rental Market):
##   - Vacancy Rate                               × Bedroom Type / Year of Construction / Structure Size
##   - Average Rent                               × Bedroom Type / Year of Construction / Structure Size
##   - Median Rent                                × Bedroom Type / Year of Construction / Structure Size
##   - Rental Universe                            × Bedroom Type / Year of Construction / Structure Size
##
## Geo levels:  CMA (annual + monthly)  |  CT (annual + monthly)
## No CSD or survey zone extractions in dictionary_V3.
###############################################################################

library(cc.data)

# ── Set up parallel workers ──────────────────────────────────────────────────
mirai::daemons(parallel::detectCores(), dispatcher = TRUE)
on.exit(mirai::daemons(0))

ct_years    <- 2010:2025
ct_months   <- sprintf("%02d", 1:12)

###############################################################################
## 1.  SCSS SURVEY — Starts & Completions
###############################################################################

# ── 1a. Starts (SAAR) — CMA monthly only ────────────────────────────────────
requests_starts_saar_cma <- list(
  list(survey = "Scss", series = "Starts (SAAR)", dimension = NULL)
)
cat("Extracting: Starts (SAAR) — CMA monthly\n")
starts_saar_cma_monthly <- cmhc_get_monthly_cma(requests_starts_saar_cma)

# ── 1b. Starts × Dwelling Type ──────────────────────────────────────────────
requests_starts_dwelling_cma <- list(
  list(survey = "Scss", series = "Starts", dimension = "Dwelling Type")
)
requests_starts_dwelling_ct <- list(
  list(survey = "Scss", series = "Starts", dimension = "Dwelling Type", years = ct_years)
)
requests_starts_dwelling_ct_monthly <- list(
  list(survey = "Scss", series = "Starts", dimension = "Dwelling Type",
       years = ct_years, months = ct_months)
)

cat("Extracting: Starts × Dwelling Type — CMA annual\n")
starts_dwelling_cma_annual  <- cmhc_get_annual_cma(requests_starts_dwelling_cma)
cat("Extracting: Starts × Dwelling Type — CMA monthly\n")
starts_dwelling_cma_monthly <- cmhc_get_monthly_cma(requests_starts_dwelling_cma)
cat("Extracting: Starts × Dwelling Type — CT annual\n")
starts_dwelling_ct_annual   <- cmhc_get_annual_ct(requests_starts_dwelling_ct)
cat("Extracting: Starts × Dwelling Type — CT monthly\n")
starts_dwelling_ct_monthly  <- cmhc_get_monthly_ct(requests_starts_dwelling_ct_monthly)

# ── 1c. Starts × Intended Market ────────────────────────────────────────────
requests_starts_market_cma <- list(
  list(survey = "Scss", series = "Starts", dimension = "Intended Market")
)
requests_starts_market_ct <- list(
  list(survey = "Scss", series = "Starts", dimension = "Intended Market", years = ct_years)
)
requests_starts_market_ct_monthly <- list(
  list(survey = "Scss", series = "Starts", dimension = "Intended Market",
       years = ct_years, months = ct_months)
)

cat("Extracting: Starts × Intended Market — CMA annual\n")
starts_market_cma_annual  <- cmhc_get_annual_cma(requests_starts_market_cma)
cat("Extracting: Starts × Intended Market — CMA monthly\n")
starts_market_cma_monthly <- cmhc_get_monthly_cma(requests_starts_market_cma)
cat("Extracting: Starts × Intended Market — CT annual\n")
starts_market_ct_annual   <- cmhc_get_annual_ct(requests_starts_market_ct)
cat("Extracting: Starts × Intended Market — CT monthly\n")
starts_market_ct_monthly  <- cmhc_get_monthly_ct(requests_starts_market_ct_monthly)

# ── 1d. Completions × Dwelling Type ─────────────────────────────────────────
requests_completions_dwelling_cma <- list(
  list(survey = "Scss", series = "Completions", dimension = "Dwelling Type")
)
requests_completions_dwelling_ct <- list(
  list(survey = "Scss", series = "Completions", dimension = "Dwelling Type", years = ct_years)
)
requests_completions_dwelling_ct_monthly <- list(
  list(survey = "Scss", series = "Completions", dimension = "Dwelling Type",
       years = ct_years, months = ct_months)
)

cat("Extracting: Completions × Dwelling Type — CMA annual\n")
completions_dwelling_cma_annual  <- cmhc_get_annual_cma(requests_completions_dwelling_cma)
cat("Extracting: Completions × Dwelling Type — CMA monthly\n")
completions_dwelling_cma_monthly <- cmhc_get_monthly_cma(requests_completions_dwelling_cma)
cat("Extracting: Completions × Dwelling Type — CT annual\n")
completions_dwelling_ct_annual   <- cmhc_get_annual_ct(requests_completions_dwelling_ct)
cat("Extracting: Completions × Dwelling Type — CT monthly\n")
completions_dwelling_ct_monthly  <- cmhc_get_monthly_ct(requests_completions_dwelling_ct_monthly)

# ── 1e. Completions × Intended Market ───────────────────────────────────────
requests_completions_market_cma <- list(
  list(survey = "Scss", series = "Completions", dimension = "Intended Market")
)
requests_completions_market_ct <- list(
  list(survey = "Scss", series = "Completions", dimension = "Intended Market", years = ct_years)
)
requests_completions_market_ct_monthly <- list(
  list(survey = "Scss", series = "Completions", dimension = "Intended Market",
       years = ct_years, months = ct_months)
)

cat("Extracting: Completions × Intended Market — CMA annual\n")
completions_market_cma_annual  <- cmhc_get_annual_cma(requests_completions_market_cma)
cat("Extracting: Completions × Intended Market — CMA monthly\n")
completions_market_cma_monthly <- cmhc_get_monthly_cma(requests_completions_market_cma)
cat("Extracting: Completions × Intended Market — CT annual\n")
completions_market_ct_annual   <- cmhc_get_annual_ct(requests_completions_market_ct)
cat("Extracting: Completions × Intended Market — CT monthly\n")
completions_market_ct_monthly  <- cmhc_get_monthly_ct(requests_completions_market_ct_monthly)

# ── 1f. Under Construction × Dwelling Type ──────────────────────────────────
requests_uc_dwelling_cma <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Dwelling Type")
)
requests_uc_dwelling_ct <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Dwelling Type", years = ct_years)
)
requests_uc_dwelling_ct_monthly <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Dwelling Type",
       years = ct_years, months = ct_months)
)

cat("Extracting: Under Construction × Dwelling Type — CMA annual\n")
uc_dwelling_cma_annual  <- cmhc_get_annual_cma(requests_uc_dwelling_cma)
cat("Extracting: Under Construction × Dwelling Type — CMA monthly\n")
uc_dwelling_cma_monthly <- cmhc_get_monthly_cma(requests_uc_dwelling_cma)
cat("Extracting: Under Construction × Dwelling Type — CT annual\n")
uc_dwelling_ct_annual   <- cmhc_get_annual_ct(requests_uc_dwelling_ct)
cat("Extracting: Under Construction × Dwelling Type — CT monthly\n")
uc_dwelling_ct_monthly  <- cmhc_get_monthly_ct(requests_uc_dwelling_ct_monthly)

# ── 1g. Under Construction × Intended Market ────────────────────────────────
requests_uc_market_cma <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Intended Market")
)
requests_uc_market_ct <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Intended Market", years = ct_years)
)
requests_uc_market_ct_monthly <- list(
  list(survey = "Scss", series = "Under Construction", dimension = "Intended Market",
       years = ct_years, months = ct_months)
)

cat("Extracting: Under Construction × Intended Market — CMA annual\n")
uc_market_cma_annual  <- cmhc_get_annual_cma(requests_uc_market_cma)
cat("Extracting: Under Construction × Intended Market — CMA monthly\n")
uc_market_cma_monthly <- cmhc_get_monthly_cma(requests_uc_market_cma)
cat("Extracting: Under Construction × Intended Market — CT annual\n")
uc_market_ct_annual   <- cmhc_get_annual_ct(requests_uc_market_ct)
cat("Extracting: Under Construction × Intended Market — CT monthly\n")
uc_market_ct_monthly  <- cmhc_get_monthly_ct(requests_uc_market_ct_monthly)

# ── 1h. Length of Construction × Dwelling Type ──────────────────────────────
requests_loc_dwelling_cma <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Dwelling Type")
)
requests_loc_dwelling_ct <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Dwelling Type", years = ct_years)
)
requests_loc_dwelling_ct_monthly <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Dwelling Type",
       years = ct_years, months = ct_months)
)

cat("Extracting: Length of Construction × Dwelling Type — CMA annual\n")
loc_dwelling_cma_annual  <- cmhc_get_annual_cma(requests_loc_dwelling_cma)
cat("Extracting: Length of Construction × Dwelling Type — CMA monthly\n")
loc_dwelling_cma_monthly <- cmhc_get_monthly_cma(requests_loc_dwelling_cma)
cat("Extracting: Length of Construction × Dwelling Type — CT annual\n")
loc_dwelling_ct_annual   <- cmhc_get_annual_ct(requests_loc_dwelling_ct)
cat("Extracting: Length of Construction × Dwelling Type — CT monthly\n")
loc_dwelling_ct_monthly  <- cmhc_get_monthly_ct(requests_loc_dwelling_ct_monthly)

# ── 1i. Length of Construction × Intended Market ────────────────────────────
requests_loc_market_cma <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Intended Market")
)
requests_loc_market_ct <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Intended Market", years = ct_years)
)
requests_loc_market_ct_monthly <- list(
  list(survey = "Scss", series = "Length of Construction", dimension = "Intended Market",
       years = ct_years, months = ct_months)
)

cat("Extracting: Length of Construction × Intended Market — CMA annual\n")
loc_market_cma_annual  <- cmhc_get_annual_cma(requests_loc_market_cma)
cat("Extracting: Length of Construction × Intended Market — CMA monthly\n")
loc_market_cma_monthly <- cmhc_get_monthly_cma(requests_loc_market_cma)
cat("Extracting: Length of Construction × Intended Market — CT annual\n")
loc_market_ct_annual   <- cmhc_get_annual_ct(requests_loc_market_ct)
cat("Extracting: Length of Construction × Intended Market — CT monthly\n")
loc_market_ct_monthly  <- cmhc_get_monthly_ct(requests_loc_market_ct_monthly)

# ── 1j. Share Absorbed at Completion × Dwelling Type ────────────────────────
requests_absorbed_dwelling_cma <- list(
  list(survey = "Scss", series = "Share absorbed at completion", dimension = "Dwelling Type")
)
requests_absorbed_dwelling_ct <- list(
  list(survey = "Scss", series = "Share absorbed at completion", dimension = "Dwelling Type",
       years = ct_years)
)
requests_absorbed_dwelling_ct_monthly <- list(
  list(survey = "Scss", series = "Share absorbed at completion", dimension = "Dwelling Type",
       years = ct_years, months = ct_months)
)

cat("Extracting: Share Absorbed × Dwelling Type — CMA annual\n")
absorbed_dwelling_cma_annual  <- cmhc_get_annual_cma(requests_absorbed_dwelling_cma)
cat("Extracting: Share Absorbed × Dwelling Type — CMA monthly\n")
absorbed_dwelling_cma_monthly <- cmhc_get_monthly_cma(requests_absorbed_dwelling_cma)
cat("Extracting: Share Absorbed × Dwelling Type — CT annual\n")
absorbed_dwelling_ct_annual   <- cmhc_get_annual_ct(requests_absorbed_dwelling_ct)
cat("Extracting: Share Absorbed × Dwelling Type — CT monthly\n")
absorbed_dwelling_ct_monthly  <- cmhc_get_monthly_ct(requests_absorbed_dwelling_ct_monthly)


###############################################################################
## 2.  RMS SURVEY — Rental Market
###############################################################################

# ── 2a. Vacancy Rate × Bedroom Type ─────────────────────────────────────────
requests_vr_bedroom_cma <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Bedroom Type")
)
requests_vr_bedroom_ct <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Bedroom Type", years = ct_years)
)

cat("Extracting: Vacancy Rate × Bedroom Type — CMA annual\n")
vr_bedroom_cma_annual <- cmhc_get_annual_cma(requests_vr_bedroom_cma)
cat("Extracting: Vacancy Rate × Bedroom Type — CT annual\n")
vr_bedroom_ct_annual  <- cmhc_get_annual_ct(requests_vr_bedroom_ct)

# ── 2b. Vacancy Rate × Year of Construction ─────────────────────────────────
requests_vr_yoc_cma <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Year of Construction")
)
requests_vr_yoc_ct <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Year of Construction", years = ct_years)
)

cat("Extracting: Vacancy Rate × Year of Construction — CMA annual\n")
vr_yoc_cma_annual <- cmhc_get_annual_cma(requests_vr_yoc_cma)
cat("Extracting: Vacancy Rate × Year of Construction — CT annual\n")
vr_yoc_ct_annual  <- cmhc_get_annual_ct(requests_vr_yoc_ct)

# ── 2c. Vacancy Rate × Structure Size ───────────────────────────────────────
requests_vr_size_cma <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Structure Size")
)
requests_vr_size_ct <- list(
  list(survey = "Rms", series = "Vacancy Rate", dimension = "Structure Size", years = ct_years)
)

cat("Extracting: Vacancy Rate × Structure Size — CMA annual\n")
vr_size_cma_annual <- cmhc_get_annual_cma(requests_vr_size_cma)
cat("Extracting: Vacancy Rate × Structure Size — CT annual\n")
vr_size_ct_annual  <- cmhc_get_annual_ct(requests_vr_size_ct)

# ── 2d. Average Rent × Bedroom Type ─────────────────────────────────────────
requests_ar_bedroom_cma <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Bedroom Type")
)
requests_ar_bedroom_ct <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Bedroom Type", years = ct_years)
)

cat("Extracting: Average Rent × Bedroom Type — CMA annual\n")
ar_bedroom_cma_annual <- cmhc_get_annual_cma(requests_ar_bedroom_cma)
cat("Extracting: Average Rent × Bedroom Type — CT annual\n")
ar_bedroom_ct_annual  <- cmhc_get_annual_ct(requests_ar_bedroom_ct)

# ── 2e. Average Rent × Year of Construction ─────────────────────────────────
requests_ar_yoc_cma <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Year of Construction")
)
requests_ar_yoc_ct <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Year of Construction", years = ct_years)
)

cat("Extracting: Average Rent × Year of Construction — CMA annual\n")
ar_yoc_cma_annual <- cmhc_get_annual_cma(requests_ar_yoc_cma)
cat("Extracting: Average Rent × Year of Construction — CT annual\n")
ar_yoc_ct_annual  <- cmhc_get_annual_ct(requests_ar_yoc_ct)

# ── 2f. Average Rent × Structure Size ───────────────────────────────────────
requests_ar_size_cma <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Structure Size")
)
requests_ar_size_ct <- list(
  list(survey = "Rms", series = "Average Rent", dimension = "Structure Size", years = ct_years)
)

cat("Extracting: Average Rent × Structure Size — CMA annual\n")
ar_size_cma_annual <- cmhc_get_annual_cma(requests_ar_size_cma)
cat("Extracting: Average Rent × Structure Size — CT annual\n")
ar_size_ct_annual  <- cmhc_get_annual_ct(requests_ar_size_ct)

# ── 2g. Median Rent × Bedroom Type ──────────────────────────────────────────
requests_mr_bedroom_cma <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type")
)
requests_mr_bedroom_ct <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type", years = ct_years)
)

cat("Extracting: Median Rent × Bedroom Type — CMA annual\n")
mr_bedroom_cma_annual <- cmhc_get_annual_cma(requests_mr_bedroom_cma)
cat("Extracting: Median Rent × Bedroom Type — CT annual\n")
mr_bedroom_ct_annual  <- cmhc_get_annual_ct(requests_mr_bedroom_ct)

# ── 2h. Median Rent × Year of Construction ──────────────────────────────────
requests_mr_yoc_cma <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Year of Construction")
)
requests_mr_yoc_ct <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Year of Construction", years = ct_years)
)

cat("Extracting: Median Rent × Year of Construction — CMA annual\n")
mr_yoc_cma_annual <- cmhc_get_annual_cma(requests_mr_yoc_cma)
cat("Extracting: Median Rent × Year of Construction — CT annual\n")
mr_yoc_ct_annual  <- cmhc_get_annual_ct(requests_mr_yoc_ct)

# ── 2i. Median Rent × Structure Size ────────────────────────────────────────
requests_mr_size_cma <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Structure Size")
)
requests_mr_size_ct <- list(
  list(survey = "Rms", series = "Median Rent", dimension = "Structure Size", years = ct_years)
)

cat("Extracting: Median Rent × Structure Size — CMA annual\n")
mr_size_cma_annual <- cmhc_get_annual_cma(requests_mr_size_cma)
cat("Extracting: Median Rent × Structure Size — CT annual\n")
mr_size_ct_annual  <- cmhc_get_annual_ct(requests_mr_size_ct)

# ── 2j. Rental Universe × Bedroom Type ──────────────────────────────────────
requests_ru_bedroom_cma <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Bedroom Type")
)
requests_ru_bedroom_ct <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Bedroom Type", years = ct_years)
)

cat("Extracting: Rental Universe × Bedroom Type — CMA annual\n")
ru_bedroom_cma_annual <- cmhc_get_annual_cma(requests_ru_bedroom_cma)
cat("Extracting: Rental Universe × Bedroom Type — CT annual\n")
ru_bedroom_ct_annual  <- cmhc_get_annual_ct(requests_ru_bedroom_ct)

# ── 2k. Rental Universe × Year of Construction ──────────────────────────────
requests_ru_yoc_cma <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Year of Construction")
)
requests_ru_yoc_ct <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Year of Construction", years = ct_years)
)

cat("Extracting: Rental Universe × Year of Construction — CMA annual\n")
ru_yoc_cma_annual <- cmhc_get_annual_cma(requests_ru_yoc_cma)
cat("Extracting: Rental Universe × Year of Construction — CT annual\n")
ru_yoc_ct_annual  <- cmhc_get_annual_ct(requests_ru_yoc_ct)

# ── 2l. Rental Universe × Structure Size ────────────────────────────────────
requests_ru_size_cma <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Structure Size")
)
requests_ru_size_ct <- list(
  list(survey = "Rms", series = "Rental Universe", dimension = "Structure Size", years = ct_years)
)

cat("Extracting: Rental Universe × Structure Size — CMA annual\n")
ru_size_cma_annual <- cmhc_get_annual_cma(requests_ru_size_cma)
cat("Extracting: Rental Universe × Structure Size — CT annual\n")
ru_size_ct_annual  <- cmhc_get_annual_ct(requests_ru_size_ct)


###############################################################################
## 3.  CSD — All series (Scss annual+monthly, Rms annual)
###############################################################################

csd_all  <- cc.pipe::get_census_digital_scales(scales = "csd")$csd
csd_uids <- unique(csd_all$id)

csd_years_annual  <- 1996:2024
csd_years_monthly <- 1996:2025
csd_months        <- 1:12

# ── 3a. Scss — CSD annual + monthly ─────────────────────────────────────────
scss_series     <- c("Starts", "Completions", "Under Construction",
                     "Length of Construction", "Share absorbed at completion")
scss_dimensions <- c("Dwelling Type", "Intended Market")

csd_scss_annual  <- list()
csd_scss_monthly <- list()

for (series in scss_series) {
  for (dimension in scss_dimensions) {
    cat(sprintf("Extracting CSD Scss annual: %s × %s\n", series, dimension))
    req_ann <- list(list(survey = "Scss", series = series, dimension = dimension,
                         years = csd_years_annual))
    res <- cmhc_get_annual_csd(req_ann, csd_uids = csd_uids)
    for (v in names(res$CSD)) csd_scss_annual[[v]] <- res$CSD[[v]]
    rm(res); gc()

    cat(sprintf("Extracting CSD Scss monthly: %s × %s\n", series, dimension))
    req_mon <- list(list(survey = "Scss", series = series, dimension = dimension,
                         years = csd_years_monthly, months = csd_months))
    res <- cmhc_get_monthly_csd(req_mon, csd_uids = csd_uids)
    for (v in names(res$CSD)) csd_scss_monthly[[v]] <- res$CSD[[v]]
    rm(res); gc()
  }
}

# ── 3b. Rms — CSD annual only ───────────────────────────────────────────────
# NOTE: CSD script uses "Structure Type" (not "Structure Size" like CMA/CT)
rms_series     <- c("Vacancy Rate", "Average Rent", "Median Rent", "Rental Universe")
rms_dimensions_csd <- c("Bedroom Type", "Year of Construction", "Structure Type")

csd_rms_annual <- list()

for (series in rms_series) {
  for (dimension in rms_dimensions_csd) {
    cat(sprintf("Extracting CSD Rms annual: %s × %s\n", series, dimension))
    req_ann <- list(list(survey = "Rms", series = series, dimension = dimension,
                         years = csd_years_annual))
    res <- cmhc_get_annual_csd(req_ann, csd_uids = csd_uids)
    for (v in names(res$CSD)) csd_rms_annual[[v]] <- res$CSD[[v]]
    rm(res); gc()
  }
}


###############################################################################
## 4.  SUMMARY — Quick inspection
###############################################################################

all_results <- list(
  # Scss — CMA
  starts_saar_cma_monthly          = starts_saar_cma_monthly,
  starts_dwelling_cma_annual       = starts_dwelling_cma_annual,
  starts_dwelling_cma_monthly      = starts_dwelling_cma_monthly,
  starts_market_cma_annual         = starts_market_cma_annual,
  starts_market_cma_monthly        = starts_market_cma_monthly,
  completions_dwelling_cma_annual  = completions_dwelling_cma_annual,
  completions_dwelling_cma_monthly = completions_dwelling_cma_monthly,
  completions_market_cma_annual    = completions_market_cma_annual,
  completions_market_cma_monthly   = completions_market_cma_monthly,
  uc_dwelling_cma_annual           = uc_dwelling_cma_annual,
  uc_dwelling_cma_monthly          = uc_dwelling_cma_monthly,
  uc_market_cma_annual             = uc_market_cma_annual,
  uc_market_cma_monthly            = uc_market_cma_monthly,
  loc_dwelling_cma_annual          = loc_dwelling_cma_annual,
  loc_dwelling_cma_monthly         = loc_dwelling_cma_monthly,
  loc_market_cma_annual            = loc_market_cma_annual,
  loc_market_cma_monthly           = loc_market_cma_monthly,
  absorbed_dwelling_cma_annual     = absorbed_dwelling_cma_annual,
  absorbed_dwelling_cma_monthly    = absorbed_dwelling_cma_monthly,
  # Scss — CT
  starts_dwelling_ct_annual        = starts_dwelling_ct_annual,
  starts_dwelling_ct_monthly       = starts_dwelling_ct_monthly,
  starts_market_ct_annual          = starts_market_ct_annual,
  starts_market_ct_monthly         = starts_market_ct_monthly,
  completions_dwelling_ct_annual   = completions_dwelling_ct_annual,
  completions_dwelling_ct_monthly  = completions_dwelling_ct_monthly,
  completions_market_ct_annual     = completions_market_ct_annual,
  completions_market_ct_monthly    = completions_market_ct_monthly,
  uc_dwelling_ct_annual            = uc_dwelling_ct_annual,
  uc_dwelling_ct_monthly           = uc_dwelling_ct_monthly,
  uc_market_ct_annual              = uc_market_ct_annual,
  uc_market_ct_monthly             = uc_market_ct_monthly,
  loc_dwelling_ct_annual           = loc_dwelling_ct_annual,
  loc_dwelling_ct_monthly          = loc_dwelling_ct_monthly,
  loc_market_ct_annual             = loc_market_ct_annual,
  loc_market_ct_monthly            = loc_market_ct_monthly,
  absorbed_dwelling_ct_annual      = absorbed_dwelling_ct_annual,
  absorbed_dwelling_ct_monthly     = absorbed_dwelling_ct_monthly,
  # Rms — CMA
  vr_bedroom_cma_annual            = vr_bedroom_cma_annual,
  vr_yoc_cma_annual                = vr_yoc_cma_annual,
  vr_size_cma_annual               = vr_size_cma_annual,
  ar_bedroom_cma_annual            = ar_bedroom_cma_annual,
  ar_yoc_cma_annual                = ar_yoc_cma_annual,
  ar_size_cma_annual               = ar_size_cma_annual,
  mr_bedroom_cma_annual            = mr_bedroom_cma_annual,
  mr_yoc_cma_annual                = mr_yoc_cma_annual,
  mr_size_cma_annual               = mr_size_cma_annual,
  ru_bedroom_cma_annual            = ru_bedroom_cma_annual,
  ru_yoc_cma_annual                = ru_yoc_cma_annual,
  ru_size_cma_annual               = ru_size_cma_annual,
  # Rms — CT
  vr_bedroom_ct_annual             = vr_bedroom_ct_annual,
  vr_yoc_ct_annual                 = vr_yoc_ct_annual,
  vr_size_ct_annual                = vr_size_ct_annual,
  ar_bedroom_ct_annual             = ar_bedroom_ct_annual,
  ar_yoc_ct_annual                 = ar_yoc_ct_annual,
  ar_size_ct_annual                = ar_size_ct_annual,
  mr_bedroom_ct_annual             = mr_bedroom_ct_annual,
  mr_yoc_ct_annual                 = mr_yoc_ct_annual,
  mr_size_ct_annual                = mr_size_ct_annual,
  ru_bedroom_ct_annual             = ru_bedroom_ct_annual,
  ru_yoc_ct_annual                 = ru_yoc_ct_annual,
  ru_size_ct_annual                = ru_size_ct_annual,
  # CSD
  csd_scss_annual                  = list(CSD = csd_scss_annual),
  csd_scss_monthly                 = list(CSD = csd_scss_monthly),
  csd_rms_annual                   = list(CSD = csd_rms_annual)
)

# Print summary: how many variables per extraction, sample rows
cat("\n\n=== EXTRACTION SUMMARY ===\n")
for (nm in names(all_results)) {
  res <- all_results[[nm]]
  geo_key <- names(res)[1]  # "CMA" or "CT"
  vars <- res[[geo_key]]
  n_vars <- length(vars)
  total_rows <- sum(vapply(vars, nrow, integer(1)))
  cat(sprintf("  %-40s  %2d variables  %7d rows\n", nm, n_vars, total_rows))
}

# Spot-check: first variable of vacancy rate should have values < 1 (divided by 100)
cat("\n=== SPOT CHECK: Vacancy Rate values (should be < 1) ===\n")
sample_vr <- vr_bedroom_cma_annual$CMA[[1]]
cat("Variable:", names(vr_bedroom_cma_annual$CMA)[1], "\n")
print(head(sample_vr, 10))

cat("\n=== SPOT CHECK: Average Rent values (should be dollars, NOT divided) ===\n")
sample_ar <- ar_bedroom_cma_annual$CMA[[1]]
cat("Variable:", names(ar_bedroom_cma_annual$CMA)[1], "\n")
print(head(sample_ar, 10))

cat("\n=== SPOT CHECK: geo_vintage varies by year ===\n")
cat("Unique geo_vintage values:\n")
print(sort(unique(sample_vr$geo_vintage)))

cat("\nDone.\n")
