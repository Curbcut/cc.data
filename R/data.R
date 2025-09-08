#' All available census years
#'
#' @format ## `census_years`
#' A numeric (year) vector
"census_years"

#' All available census scales
#'
#' @format ## `census_scales`
#' A character (scales) vector
"census_scales"

#' Housing census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_housing`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_housing"

#' Income census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_income`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_income"

#' Identity census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_identity`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_identity"

#' Transport census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_transport`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_transport"

#' Family census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_family`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_family"

#' Language census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_language`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_language"

#' Age census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_age`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_age"

#' Education census vectors
#'
#' The variable code for each year
#'
#' @format ## `census_vectors_education`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_education"

#' All census vectors tables combined
#'
#' The variable code for each year of all the census themes
#'
#' @format ## `census_vectors_table`
#' A data.frame of census variables code
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type; often a list-column of character tags (e.g., "count", "pct", "dollar", "index").}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year; columns named like vec_1996, vec_2001, …, vec_2021 (list-columns of character).}
#'   \item{parent_vectors}{Cancensus codes for parent totals, if any.}
#'   \item{var_title}{Long title.}
#'   \item{var_short}{Short label.}
#'   \item{explanation}{Human-readable explanation.}
#'   \item{exp_q5}{Optional extra explanatory fragment.}
#'   \item{rankings_chr}{Optional ranking labels for buckets.}
#'   \item{parent_vec}{Code of the parent variable (used for normalization when type includes "pct").}
#'   \item{parent}{Logical; TRUE if this row is a parent/total, FALSE otherwise.}
#' }
"census_vectors_table"

#' All census variable codes in a vector
#'
#' @format ## `census_vectors`
#' A character vector
"census_vectors"

#' All census vectors details used for data export on Curbcut
#'
#' @format ## `census_vectors_details`
#' A data.frame the same number of rows as there are Curbcut
#' \code{census_vectors}
#' \describe{
#'   \item{var_code}{The code used to refer to the variables}
#'   \item{vec}{The `cancensus` code for the vector}
#'   \item{vec_label}{The name of the variable}
#'   \item{parent_vec}{The `cancensus` code for the parent vector}
#'   \item{aggregation}{Indicating whether the value is additive or a
#'   transformation}
#'   \item{parent_vec_label}{The name of the parent variable}
#' }
"census_vectors_details"

#' DA ID years dictionary
#'
#' Dictionary to map all IDs of the most recent census to the DA IDs of the
#' previous censuses.
#'
#' @format ## `census_DA_years_dict`
#' A data.frame of the same number of columns as there are years available in
#' the census
#' \describe{
#'   \item{ID_20xx}{All DA IDs of the most recent census}
#'   \item{ID_...}{List of that year's DA IDs for which the DA ID of the most
#'   recent census years intersects.}
#' }
"census_DA_years_dict"

#' All census ID of each scale, from the latest census
#'
#' @format ## `census_all_ids`
#' A named list the same length as the length of `cc.data::census_scales`
"census_all_ids"

#' Links and key from where to download the buildings dataset
#'
#' @format ## `buildings_osm_ms_keys`
#' A data frame of 2 columns and the same number of 13 rows
#' \describe{
#'   \item{osm_link }{The link from where to download buildings data from OSM}
#'   \item{ms_code}{The province key from which to download MS building data}
#'   \item{addresses}{The download link to get the province's database of addresses}
#' }
#' @source <https://github.com/microsoft/CanadianBuildingFootprints>
"buildings_osm_ms_keys"

#' Dictionary for pre-processed accessibility variables
#'
#' Computed using the \code{accessibility_DA_location()} function.
#' Industry name and explanation comes from the SIC code attached to every
#' point.
#'
#' @format ## `accessibility_point_dict`
#' A data frame of 3 columns
#' \describe{
#'   \item{var_code}{The code used to refer to the variables}
#'   \item{industry}{Name of the industry attached to the variable code}
#'   \item{exp}{Explanation of the industry attached to the variable code}
#' }
#' @source Underlying data coming from DMTI. Industry name and explanation
#' retrieved using SIC codes come from
#' <https://www.naics.com/sic-industry-description>
"accessibility_point_dict"

#' All pre-processed accessibility themes
#'
#' @format ## `accessibility_themes`
#' A character vector
"accessibility_themes"

#' URLs of where to get the road network shapefile
#'
#' @format ## `road_network_url`
#' tibble with census years and URLs
"road_network_url"

#' Correspondence Tables for CTs Across Census Years
#'
#' A list of correspondence tables matching Census Tracts (CTs) between the reference year (CA21)
#' and previous census years (e.g., CA16, CA11, etc.), based on spatial intersection.
#'
#' These tables are used to track changes in CT boundaries over time for consistent longitudinal analysis.
#'
#' @format ## `census_ct_correspondences_list`
#' A named list of data frames. Each element corresponds to a pair of census years, and contains:
#' \describe{
#'   \item{geouid_21}{GeoUID of the CT in the reference year (2021)}
#'   \item{geouid_XX}{GeoUID of the CT in the comparison year (e.g., 2016, 2011, etc.)}
#'   \item{status}{Indicates if the CT has remained stable or changed ("stable", "changed")}
#'   \item{cma_code}{CMA code used to generate the correspondence}
#' }
#' The list is named using the format `"correspondence_2021_2016"`, `"correspondence_2021_2011"`, etc.
#'
#' @source Computed using `cancensus` and spatial intersections between CT shapefiles.
"census_ct_correspondences_list"

#' Correspondence Tables for CTs Across Census Years
#'
#' A list of correspondence tables matching Census Tracts (CTs) between the reference year (CA21)
#' and previous census years (e.g., CA16, CA11, etc.), based on spatial intersection.
#'
#' These tables are used to track changes in CT boundaries over time for consistent longitudinal analysis.
#'
#' @format ## `census_csd_correspondences_list`
#' A named list of data frames. Each element corresponds to a pair of census years, and contains:
#' \describe{
#'   \item{geouid_21}{GeoUID of the CT in the reference year (2021)}
#'   \item{geouid_XX}{GeoUID of the CT in the comparison year (e.g., 2016, 2011, etc.)}
#'   \item{status}{Indicates if the CT has remained stable or changed ("stable", "changed")}
#'   \item{cma_code}{CMA code used to generate the correspondence}
#' }
#' The list is named using the format `"correspondence_2021_2016"`, `"correspondence_2021_2011"`, etc.
#'
#' @source Computed using `cancensus` and spatial intersections between CT shapefiles.
"census_csd_correspondences_list"

#' Bilingual NAICS Classification Dictionary (2- to 6-digit)
#'
#' A list containing the full NAICS 2022 classification hierarchy (2-digit to 6-digit levels),
#' downloaded from the official Statistics Canada website in both English and French.
#'
#' This dictionary allows users to match NAICS codes with their official titles in both languages,
#' which is useful for harmonizing business datasets or performing statistical analyses across industries.
#'
#' @format A named list of two elements:
#' \describe{
#'   \item{en}{A tibble containing English NAICS codes and descriptions, with columns:}
#'     \describe{
#'       \item{level}{Hierarchical level of the NAICS code (2, 3, 4, 5, or 6)}
#'       \item{hierarchical_structure}{Full hierarchical string of the NAICS code}
#'       \item{code}{The NAICS code as character (2 to 6 digits)}
#'       \item{parent}{Parent NAICS code}
#'       \item{class_title}{Official English title of the NAICS category}
#'       \item{class_definition}{Official English definition of the NAICS category}
#'     }
#'   \item{fr}{A tibble containing French NAICS codes and descriptions, same structure as \code{en}:}
#'     \describe{
#'       \item{level}{Hierarchical level of the NAICS code (2, 3, 4, 5, or 6)}
#'       \item{hierarchical_structure}{Full hierarchical string of the NAICS code}
#'       \item{code}{The NAICS code as character (2 to 6 digits)}
#'       \item{parent}{Parent NAICS code}
#'       \item{class_title}{Official French title of the NAICS category}
#'       \item{class_definition}{Official French definition of the NAICS category}
#'     }
#' }
#'
#' @source Statistics Canada – NAICS 2022 Version 1.0  
#' ([English](https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=1181553),  
#'  [French](https://www23.statcan.gc.ca/imdb/p3VD_f.pl?Function=getVD&TVD=1181553))
"naics_dictionary"


#' Bedroom-size census vectors
#'
#' Variable codes for each census year (bedroom size theme).
#'
#' @format ## `census_vectors_bedroomsize`
#' A data.frame with one row per variable and columns:
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type (e.g., share, count, index).}
#'   \item{theme}{High-level theme label (e.g., housing, bedroomsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year, stored in columns named like `vec_2021`, `vec_2016`, etc.}
#'   \item{parent_vectors}{Cancensus codes for parent vectors, if any.}
#'   \item{var_title}{Long title of the variable.}
#'   \item{var_short}{Short label of the variable.}
#'   \item{explanation}{Human-readable explanation of the variable.}
#'   \item{exp_q5}{Optional additional explanation text.}
#'   \item{rankings_chr}{Ranking labels or hints used for sorting/tiers.}
#'   \item{parent_vec}{Code of the parent variable, if any.}
#'   \item{parent}{Logical flag indicating whether the row is a parent variable (`FALSE` means directly usable).}
#' }
#' @keywords datasets
"census_vectors_bedroomsize"

#' Building-age census vectors
#'
#' Variable codes for each census year (building age theme).
#'
#' @format ## `census_vectors_buildingage`
#' A data.frame with one row per variable and columns:
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type (e.g., share, count, index).}
#'   \item{theme}{High-level theme label (e.g., housing, buildingage).}
#'   \item{vec_...}{Cancensus vector code(s) by year, stored in columns named like `vec_2021`, `vec_2016`, etc.}
#'   \item{parent_vectors}{Cancensus codes for parent vectors, if any.}
#'   \item{var_title}{Long title of the variable.}
#'   \item{var_short}{Short label of the variable.}
#'   \item{explanation}{Human-readable explanation of the variable.}
#'   \item{exp_q5}{Optional additional explanation text.}
#'   \item{rankings_chr}{Ranking labels or hints used for sorting/tiers.}
#'   \item{parent_vec}{Code of the parent variable, if any.}
#'   \item{parent}{Logical flag indicating whether the row is a parent variable (`FALSE` means directly usable).}
#' }
#' @keywords datasets
"census_vectors_buildingage"

#' Employment census vectors
#'
#' Variable codes for each census year (employment theme).
#'
#' @format ## `census_vectors_employment`
#' A data.frame with one row per variable and columns:
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type (e.g., share, count, index).}
#'   \item{theme}{High-level theme label (e.g., employment).}
#'   \item{vec_...}{Cancensus vector code(s) by year, stored in columns named like `vec_2021`, `vec_2016`, etc.}
#'   \item{parent_vectors}{Cancensus codes for parent vectors, if any.}
#'   \item{var_title}{Long title of the variable.}
#'   \item{var_short}{Short label of the variable.}
#'   \item{explanation}{Human-readable explanation of the variable.}
#'   \item{exp_q5}{Optional additional explanation text.}
#'   \item{rankings_chr}{Ranking labels or hints used for sorting/tiers.}
#'   \item{parent_vec}{Code of the parent variable, if any.}
#'   \item{parent}{Logical flag indicating whether the row is a parent variable (`FALSE` means directly usable).}
#' }
#' @keywords datasets
"census_vectors_employment"

#' Household-size census vectors
#'
#' Variable codes for each census year (household size theme).
#'
#' @format ## `census_vectors_householdsize`
#' A data.frame with one row per variable and columns:
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type (e.g., share, count, index).}
#'   \item{theme}{High-level theme label (e.g., householdsize).}
#'   \item{vec_...}{Cancensus vector code(s) by year, stored in columns named like `vec_2021`, `vec_2016`, etc.}
#'   \item{parent_vectors}{Cancensus codes for parent vectors, if any.}
#'   \item{var_title}{Long title of the variable.}
#'   \item{var_short}{Short label of the variable.}
#'   \item{explanation}{Human-readable explanation of the variable.}
#'   \item{exp_q5}{Optional additional explanation text.}
#'   \item{rankings_chr}{Ranking labels or hints used for sorting/tiers.}
#'   \item{parent_vec}{Code of the parent variable, if any.}
#'   \item{parent}{Logical flag indicating whether the row is a parent variable (`FALSE` means directly usable).}
#' }
#' @keywords datasets
"census_vectors_householdsize"

#' Typology census vectors
#'
#' Variable codes for each census year (typology theme).
#'
#' @format ## `census_vectors_typology`
#' A data.frame with one row per variable and columns:
#' \describe{
#'   \item{var_code}{Internal variable code used by the package.}
#'   \item{type}{Variable type (e.g., share, count, index).}
#'   \item{theme}{High-level theme label (e.g., typology).}
#'   \item{vec_...}{Cancensus vector code(s) by year, stored in columns named like `vec_2021`, `vec_2016`, etc.}
#'   \item{parent_vectors}{Cancensus codes for parent vectors, if any.}
#'   \item{var_title}{Long title of the variable.}
#'   \item{var_short}{Short label of the variable.}
#'   \item{explanation}{Human-readable explanation of the variable.}
#'   \item{exp_q5}{Optional additional explanation text.}
#'   \item{rankings_chr}{Ranking labels or hints used for sorting/tiers.}
#'   \item{parent_vec}{Code of the parent variable, if any.}
#'   \item{parent}{Logical flag indicating whether the row is a parent variable (`FALSE` means directly usable).}
#' }
#' @keywords datasets
"census_vectors_typology"
