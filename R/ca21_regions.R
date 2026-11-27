#' 2021 census provinces and CMAs, fixed
#'
#' A copy of CensusMapper's CA21 region list (`cancensus::list_census_regions("CA21")`,
#' taken on 2026-11-26), so the functions that only need ids and names make no call
#' to CensusMapper and need no API key. Census geography for 2021 does not change.
#'
#' @param level `"PR"` (13 provinces and territories) or `"CMA"` (the 149
#'   CMAs and census agglomerations, as CensusMapper's CMA level holds them).
#' @param parts Also return Ottawa - Gatineau's Ontario and Quebec parts (35505,
#'   24505), as `list_census_regions()` does; `get_census()` does not.
#' @return A data frame: `GeoUID` (and its copy `region`), `name` as CensusMapper
#'   writes it ("Toronto"), and `Region Name` as `cancensus::get_census()` returns
#'   it ("Toronto (B)", "Alberta (Alta.)").
#' @keywords internal
ca21_regions <- function(level = c("PR", "CMA"), parts = FALSE) {
  level <- match.arg(level)
  r <- if (level == "PR") .ca21_pr else .ca21_cma
  if (!parts) r <- r[!r$GeoUID %in% c("35505", "24505"), ]
  r$region <- r$GeoUID
  r$`Region Name` <- paste0(r$name, " (", r$status, ")")
  rownames(r) <- NULL
  r[c("GeoUID", "region", "name", "Region Name")]
}

#' @noRd
.ca21_pr <- data.frame(
  GeoUID = c(
    "10", "11", "12", "13", "24", "35", "46", "47", "48", "59", "60", "61", "62"),
  name = c(
    "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick",
    "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon",
    "Northwest Territories", "Nunavut"),
  status = c(
    "N.L.", "P.E.I.", "N.S.", "N.B.", "Que.", "Ont.", "Man.", "Sask.", "Alta.", "B.C.",
    "Y.T.", "N.W.T.", "Nvt."),
  stringsAsFactors = FALSE
)

#' @noRd
.ca21_cma <- data.frame(
  GeoUID = c(
    "10001", "10010", "10011", "10015", "11105", "11110", "12205", "12210", "12215",
    "12220", "12225", "13305", "13310", "13320", "13328", "13329", "13330", "13335",
    "24330", "24403", "24404", "24405", "24406", "24408", "24410", "24411", "24412",
    "24421", "24423", "24428", "24430", "24433", "24437", "24440", "24442", "24444",
    "24447", "24454", "24456", "24462", "24465", "24467", "24468", "24480", "24481",
    "24485", "24502", "24505", "330", "35501", "35502", "35505", "35512", "35515", "35516",
    "35521", "35522", "35527", "35528", "35529", "35530", "35531", "35532", "35533",
    "35535", "35537", "35539", "35541", "35543", "35544", "35546", "35547", "35550",
    "35553", "35555", "35556", "35559", "35563", "35565", "35566", "35567", "35568",
    "35569", "35571", "35580", "35582", "35586", "35595", "35598", "46602", "46603",
    "46605", "46607", "46610", "46640", "47705", "47710", "47715", "47720", "47725",
    "47735", "47745", "47750", "47755", "47840", "48806", "48810", "48820", "48821",
    "48825", "48826", "48828", "48830", "48831", "48832", "48833", "48835", "48840",
    "48865", "502", "505", "59905", "59907", "59910", "59913", "59915", "59918", "59920",
    "59925", "59930", "59932", "59933", "59934", "59935", "59936", "59937", "59938",
    "59939", "59940", "59943", "59944", "59945", "59950", "59952", "59955", "59965",
    "59975", "59977", "60990", "61995", "840"),
  name = c(
    "St. John's", "Grand Falls-Windsor", "Gander", "Corner Brook", "Charlottetown",
    "Summerside", "Halifax", "Kentville", "Truro", "New Glasgow", "Cape Breton", "Moncton",
    "Saint John", "Fredericton", "Bathurst", "Miramichi",
    "Campbellton (New Brunswick part / partie du Nouveau-Brunswick)", "Edmundston",
    "Campbellton (partie du Québec / Quebec part)", "Matane", "Rimouski", "Rivière-du-Loup",
    "Baie-Comeau", "Saguenay", "Alma", "Dolbeau-Mistassini", "Sept-Îles", "Québec",
    "Sainte-Marie", "Saint-Georges", "Thetford Mines", "Sherbrooke", "Cowansville",
    "Victoriaville", "Trois-Rivières", "Shawinigan", "Drummondville", "Sorel-Tracy",
    "Joliette", "Montréal", "Salaberry-de-Valleyfield", "Sainte-Agathe-des-Monts",
    "Lachute", "Val-d'Or", "Amos", "Rouyn-Noranda",
    "Hawkesbury (partie du Québec / Quebec part)",
    "Ottawa - Gatineau (partie du Québec / Quebec part)", "Campbellton", "Cornwall",
    "Hawkesbury (Ontario part / partie de l'Ontario)",
    "Ottawa - Gatineau (Ontario part / partie de l'Ontario)", "Brockville", "Pembroke",
    "Petawawa", "Kingston", "Belleville - Quinte West", "Cobourg", "Port Hope",
    "Peterborough", "Kawartha Lakes", "Centre Wellington", "Oshawa", "Ingersoll", "Toronto",
    "Hamilton", "St. Catharines - Niagara", "Kitchener - Cambridge - Waterloo", "Brantford",
    "Woodstock", "Tillsonburg", "Norfolk", "Guelph", "Stratford", "London", "Chatham-Kent",
    "Windsor", "Essa", "Wasaga Beach", "Owen Sound", "Collingwood", "Barrie", "Orillia",
    "Midland", "Greater Sudbury / Grand Sudbury", "Elliot Lake", "Timmins", "Thunder Bay",
    "Kenora", "Winnipeg", "Winkler", "Steinbach", "Portage la Prairie", "Brandon",
    "Thompson", "Regina", "Yorkton", "Moose Jaw", "Swift Current", "Saskatoon",
    "North Battleford", "Prince Albert", "Estevan", "Weyburn",
    "Lloydminster (Saskatchewan part / partie de la Saskatchewan)", "Brooks", "Lethbridge",
    "Okotoks", "High River", "Calgary", "Strathmore", "Canmore", "Red Deer", "Sylvan Lake",
    "Lacombe", "Camrose", "Edmonton", "Lloydminster (Alberta part / partie de l'Alberta)",
    "Wetaskiwin", "Hawkesbury", "Ottawa - Gatineau", "Cranbrook", "Nelson", "Trail",
    "Penticton", "Kelowna", "Vernon", "Salmon Arm", "Kamloops", "Chilliwack",
    "Abbotsford - Mission", "Vancouver", "Squamish", "Victoria", "Ladysmith", "Duncan",
    "Nanaimo", "Parksville", "Port Alberni", "Courtenay", "Campbell River", "Powell River",
    "Williams Lake", "Quesnel", "Prince Rupert", "Terrace", "Dawson Creek", "Fort St. John",
    "Whitehorse", "Yellowknife", "Lloydminster"),
  status = c(
    "B", "D", "D", "D", "D", "D", "B", "D", "D", "D", "D", "B", "B", "B", "D", "D", "D",
    "D", "D", "D", "D", "D", "D", "B", "D", "D", "D", "B", "D", "D", "D", "B", "D", "D",
    "B", "D", "B", "D", "D", "B", "D", "D", "D", "D", "D", "D", "D", "B", "D", "D", "D",
    "B", "D", "D", "D", "B", "B", "D", "D", "B", "D", "D", "B", "D", "B", "B", "B", "B",
    "B", "D", "D", "D", "B", "D", "B", "D", "B", "D", "D", "D", "D", "B", "D", "D", "B",
    "D", "D", "B", "D", "B", "D", "D", "D", "D", "D", "B", "D", "D", "D", "B", "D", "D",
    "D", "D", "D", "D", "B", "D", "D", "B", "D", "D", "B", "D", "D", "D", "B", "D", "D",
    "D", "B", "D", "D", "D", "D", "B", "D", "D", "B", "B", "B", "B", "D", "B", "D", "D",
    "B", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D"),
  stringsAsFactors = FALSE
)
