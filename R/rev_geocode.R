#' Reverse geocode for a point in Quebec
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the Adresses Québec service
#' @export
rev_geocode_Quebec <- function(point_sf) {
  if (sf::st_crs(point_sf)$input != "EPSG:32198") {
    point_sf <- sf::st_transform(point_sf, crs = 32198)
  }

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://servicescarto.mern.gouv.qc.ca/pes/rest/services/Territoi",
      "re/AdressesQuebec_Geocodage/GeocodeServer/reverseGeocode?location=",
      x,
      "%2C+",
      y,
      "&distance=&langCode=&outSR=4326&returnIntersection=false&f",
      "=pjson"
    )

  out <- tryCatch(
    jsonlite::fromJSON(httr::content(httr::GET(link))),
    error = function(e) NULL
  )

  if (!is.null(out$error)) {
    return(NA_character_)
  }
  return(paste(out$address$Street, out$address$City, sep = ", "))
}

#' Reverse geocode for points in British Columbia
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the BC Geocoder service
#' @export
rev_geocode_BritishColumbia <- function(point_sf) {
  if (sf::st_crs(point_sf)$input != "EPSG:4326") {
    point_sf <- sf::st_transform(point_sf, crs = 4326)
  }

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://geocoder.api.gov.bc.ca/sites/nearest.json?point=",
      x,
      ",",
      y
    )

  out <- tryCatch(httr::content(httr::GET(link)), error = function(e) NULL)

  if (is.null(out$properties$fullAddress)) {
    return(NA_character_)
  }
  return(gsub(", BC$", "", out$properties$fullAddress))
}

#' Reverse geocode using OSM
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the OSM service
#' @export
rev_geocode_OSM <- function(point_sf) {
  if (sf::st_crs(point_sf)$input != "EPSG:4326") {
    point_sf <- sf::st_transform(point_sf, crs = 4326)
  }

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <- paste0("photon.komoot.io/reverse?lon=", x, "&lat=", y)

  out <- tryCatch(httr::content(httr::GET(link)), error = function(e) NULL)

  if (is.null(out$features) || length(out$features) == 0) {
    return(NA_character_)
  }

  out <- out$features[[1]]$properties

  # Third level of the address, after street number and street name
  third <-
    out |>
    (\(out) {
      if (!is.null(out$city)) {
        return(gsub(" \\(\\d{2}\\)$", "", out$city))
      }
      if (!is.null(out$locality)) {
        return(out$locality)
      }
      if (!is.null(out$district)) {
        return(out$district)
      }
      if (!is.null(out$town)) {
        return(out$town)
      }
      if (!is.null(out$village)) {
        return(out$village)
      }
      if (!is.null(out$suburb)) {
        return(out$suburb)
      }
      if (!is.null(out$region)) {
        return(out$region)
      }
      if (!is.null(out$county)) {
        return(out$county)
      }
    })()

  second <-
    out |>
    (\(out) {
      if (!is.null(out$street)) {
        return(paste(out$street, third, sep = ", "))
      }
      if (!is.null(out$name)) {
        return(paste(out$name, third, sep = ", "))
      }
      return(third)
    })()

  name <-
    out |>
    (\(out) {
      if (is.null(out$housenumber)) {
        return(second)
      }
      return(paste(out$housenumber, second, sep = " "))
    })()

  if (is.null(name)) {
    return(NA_character_)
  }

  return(name)
}

#' Reverse geocode using a local Nominatim instance (docker)
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#' @param street < `logical`> Is it to reverse geocode streets (No city name
#' appended)
#'
#' @return The address retrieved from the local Nominatim instance
#' @export
rev_geocode_localhost <- function(point_sf, street = FALSE) {
  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <- paste0(
    "http://localhost:8080/reverse?lon=",
    x,
    "&lat=",
    y,
    "&format=json"
  )

  out <- tryCatch(httr::content(httr::GET(link)), error = function(e) NULL)

  if (is.null(out$address) || !is.null(out$error)) {
    return(NA_character_)
  }

  out <- out$address

  # Third level of the address, after street number and street name
  third <-
    out |>
    (\(out) {
      if (!is.null(out$city)) {
        return(gsub(" \\(\\d{2}\\)$", "", out$city))
      }
      if (!is.null(out$locality)) {
        return(out$locality)
      }
      if (!is.null(out$city_district)) {
        return(out$city_district)
      }
      if (!is.null(out$town)) {
        return(out$town)
      }
      if (!is.null(out$village)) {
        return(out$village)
      }
      if (!is.null(out$suburb)) {
        return(out$suburb)
      }
      if (!is.null(out$region)) {
        return(out$region)
      }
      if (!is.null(out$county)) {
        return(out$county)
      }
    })()

  second <-
    out |>
    (\(out) {
      if (is.null(out$road)) {
        return(third)
      }
      return(paste(out$road, third, sep = ", "))
    })()

  name <-
    out |>
    (\(out) {
      if (is.null(out$house_number)) {
        return(second)
      }
      return(paste(out$house_number, second, sep = " "))
    })()

  if (is.null(name)) {
    return(NA_character_)
  }

  if (street) {
    name <- gsub(",.*", "", name)
  }

  return(name)
}

#' Geocode using a local Nominatim instance (docker)
#'
#' @param address < `character`> Character vector corresponding to an address,
#' e.g. 6540, Avenue Baldwin, Anjou, Montreal
#' @param expand_french_directions < `logical`> Whether to expand French cardinal
#' directions (E, O, N, S, NE, NO, SE, SO) to their full form (Est, Ouest, Nord,
#' Sud, Nord-Est, Nord-Ouest, Sud-Est, Sud-Ouest) to improve geocoding results.
#' @param strip_noise < `logical`> Whether to strip common address noise before
#' geocoding: parenthetical descriptors (e.g. `"(sous-sol)"`), slash-separated
#' alternate street names (e.g. `"rue Meunier/7e avenue"`), and leading venue
#' or building names preceding the civic number.
#' @param strip_unit < `logical`> Whether to strip unit/apartment tokens from the
#' address before geocoding.
#' @param strip_leading_unit < `logical`> Whether to strip a leading unit prefix
#' in the format `{unit}-{civic}` (e.g. `"513-3300, rue des Châteaux"` becomes
#' `"3300, rue des Châteaux"`). Common in Quebec addresses.
#' @param photon_fallback < `logical`> Whether to fall back to the Photon geocoder
#' (photon.komoot.io) when Nominatim returns an empty result. Photon uses
#' Elasticsearch and supports fuzzy matching, useful for misspelled street names.
#'
#' @return `sf` point
#' @export
geocode_localhost <- function(
  address,
  expand_french_directions = TRUE,
  strip_noise = TRUE,
  strip_unit = TRUE,
  strip_leading_unit = TRUE,
  photon_fallback = TRUE
) {
  if (expand_french_directions) {
    abbrevs <- c(
      "\\bE\\.?(?=\\s*,|\\s*$)" = "Est",
      "\\bO\\.?(?=\\s*,|\\s*$)" = "Ouest",
      "\\bN\\.?(?=\\s*,|\\s*$)" = "Nord",
      "\\bS\\.?(?=\\s*,|\\s*$)" = "Sud",
      "\\bNE\\.?(?=\\s*,|\\s*$)" = "Nord-Est",
      "\\bNO\\.?(?=\\s*,|\\s*$)" = "Nord-Ouest",
      "\\bSE\\.?(?=\\s*,|\\s*$)" = "Sud-Est",
      "\\bSO\\.?(?=\\s*,|\\s*$)" = "Sud-Ouest"
    )
    address <- stringr::str_replace_all(address, abbrevs)
  }
  if (strip_noise) {
    # 1. Parentheticals
    address <- stringr::str_remove_all(address, "\\([^)]*\\)")
    # 2. Slash alternate street names
    address <- stringr::str_remove_all(address, "/[^,]+")
    # 3. Leading venue name (everything before first digit)
    address <- stringr::str_remove(address, "^\\D+(?=\\d)")
    # 4. Cleanup
    address <- stringr::str_replace_all(address, ",\\s*,+", ",") |>
      stringr::str_trim()
  }
  if (strip_unit) {
    address <- stringr::str_remove_all(
      address,
      stringr::regex(
        ",?\\s*\\b(appt?\\.?|app\\.?|suite\\.?|poste\\.?|unit\\.?|unité\\.?|bureau\\.?|local\\.?|salle\\.?|porte\\.?|étage\\.?|etage\\.?|#)\\s*[\\w-]+",
        ignore_case = TRUE
      )
    )
    address <- stringr::str_replace_all(address, ",\\s*,", ",") |>
      stringr::str_trim()
  }
  if (strip_leading_unit) {
    address <- stringr::str_remove(address, "^\\d+-(?=\\d)")
  }

  reworked <- paste0("%", charToRaw(address), collapse = "")
  link <- paste0("http://localhost:8080/search?q=", reworked)
  out <- tryCatch(httr::content(httr::GET(link)), error = function(e) NULL)

  if (!is.null(out) && length(out) > 0) {
    return(sf::st_point(
      x = c(as.numeric(out[[1]]$lon), as.numeric(out[[1]]$lat))
    ))
  }

  if (photon_fallback) {
    photon_link <- paste0(
      "https://photon.komoot.io/api/?q=",
      utils::URLencode(address, repeated = TRUE),
      "&limit=1"
    )
    photon_out <- tryCatch(
      httr::content(httr::GET(photon_link)),
      error = function(e) NULL
    )
    if (!is.null(photon_out) && length(photon_out$features) > 0) {
      coords <- photon_out$features[[1]]$geometry$coordinates
      return(sf::st_point(
        x = c(as.numeric(coords[[1]]), as.numeric(coords[[2]]))
      ))
    }
  }

  return(sf::st_point())
}
