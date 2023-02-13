#' Reverse geocode for a point in Quebec
#'
#' @param point_sf < `sfc_POINT`> Point sf, e.g. a centroid of a building
#'
#' @return The address retrieved from the Adresses Québec service
#' @export
rev_geocode_Quebec <- function(point_sf) {

  if (sf::st_crs(point_sf)$input != "EPSG:32198")
    point_sf <- sf::st_transform(point_sf, crs = 32198)

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://servicescarto.mern.gouv.qc.ca/pes/rest/services/Territoi",
      "re/AdressesQuebec_Geocodage/GeocodeServer/reverseGeocode?location=",
      x, "%2C+", y, "&distance=&langCode=&outSR=4326&returnIntersection=false&f",
      "=pjson"
    )

  out <- tryCatch(jsonlite::fromJSON(httr::content(httr::GET(link))),
                  error = function(e) NULL)

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

  if (sf::st_crs(point_sf)$input != "EPSG:4326")
    point_sf <- sf::st_transform(point_sf, crs = 4326)

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <-
    paste0(
      "https://geocoder.api.gov.bc.ca/sites/nearest.json?point=",
      x, ",", y
    )

  out <- tryCatch(httr::content(httr::GET(link)),
                  error = function(e) NULL)

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

  if (sf::st_crs(point_sf)$input != "EPSG:4326")
    point_sf <- sf::st_transform(point_sf, crs = 4326)

  coords <- sf::st_coordinates(point_sf)

  x <- coords[1]
  y <- coords[2]

  link <- paste0("photon.komoot.io/reverse?lon=", x, "&lat=", y)

  out <- tryCatch(httr::content(httr::GET(link)),
    error = function(e) NULL)

  if (is.null(out$features) || length(out$features) == 0) {
    return(NA_character_)
  }

  out <- out$features[[1]]$properties

  # Third level of the address, after street number and street name
  third <-
    out |> (\(out) {
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
    out |> (\(out) {
      if (!is.null(out$street)) return(paste(out$street, third, sep = ", "))
      if (!is.null(out$name)) return(paste(out$name, third, sep = ", "))
      return(third)
    })()

  name <-
    out |> (\(out) {
      if (is.null(out$housenumber)) {
        return(second)
      }
      return(paste(out$housenumber, second, sep = " "))
    })()

  if (is.null(name)) return(NA_character_)

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

  link <- paste0("http://localhost:8080/reverse?lon=", x, "&lat=", y, "&format=json")

  out <- tryCatch(httr::content(httr::GET(link)),
                  error = function(e) NULL)

  if (is.null(out$address) || !is.null(out$error)) {
    return(NA_character_)
  }

  out <- out$address

  # Third level of the address, after street number and street name
  third <-
    out |> (\(out) {
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
    out |> (\(out) {
      if (is.null(out$road)) {
        return(third)
      }
      return(paste(out$road, third, sep = ", "))
    })()

  name <-
    out |> (\(out) {
      if (is.null(out$house_number)) {
        return(second)
      }
      return(paste(out$house_number, second, sep = " "))
    })()

  if (is.null(name)) return(NA_character_)

  if (street) name <- gsub(",.*", "", name)

  return(name)
}

#' Geocode using a local Nominatim instance (docker)
#'
#' @param address < `character`> Character vector corresponding to an address,
#' e.g. 6540, Avenue Baldwin, Anjou, Montreal
#'
#' @return `sf` point
#' @export
geocode_localhost <- function(address) {

  # Convert to ASCII
  reworked <- paste0("%", charToRaw(address), collapse = "")

  link <- paste0("http://localhost:8080/search?q=", reworked)

  out <- tryCatch(httr::content(httr::GET(link)),
                  error = function(e) NULL)

  if (is.null(out) || length(out) == 0) return(sf::st_point())
  return(
    sf::st_point(x = c(as.numeric(out[[1]]$lon), as.numeric(out[[1]]$lat))))

}

