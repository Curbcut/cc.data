% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foursquare_poi.R
\name{foursquare_get_canada}
\alias{foursquare_get_canada}
\title{Get All Foursquare POIs in Canada}
\usage{
foursquare_get_canada(conn)
}
\arguments{
\item{conn}{A database connection object from \code{foursquare_connect}.}
}
\value{
A data frame containing POI attributes including id, name, category, coordinates, location, and dates.
}
\description{
Retrieves all Points of Interest (POIs) from the Foursquare open source database for Canada.
}
