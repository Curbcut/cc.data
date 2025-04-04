% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foursquare_poi.R
\name{poi_run_evolution}
\alias{poi_run_evolution}
\title{Process POI Evolution for Multiple Geographies}
\usage{
poi_run_evolution(geo_list)
}
\arguments{
\item{geo_list}{A named list of geographic \code{sf} objects (e.g., list(pr = pr_sf, cma = cma_sf)).
Each object must include a column named \code{ID} for regional grouping.}
}
\value{
A named list of data frames, one per geography, each in wide format
with cumulative POI totals per year and per region (ID).
}
\description{
Connects to the Foursquare database, retrieves POIs for Canada,
and processes each geographic layer to calculate yearly cumulative POI counts.
}
