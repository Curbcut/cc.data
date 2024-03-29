% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rev_geocode_sf.R
\name{rev_geocode_building}
\alias{rev_geocode_building}
\title{Reverse geocode spatial features}
\usage{
rev_geocode_building(prov_folder, nb_batches = 100, nb_sf_process = 400)
}
\arguments{
\item{prov_folder}{<\code{character}> Where the sf data.frames with spatial features
for each province live (\code{.qs} files).}

\item{nb_batches}{<\code{numeric}> In how many batches should the sf that
are to be reverse geocoded through the local instance of Nominatim be divided?
Useful to have progress bar information.}

\item{nb_sf_process}{<\code{numeric}> Once the sf dataframe has been split up
in x number of batches, how many spatial features should be in each of the
apply iterations? By default, each data.frame going to each process will
be roughly 400 rows. Smaller dataframes help to not overload memory (every apply
iteration only has to hold a 400 sf data.frame before getting flushed and
restart with another data.frame). There must be a compromise between 1. not
too heavy on memory and 2. limit overhead of each iteration.}
}
\value{
An \code{sf} data.frame combining all the \code{.qs} files in \code{prov_folder} with
all spatial features reverse geocoded.
}
\description{
This geocoding works in two steps. The province database of addresses is
first downloaded from the National Open Database of Addresses, with URLs to
download the zip files available in \code{\link[cc.data]{buildings_osm_ms_keys}}.
Second, we reverse geocode using the OSM service. The reverse geocoding is done
using the centroid of every spatial feature.
}
