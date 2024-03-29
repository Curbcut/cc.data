% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs.R
\name{gtfs_combine}
\alias{gtfs_combine}
\title{Combine gtfs zips to a unique zip}
\usage{
gtfs_combine(dest_folder = gtfs_download())
}
\arguments{
\item{dest_folder}{Path to the directory where the gtfs zip files are saved,
and where the final combined gtfs zip file should be. If not provided,
the gtfs zip files are downloaded using \code{\link[cc.data]{gtfs_download}}}
}
\value{
Path to the zip file containing the merged GTFS data.
}
\description{
Reads in all GTFS zip files in \code{dest_folder}, merges them, and writes the
resulting merged GTFS data to a new zip file in dest_folder.
}
