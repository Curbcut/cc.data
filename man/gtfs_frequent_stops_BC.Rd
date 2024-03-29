% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BC_SSMUH.R, R/BC_gtfs.R
\name{gtfs_frequent_stops_BC}
\alias{gtfs_frequent_stops_BC}
\title{Identify High-Frequency GTFS Stops in British Columbia}
\usage{
gtfs_frequent_stops_BC(DA_table)

gtfs_frequent_stops_BC(DA_table)
}
\value{
An sf object of stops in British Columbia that are serviced
every 15 minutes on average during the specified time windows on
weekdays, Saturdays, and Sundays. The sf object includes stop coordinates
in longitude and latitude.

An sf object of stops in British Columbia that are serviced
every 15 minutes on average during the specified time windows on
weekdays, Saturdays, and Sundays. The sf object includes stop coordinates
in longitude and latitude.
}
\description{
This function downloads the GTFS (General Transit Feed Specification) data
for British Columbia, combines it, and identifies stops that are serviced
frequently (every 15 minutes on average) during specific time windows on
weekdays, Saturdays, and Sundays. This will be used for the new
small-scale, multi-unit housing policy.

This function downloads the GTFS (General Transit Feed Specification) data
for British Columbia, combines it, and identifies stops that are serviced
frequently (every 15 minutes on average) during specific time windows on
weekdays, Saturdays, and Sundays. This will be used for the new
small-scale, multi-unit housing policy.
}
\details{
The function first downloads the GTFS data specific to British
Columbia using \code{gtfs_download_BC} and combines it with \code{gtfs_combine}.
The GTFS data is then read and processed. The \code{stop_times} are ordered
chronologically. Arrival and departure times are converted to seconds.
The function \code{is_high_freq_stop} checks if a stop is serviced at least
48 times (weekdays) or 32 times (weekends) between specified time ranges,
indicating high frequency (every 15 minutes). The function iterates over each stop and route
to assess the frequency. Finally, stops meeting the high-frequency criteria
across all specified time ranges are returned as an \code{sf} object with
coordinates in the CRS format (EPSG:4326).

The function first downloads the GTFS data specific to British
Columbia using \code{gtfs_download_BC} and combines it with \code{gtfs_combine}.
The GTFS data is then read and processed. The \code{stop_times} are ordered
chronologically. Arrival and departure times are converted to seconds.
The function \code{is_high_freq_stop} checks if a stop is serviced at least
48 times (weekdays) or 32 times (weekends) between specified time ranges,
indicating high frequency (every 15 minutes). The function iterates over each stop and route
to assess the frequency. Finally, stops meeting the high-frequency criteria
across all specified time ranges are returned as an \code{sf} object with
coordinates in the CRS format (EPSG:4326).
}
