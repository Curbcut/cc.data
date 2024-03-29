% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_process.R
\name{read_and_process_data_canue}
\alias{read_and_process_data_canue}
\title{Read and Process Spatial Data}
\usage{
read_and_process_data_canue(
  DA_table,
  bucket = "curbcut.rawdata",
  bucket_object,
  objectext = ".csv",
  col_val,
  value_name
)
}
\arguments{
\item{DA_table}{<\verb{sf data.frame}> A \code{DA} sf data.frame}

\item{bucket}{<\code{character}> The bucket from which to grab the data. Defaults
to \code{"curbcut.rawdata"}.}

\item{bucket_object}{<\code{character}> The object in the bucket to grab and read
data values from. e.g. \code{"no2_2016.csv"}.}

\item{objectext}{<\code{character}> Extension of the bucket_object. Defaults to \code{".csv"}.}

\item{col_val}{<\code{character}> The name of the column which holds the data in the
bucket_object.}

\item{value_name}{<\code{character}> The name of the column of the output value.
e.g. \code{"NO2"}.}
}
\value{
A tibble containing the processed data, including the aggregated values.
}
\description{
This function reads and processes spatial data related to postal codes and specific
data values, mostly data coming from CANUE. It performs a series of transformations
and aggregations, resulting in a tibble with the average values grouped by DA ID.
}
