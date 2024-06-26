% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mysql.R
\name{db_write_ttm}
\alias{db_write_ttm}
\title{Write a Travel Time Matrix to Database}
\usage{
db_write_ttm(mode, zip_file_path, overwrite)
}
\arguments{
\item{mode}{<\code{character}> The mode of transport for the TTM. Valid values are
"foot", "bicycle", "car", or "transit".}

\item{zip_file_path}{<\code{character}> The file path to the zip file containing the TTM data.}

\item{overwrite}{<\code{logical}> If \code{TRUE}, existing data for the specified mode will
be overwritten. If \code{FALSE}, the function will append new data without deleting existing entries.}
}
\value{
Invisible. The function is called for its side effect of writing data to a database.
}
\description{
This function writes a travel time matrix (TTM) from a specified mode of transport
and zip file to a database, with the option to overwrite existing data.
Supported modes are "foot", "bicycle", "car", or "transit". The function first checks
if the table for the specified mode exists and, based on the \code{overwrite} parameter,
either updates the table or prompts the user to confirm overwriting.
}
\details{
The function performs several steps: it first lists and filters files within the zip
archive, connects to a database, and then based on the \code{overwrite} parameter and user confirmation,
decides whether to recreate the table for the TTM data. It processes the data in batches for efficiency
and uses a retry mechanism for database operations to handle potential transient errors.
}
