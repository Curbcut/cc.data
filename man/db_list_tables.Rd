% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mysql.R
\name{db_list_tables}
\alias{db_list_tables}
\title{List all tables in the MySQL database}
\usage{
db_list_tables()
}
\value{
A character vector of table names.
}
\description{
Connects to the database, lists all tables in it, and then disconnects.
}
