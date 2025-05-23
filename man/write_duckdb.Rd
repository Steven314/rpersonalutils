% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_duckdb.R
\name{write_duckdb}
\alias{write_duckdb}
\title{Write a Table to a DuckDB Instance}
\usage{
write_duckdb(
  con,
  table,
  table_name,
  comment = NULL,
  overwrite = TRUE,
  ...,
  quiet = FALSE
)
}
\arguments{
\item{con}{DuckDB connection. Default is the in-memory database.}

\item{table}{The table to add to the database. A \code{\link{data.frame}} (or coercible to data.frame).}

\item{table_name}{A string to be the name of the table in the database.}

\item{comment}{A string or \code{\link{paste}}d vector of strings to be the comment on the table.}

\item{overwrite}{Whether to overwrite the table in the database if it already exists.}

\item{...}{Other parameters passed on to \href{https://r.duckdb.org/reference/duckdb_connection-class.html}{\code{duckdb::dbWriteTable}} or \href{https://dbi.r-dbi.org/reference/dbWriteTable.html}{\code{DBI::dbWriteTable}}.}

\item{quiet}{By default this will output confirmation of success. Set this argument to \code{TRUE} to silence those messages.}
}
\value{
Invisibly returns \code{table}.
}
\description{
Add or replace a table in a DuckDB instance with the ability to add a comment for a description of the table.
This wraps \href{https://r.duckdb.org/reference/duckdb_connection-class.html}{\code{duckdb::dbWriteTable}} and adds the comment via \href{https://dbi.r-dbi.org/reference/dbExecute.html}{\code{dbExecute}}.
}
