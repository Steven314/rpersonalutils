#' Oracle Database Connection
#'
#' Connect to an Oracle database.
#' Provide the username and password through secret means.
#'
#' @param user Oracle username.
#' @param pass Oracle password.
#' @param dbname Database name.
#'
#' @returns An ROracle database connection (`OraConnection`).
#' @export
oracle_con <- function(user, pass, dbname) {
    requireNamespace("DBI", quietly = TRUE)
    requireNamespace("ROracle", quietly = TRUE)

    DBI::dbConnect(
        ROracle::Oracle(),
        username = user,
        password = pass,
        dbname   = dbname
    )
}

#' DuckDB Connection
#'
#' Connect to a DuckDB file.
#'
#' @param dbdir Location for database files. Should be a path to an existing
#'   directory in the file system. With the default (or ""), all data is kept in
#'   RAM.
#' @param read_only Set to TRUE for read-only operation. For file-based
#'   databases, this is only applied when the database file is opened for the
#'   first time. Subsequent connections (via the same drv object or a drv object
#'   pointing to the same path) will silently ignore this flag.
#'
#' @returns A DuckDB connection object ([`duckdb::duckdb_driver`]).
#' @importFrom rlang arg_match
#' @export
duck_con <- function(dbdir, read_only = FALSE) {
    requireNamespace("duckdb", quietly = TRUE)

    read_only <- rlang::arg_match(read_only, c(TRUE, FALSE))

    DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = dbdir,
        read_only = read_only
    )
}
