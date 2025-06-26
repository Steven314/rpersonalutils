#' Oracle Database Connection
#'
#' Connect to an Oracle database. Provide the username and password through
#' secret means.
#'
#' Installing the `ROracle` package can be complicated. It requires the OCI
#' library. Instructions can be found on the Oracle website and require an
#' Oracle account. The [version on
#' CRAN](https://cran.r-project.org/package=ROracle) is outdated, see
#' [Oracle](https://www.oracle.com/database/technologies/roracle-downloads.html)
#' for the most recent release.
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
        dbname = dbname
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
#' @importFrom rlang is_logical
#' @export
duck_con <- function(dbdir, read_only = FALSE) {
    requireNamespace("duckdb", quietly = TRUE)

    stopifnot(rlang::is_logical(read_only))

    DBI::dbConnect(
        drv = duckdb::duckdb(),
        dbdir = dbdir,
        read_only = read_only
    )
}
