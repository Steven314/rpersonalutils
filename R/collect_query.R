#' Collect Query from SQL File
#'
#' From a SQL file, query the database and return the results in a tibble.
#'
#' The SQL must not end with a semicolon. Whitespace and comments are allowed.
#'
#' @param conn A database connection.
#' @param path File path to a SQL file.
#'
#' @returns A tibble containing the results of the query.
#' @importFrom readr read_file
#' @importFrom dplyr sql collect
#' @importFrom janitor clean_names
#' @export
collect_query <- function(conn, path) {
    dplyr::tbl(
        conn,
        path |>
            readr::read_file() |>
            dplyr::sql()
    ) |>
        dplyr::collect() |>
        janitor::clean_names()
}
