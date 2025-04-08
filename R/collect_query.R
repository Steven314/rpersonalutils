#' Collect Query from SQL File
#'
#' From a SQL file, query the database and return the results in a tibble.
#'
#' The SQL must not contain any comments and must not end with a semicolon.
#'
#' @param conn A database connection.
#' @param path File path to a SQL file.
#'
#' @returns A tibble containing the results of the query.
#' @importFrom readr read_file
#' @importFrom stringr str_squish
#' @importFrom dplyr sql collect
#' @importFrom janitor clean_names
#' @export
collect_query <- function(conn, path) {
    dplyr::tbl(
        conn,
        path |>
            readr::read_file() |>
            stringr::str_squish() |>
            dplyr::sql()
    ) |>
        dplyr::collect() |>
        janitor::clean_names()
}
