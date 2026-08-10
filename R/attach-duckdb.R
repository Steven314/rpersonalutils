#' Attach an Additional DuckDB File to Connection
#'
#' @param con DuckDB connection.
#' @param path Path of the new DuckDB file.
#' @param name Name to use for the new database. Defaults the name of the file.
#' @param read_only Read only. Defaults to false.
#'
#' @returns A single value vector with a value of zero if successful.
#'
#' @export
attach_duckdb <- function(
    con,
    path,
    name = sub('\\..*$', '', basename(path)),
    read_only = FALSE
) {
    requireNamespace("DBI", quietly = TRUE)

    DBI::dbExecute(
        con,
        paste(
            "ATTACH IF NOT EXISTS",
            paste0("'", path, "'"),
            "as",
            name,
            ifelse(read_only, "(READ_ONLY)", "")
        )
    )
}
