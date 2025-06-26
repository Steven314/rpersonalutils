#' Write a Table to a DuckDB Instance
#'
#' Add or replace a table in a DuckDB instance with the ability to add a comment for a description of the table.
#' This wraps [`duckdb::dbWriteTable`](https://r.duckdb.org/reference/duckdb_connection-class.html) and adds the comment via [`dbExecute`](https://dbi.r-dbi.org/reference/dbExecute.html).
#'
#' @param con DuckDB connection. Default is the in-memory database.
#' @param table The table to add to the database. A [`data.frame`] (or coercible to data.frame).
#' @param table_name A string to be the name of the table in the database.
#' @param comment A string or [`paste`]d vector of strings to be the comment on the table.
#' @param overwrite Whether to overwrite the table in the database if it already exists.
#' @param ... Other parameters passed on to [`duckdb::dbWriteTable`](https://r.duckdb.org/reference/duckdb_connection-class.html) or [`DBI::dbWriteTable`](https://dbi.r-dbi.org/reference/dbWriteTable.html).
#' @param quiet By default this will output confirmation of success. Set this argument to `TRUE` to silence those messages.
#'
#' @returns Invisibly returns `table`.
#' @export
write_duckdb <- function(
    con,
    table,
    table_name,
    comment = NULL,
    overwrite = TRUE,
    ...,
    quiet = FALSE
) {
    requireNamespace("duckdb", quietly = TRUE)

    # ensure that the database connect allows writing
    if (con@driver@read_only) {
        stop("The connection is read-only.")
    }

    # prepare the comment statement
    if (!is.null(comment)) {
        statement <- paste0(
            "COMMENT ON TABLE ",
            table_name,
            " IS ",
            "'",
            comment,
            "'"
        )
    }

    # alert the user if the table already exists.
    exists <- duckdb::dbExistsTable(conn = con, name = table_name)

    if (exists & overwrite & !quiet) {
        message(paste("Table", table_name, "already exists. Overwriting..."))
    }

    if (exists & !overwrite & !quiet) {
        message(paste("Table", table_name, "already exists."))
    }

    tryCatch(
        expr = {
            duckdb::dbWriteTable(
                conn = con,
                name = table_name,
                value = table,
                overwrite = overwrite,
                ...
            )

            if (!quiet) {
                message(paste("Table", table_name, "added to the database."))
            }
        },
        error = \(e) e
    )

    if (!is.null(comment)) {
        tryCatch(
            expr = {
                DBI::dbExecute(
                    conn = con,
                    statement = statement
                )

                if (!quiet) {
                    message(paste("Comment on ", table_name, "added."))
                }
            },
            error = \(e) e
        )
    }

    invisible(table)
}
